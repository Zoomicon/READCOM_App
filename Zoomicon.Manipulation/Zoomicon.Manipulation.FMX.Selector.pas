unit Zoomicon.Manipulation.FMX.Selector;

interface
  uses
    System.Classes, //for TComponent
    System.Types, //for TPointF
    FMX.Objects, //for TSelection
    FMX.Controls; //for TControlList

type
  TRectFPredicate = reference to function(Rectangle: TRectF): Boolean;

  {$REGION 'TLocationSelector' ------------------------------------------------}

  TMovingEvent = procedure(Sender: TObject; const DX, DY: Single; out Canceled: Boolean) of object;
  TMovedEvent = procedure(Sender: TObject; const DX, DY: Single) of object;

  TLocationSelector = class(TSelectionPoint)
  protected
    FChangeCanceled: boolean;
    FRestorePosition: TPointF;
    FOnParentMoving: TMovingEvent;
    FOnParentMoved: TMovedEvent;

    //procedure DoChangeTracking(var X, Y: Single); override; //unfortunately ancestor method isn't virtual, using OnTrack=HandleChangeTracking in constructor instead
    procedure HandleChangeTracking(Sender: TObject; var X, Y: Single); //called while mouse drag occurs

    //procedure DoChange; override; //unfortunately ancestor method isn't virtual, using OnChange=HandleChange in constructor instead
    procedure HandleChange(Sender: TObject); //called when mouse is released

  public
    constructor Create(AOwner: TComponent); override;

  published
    property OnParentMoving: TMovingEvent read FOnParentMoving write FOnParentMoving;
    property OnParentMoved: TMovedEvent read FOnParentMoved write FOnParentMoved;
    property ChangeCanceled: Boolean read FChangeCanceled stored false;
  end;

  {$ENDREGION .................................................................}

  TAreaSelectionMode = (asmoContained, asmoIntersected); //the default is the 1st one (select only contained items in area selection)

  {$REGION 'TAreaSelector' ----------------------------------------------------}

  TAreaSelector = class(TSelection)
  protected
    FSelectionMode: TAreaSelectionMode;
    FOnlyFromTop: Boolean;

    function DoGetUpdateRect: TRectF; override; //used to fix bug in TSelection that doesn't consider usage inside a TScaledLayout

    function GetSelectedArea: TRectF; virtual;

    function GetControls(const RectPicker: TRectFPredicate): TControlList; virtual;
    function GetContained: TControlList; virtual;
    function GetIntersected: TControlList; virtual;
    function GetSelected: TControlList; virtual;

    function GetControlCount(const RectPicker: TRectFPredicate): Integer; virtual;
    function GetContainedCount: Integer; virtual;
    function GetIntersectedCount: Integer; virtual;
    function GetSelectedCount: Integer; virtual;

  public
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override; //seems "MouseMove" is exposed as public in TSelection (whereas in TControl it's "protected")

  published
    const STR_FORMAT_HINT = '%f x %f';

    property SelectedArea: TRectF read GetSelectedArea;

    property Contained: TControlList read GetContained;
    property Intersected: TControlList read GetIntersected;
    property Selected: TControlList read GetSelected;

    property ContainedCount: Integer read GetContainedCount;
    property IntersectedCount: Integer read GetIntersectedCount;
    property SelectedCount: Integer read GetSelectedCount;

    property SelectionMode: TAreaSelectionMode read FSelectionMode write FSelectionMode default Low(TAreaSelectionMode);
    property OnlyFromTop: Boolean read FOnlyFromTop write FOnlyFromTop;
  end;

  {$ENDREGION .................................................................}

  procedure Register;

implementation
  uses
    Zoomicon.Generics.Functors, //for TF
    Zoomicon.Generics.Collections, //for TListEx
    Zoomicon.Helpers.FMX.Controls.ControlHelper, //for TControl.SubComponent
    FMX.Ani,
    FMX.Effects,
    //FMX.Forms, //for Application
    //FMX.StdActns, //for THintAction
    FMX.Types, //for RegisterFmxClasses
    System.Math, //for Min
    System.SysUtils; //for Format

{$REGION 'TLocationSelector' --------------------------------------------------}

constructor TLocationSelector.Create(AOwner: TComponent);
begin
  inherited;
  OnTrack := HandleChangeTracking;
  OnChange := HandleChange;
end;

procedure TLocationSelector.HandleChangeTracking(Sender: TObject; var X, Y: Single);
begin
  var PressedPos := PressedPosition;
  var ParentSize := ParentControl.Size.Size;
  var DX := X - PressedPos.X/2 - ParentSize.Width/2;
  var DY := Y - PressedPos.X/2 - ParentSize.Height/2;

  if not FChangeCanceled then
    begin
    if Assigned(OnParentMoving) then
      OnParentMoving(Self, DX, DY, FChangeCanceled);

    if FChangeCanceled then //must do this right after calling OnParentMoving
      FRestorePosition := Position.Point - PointF(DX, DY)
    else
      begin
      with ParentControl.Position do
        Point := Point + PointF(DX, DY); //Move parent control

      if Assigned(OnParentMoved) then
        OnParentMoved(Self, DX, DY);
      end;
    end
end;

procedure TLocationSelector.HandleChange(Sender: TObject);
begin
  if FChangeCanceled then
    begin
    Position.Point := FRestorePosition;
    FChangeCanceled := True;
    end;
end;

{$ENDREGION ...................................................................}

{$REGION 'TAreaSelector' ------------------------------------------------------}

function TAreaSelector.DoGetUpdateRect: TRectF; //Fix for TSelection's calculation for the case we have one or more nested TScaledLayout in parent hierarchy
begin
  Result := inherited;
  with AbsoluteScale do //avoids calling AbsoluteScale property getter method twice (for AbsoluteScale.X and AbsoluteScale.Y). Equivalent to using an LAbsoluteScale local variable
    Result.Inflate((GripSize + 1) * X, (GripSize + 1) * Y); //not sure if that is too big, however it works compared to just using Scale as TSelection does
end;

{$region 'Get'}

function TAreaSelector.GetSelectedArea: TRectF;
begin
  var rect := BoundsRect; //TODO: if reusing AreaSelector without reparenting it, should allow setting its "user/target" control to do coordinate conversion (it should provide property to set what control it's using as base and it should provide mapped bounds for that as special property)
  var d := GripSize/1.3;
  rect.Inflate(-d, -d, -d, -d); //Inflating by -SELECTION_GRIP_SIZE * 1.5 to avoid strange issue where dragging from the part of the knob that is over the object does selection update
  result := rect;
end;

function TAreaSelector.GetControls(const RectPicker: TRectFPredicate): TControlList;
begin
  if not Assigned(RectPicker) or (Parent = nil) then exit(nil);

  result := TListEx<TControl>.GetAll(ParentControl.Controls,
    function(AControl: TControl): Boolean
    begin
      result := (AControl <> Self) //not selecting ourselves
                and (not AControl.SubComponent) //not selecting subcomponents //TODO: maybe add property for that, defaulting to true
                and RectPicker(AControl.BoundsRect);
    end
  );

  if OnlyFromTop and (result.Count > 1) then
  begin
    var TopOneSelected := result.Last; //the last one in the Components list is the one on top in the Z-Order
    result.Clear; //clear the previously calculated result, avoid creating new list (would have to free previous one)
    result.Add(TopOneSelected); //return single result in a list
  end;
end;

function TAreaSelector.GetContained: TControlList;
begin
  var TheBounds := BoundsRect;
  result := GetControls(
      function (ARect: TRectF): Boolean
      begin
        result := TheBounds.Contains(ARect);
      end
    );
end;

function TAreaSelector.GetIntersected: TControlList;
begin
  var TheBounds := BoundsRect;
  result := GetControls(
      function (ARect: TRectF): Boolean
      begin
        result := TheBounds.IntersectsWith(ARect);
      end
    );
end;

function TAreaSelector.GetSelected: TControlList;
begin
  case FSelectionMode of
    asmoContained: result := GetContained;
    asmoIntersected: result := GetIntersected;
  else
    result := nil;
  end;
end;

{$endregion}

{$region 'Count'}

function TAreaSelector.GetControlCount(const RectPicker: TRectFPredicate): Integer;
begin
  if not Assigned(RectPicker) or (Parent = nil) then exit(0);

  result := TListEx<TControl>.GetCount(ParentControl.Controls,
    function(AControl: TControl): Boolean
    begin
      result := (AControl <> Self) //not selecting ourselves
                and RectPicker(AControl.BoundsRect);
    end
  );

  if OnlyFromTop and (result > 1) then
    result := 1;
end;

function TAreaSelector.GetContainedCount: Integer;
begin
  var TheBounds := BoundsRect;
  result := GetControlCount(
      function (ARect: TRectF): Boolean
      begin
        result := TheBounds.Contains(ARect);
      end
    );
end;

function TAreaSelector.GetIntersectedCount: Integer;
begin
  var TheBounds := BoundsRect;
  result := GetControlCount(
      function (ARect: TRectF): Boolean
      begin
        result := TheBounds.IntersectsWith(ARect);
      end
    );
end;

function TAreaSelector.GetSelectedCount: Integer;
begin
  case FSelectionMode of
    asmoContained: result := GetContainedCount;
    asmoIntersected: result := GetIntersectedCount;
  else
    result := 0;
  end;
end;

{$endregion}

{$region 'Events'}

procedure TAreaSelector.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;

  Hint := Format(STR_FORMAT_HINT, [Width, Height]); //set area selection size as hint (tooltip) - depends on "ShowHint" property (defaults to true) if it will be displayed

  //TODO: VCL has Application.ActivateHint to force show the hint, but FMX doesn't. There's Application.SetHint private method which uses THintAction, but using similar code doesn't seem to do the job
  //TODO: There's issue with hint not being displayed when user clicks on same-sized object to wrap AreaSelection around it. Seems they have to resize the AreaSelection first or move the mouse outside and back inside it to show the hint. Tried setting the Hint to empty first or to something dummy (and call Application.ProcessMessages in between), but doesn't work either
end;

{$endregion}

{$ENDREGION ...................................................................}

{$REGION 'Registration' -------------------------------------------------------}

procedure RegisterSerializationClasses;
begin
  RegisterFmxClasses([TLocationSelector, TAreaSelector]);
end;

procedure Register;
begin
  GroupDescendentsWith(TLocationSelector, TControl);
  GroupDescendentsWith(TAreaSelector, TControl);
  RegisterSerializationClasses;
  RegisterComponents('Zoomicon', [TLocationSelector, TAreaSelector]);
end;

{$ENDREGION ...................................................................}

initialization
  RegisterSerializationClasses; //don't call Register here, it's called by the IDE automatically on a package installation (fails at runtime)

end.

