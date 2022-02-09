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

    function GetControls(const RectPicker: TRectFPredicate): TControlList;
    function GetContained: TControlList; virtual;
    function GetIntersected: TControlList; virtual;
    function GetSelected: TControlList; virtual;

    function GetControlCount(const RectPicker: TRectFPredicate): Integer;
    function GetContainedCount: Integer; virtual;
    function GetIntersectedCount: Integer; virtual;
    function GetSelectedCount: Integer; virtual;

  published
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
    Zoomicon.Helpers.FMX.Controls.ControlHelpers, //for TControlSubComponentHelper
    FMX.Ani,
    FMX.Effects,
    FMX.Types, //for RegisterFmxClasses
    System.Math; //for Min

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
  Result.Inflate((GripSize + 1) * AbsoluteScale.X, (GripSize + 1) * AbsoluteScale.Y); //not sure if that is too big, however it works compared to just using Scale as TSelection does
end;

{$region 'Get'}

function TAreaSelector.GetControls(const RectPicker: TRectFPredicate): TControlList;
begin
  if not Assigned(RectPicker) or (Parent = nil) then exit(nil);

  result := TListEx<TControl>.GetAll(ParentControl.Controls,
    function(AControl: TControl): Boolean
    begin
      result := (AControl <> Self) //not selecting ourselves
                and (not AControl.SubComponent) //not selecting our subcomponents
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

{$ENDREGION ...................................................................}

procedure RegisterClasses;
begin
  RegisterFmxClasses([TLocationSelector, TAreaSelector]); //register for persistence (in case they're used standalone)
end;

procedure Register;
begin
  GroupDescendentsWith(TLocationSelector, TControl);
  GroupDescendentsWith(TAreaSelector, TControl);
  RegisterClasses;
  RegisterComponents('Zoomicon', [TLocationSelector, TAreaSelector]);
end;

initialization
  RegisterClasses; //don't call Register here, it's called by the IDE automatically on a package installation (fails at runtime)

end.

