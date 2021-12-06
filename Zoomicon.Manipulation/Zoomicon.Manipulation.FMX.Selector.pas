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

  {$REGION 'TAreaSelector' ----------------------------------------------------}

  TAreaSelector = class(TSelection)
  protected
    FOnlyFromTop: Boolean;

    function DoGetUpdateRect: TRectF; override; //used to fix bug in TSelection that doesn't consider usage inside a TScaledLayout
    function GetControls(const RectPicker: TRectFPredicate): TControlList;
    function GetIntersected: TControlList; virtual;
    function GetContained: TControlList; virtual;

  published
    property Intersected: TControlList read GetIntersected stored false;
    property Selected: TControlList read GetContained stored false;
    property OnlyFromTop: Boolean read FOnlyFromTop write FOnlyFromTop;
  end;

  {$ENDREGION .................................................................}

implementation
  uses
    Zoomicon.Generics.Functors, //for TF
    Zoomicon.Generics.Collections, //for TListEx
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

function TAreaSelector.GetControls(const RectPicker: TRectFPredicate): TControlList;
begin
  if not Assigned(RectPicker) then
    result := nil
  else
    begin
    if (Parent = nil) then
      result := nil
    else
      result := TListEx<TControl>.GetAll(ParentControl.Controls,
        function(AControl: TControl): Boolean
        begin
          result := (AControl <> Self) //not selecting ourselves
                    and RectPicker(AControl.BoundsRect);
        end
      );
      if OnlyFromTop and (result.Count > 1) then
        result := TControlList.Create(result.Last);
    end;
end;

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

