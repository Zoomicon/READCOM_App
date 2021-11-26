unit Zoomicon.Manipulator;

interface

uses
  Zoomicon.Selector, //for TLocationSelector, TAreaSelector
  System.Types,
  System.UITypes,
  System.Classes,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Gestures, //TODO: is this needed here?
  FMX.Objects; //for TSelection

const
  SELECTION_GRIP_SIZE = 8;
  DEFAULT_AUTOSIZE = false;

type

  {$REGION 'TManipulator' -----------------------------------------------------}

  TManipulator = class(TFrame)
    {Gestures}
    procedure DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean); override;

    {Mouse}
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;

  protected
    FMouseShift: TShiftState; //TODO: if Delphi fixes the Shift parameter to not be empty at MouseClick in the future, remove this

    {Gestures}
    FLastPosition: TPointF;
    FLastDistance: Integer;

    FAreaSelector: TAreaSelector;
    FAutoSize: Boolean;
    FDragStartLocation: TPointF;

    procedure DoAutoSize;
    procedure SetAutoSize(const Value: Boolean);

    {EditMode}
    function IsEditMode: Boolean; virtual;
    procedure SetEditMode(const Value: Boolean); virtual;

    {Proportional}
    function IsProportional: Boolean; virtual;
    procedure SetProportional(value: Boolean); virtual;

    procedure HandleAreaSelectorMoving(Sender: TObject; const DX, DY: Single; out Canceled: Boolean);
    procedure HandleAreaSelectorMoved(Sender: TObject; const DX, DY: Single);

    procedure HandlePan(EventInfo: TGestureEventInfo);
    procedure HandleRotate(EventInfo: TGestureEventInfo);
    procedure HandleZoom(EventInfo: TGestureEventInfo);
    procedure HandlePressAndTap(EventInfo: TGestureEventInfo);


  public
    constructor Create(AOwner: TComponent); override;
    procedure MoveControls(const Controls: TControlList; const DX, DY: Single); overload;
    procedure RotateControls(const Controls: TControlList; const DAngle: Single); overload;
    procedure MoveControls(const DX, DY: Single); overload;
    procedure RotateControls(const DAngle: Single); overload;
    procedure MoveSelected(const DX, DY: Single);
    procedure RotateSelected(const DAngle: Single);

  published
    property AreaSelector: TAreaSelector read FAreaSelector stored false;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default DEFAULT_AUTOSIZE;
    property EditMode: Boolean read IsEditMode write SetEditMode default false;
    property Proportional: Boolean read IsProportional write SetProportional;

  end;

  {$ENDREGION .................................................................}

procedure Register;

implementation

uses
  Zoomicon.Generics.Functors, //for TF
  Zoomicon.Generics.Collections, //for TListEx
  FMX.Ani,
  FMX.Effects,
  System.Math, //for Min, EnsureInRange
  System.SysUtils; //for Supports

{$R *.fmx}

{$REGION 'TManipulator'}

constructor TManipulator.Create(AOwner: TComponent);

  procedure CreateAreaSelector;
  begin
     FAreaSelector := TAreaSelector.Create(Self);
     With FAreaSelector do
       begin
       Stored := False; //don't store state, should use state from designed .FMX resource
       ParentBounds := true;
       Size.Size := TPointF.Zero;
       Visible := false;
       GripSize := SELECTION_GRIP_SIZE;
       end;
     FAreaSelector.Parent := Self;
  end;

  procedure CreateLocationSelector;
  begin
    var LocationSelector := TLocationSelector.Create(FAreaSelector); //don't use Self so that the middle SelectionPoint doesn't show up in the frame designer
    with LocationSelector do
      begin
      Stored := False; //don't store state, should use state from designed .FMX resource (NOT NEEDED, SINCE WE HAVE IT AS CHILD OF AREASELECTOR WHICH IS SET TO NOT STORED)
      ParentBounds := false; //can move outside of parent area
      GripSize := SELECTION_GRIP_SIZE;
      Align := TAlignLayout.Center;
      Parent := FAreaSelector;
      OnParentMoving := HandleAreaSelectorMoving;
      OnParentMoved := HandleAreaSelectorMoved;
      end;
  end;

begin
  CreateAreaSelector;
  CreateLocationSelector; //must do after CreateAreaSelector
  FAutoSize := DEFAULT_AUTOSIZE; //must do after CreateAreaSelector
  inherited; //do last since it will also load designer resource
end;

{$region 'Manipulation'}

procedure TManipulator.MoveControls(const Controls: TControlList; const DX, DY: Single);
begin
  if (DX <> 0) or (DY <> 0) then
    begin
    BeginUpdate;

    TListEx<TControl>.ForEach(Controls,
      procedure (Control: TControl)
      begin
        with Control.Position do
          Point := Point + PointF(DX, DY);
      end
    );

    DoAutoSize;
    EndUpdate;
    end;
end;

procedure TManipulator.RotateControls(const Controls: TControlList; const DAngle: Single);
begin
  if (DAngle <>0) then
    begin
    BeginUpdate;

    TListEx<TControl>.ForEach(Controls,
      procedure (Control: TControl)
      begin
        with (Control as IRotatedControl) do
          try
            RotationAngle := RotationAngle + DAngle; //seems RotationAngle is protected, but since TControl implements IRotatedControl we can access that property through that interface
          except //catch exceptions, in case any controls fail when you try to rotate them
            //NOP //TODO: do some error logging
          end;
      end
    );

    DoAutoSize; //TODO: is this needed? Do bounds change on rotation?
    EndUpdate;
    end;
end;

procedure TManipulator.MoveControls(const DX, DY: Single);
begin
  MoveControls(Controls, DX, DY);
end;

procedure TManipulator.RotateControls(const DAngle: Single);
begin
  RotateControls(Controls, DAngle);
end;

procedure TManipulator.MoveSelected(const DX, DY: Single);
begin
  if (DX <> 0) or (DY <> 0) then
    begin
    var TheSelected := AreaSelector.Selected;
    MoveControls(TheSelected, DX, DY);
    FreeAndNil(TheSelected);
    end;
end;

procedure TManipulator.RotateSelected(const DAngle: Single);
begin
  if (DAngle <> 0) then
    begin
    var TheSelected := AreaSelector.Selected;
    RotateControls(TheSelected, DAngle);
    FreeAndNil(TheSelected);
    end;
end;

{$endregion}

{$region 'AutoSize'}

procedure TManipulator.SetAutoSize(const Value: Boolean);
begin
  FAutoSize := Value;
  DoAutoSize;
end;

function IsSelection(obj: TFmxObject): Boolean;
begin
  Result := obj is TSelection;
end;

procedure TManipulator.DoAutoSize;
begin
  if (FAutoSize) then
    begin
    BeginUpdate;

    //temporarily disable Align:=Scale setting of all Selections and set it back again when done
    //var theSelections := Selections;
    try
      //SetControlsAlign(TAlignLayout.None);

      var rect := GetChildrenRect;
      SetSize(rect.Width, rect.Height);

      //SetControlsAlign(TAlignLayout.Scale);
    finally
      //FreeAndNil(theSelections);
    end;

    EndUpdate;
    end;
end;

{$endregion}

{$region 'EditMode'}

function TManipulator.IsEditMode: Boolean;
begin
  result := AreaSelector.Visible;
end;

procedure TManipulator.SetEditMode(const Value: Boolean);
begin
  TListEx<TControl>.ForEach(
    Controls,
    procedure(Control: TControl)
    begin
      if not (Control is TAreaSelector) then //no need to use a Predicate<TControl> to select the non-TSelectors, since we can excluse the TSelectorArea here
        Control.Enabled := not Value;
    end
  );

  if Assigned(FAreaSelector) then
    FAreaSelector.Visible := Value; //Show or Hide selection UI (this will also hide the move control point)
end;

{$endregion}

{$region 'Proportional'}

function TManipulator.IsProportional: Boolean;
begin
  result := AreaSelector.Proportional;
end;

procedure TManipulator.SetProportional(Value: boolean);
begin
  //if Value then Align := TAlignLayout.Fit else Align := TAlignLayout.Scale;
  AreaSelector.Proportional := Value;
end;

{$endregion}

{$region 'Events handling'}

procedure TManipulator.HandleAreaSelectorMoving(Sender: TObject; const DX, DY: Single; out Canceled: Boolean);
begin
  BeginUpdate;
  //Move all controls selected by the AreaSelector (that has just been moved)
  MoveSelected(DX, DY); //this will also call DoAutoSize
  Canceled := false;
  //after this, TSelectionPoint.HandleChangeTracking will move the SelectionArea, then fire OnParentMoved event (which is assigned to HandleAreaSelectorMoved of the manipulator)
end;

procedure TManipulator.HandleAreaSelectorMoved(Sender: TObject; const DX, DY: Single);
begin
  //at this point TSelectionPoint.HandleChangeTracking has moved the SelectionArea
  var SelectionPoint := TSelectionPoint(Sender); //assuming events are sent by TSelectionPoint
  with SelectionPoint do
    begin
    var ParentPos := ParentControl.Position.Point;
    var newX := ParentPos.X;
    var newY := ParentPos.Y;

    //Offset all controls (including this one) by the amount this control got into negative coordinates:
    MoveControls(TF.Iff<Single>(newX < 0, newX, 0), TF.Iff<Single>(newY < 0, newY, 0)); //this will also call DoAutoSize //there's also IfThen from System.Math, but those aren't marked as "Inline"

    //PressedPosition := TPointF.Zero;
    Position.Point := TPointF.Zero;
    //Align := TAlignLayout.Center;
    end;
  EndUpdate;
end;

procedure TManipulator.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FMouseShift := Shift; //TODO: remove if Delphi fixes related bug (more at FMouseShift definition) //Note that MouseUp fires before MouseClick so we do need to have this in MouseDown

  inherited; //needed for event handlers to be fired (e.g. at ancestors)

  if not EditMode then exit;

  if (ssLeft in Shift) then
    begin
    BeginUpdate;
    Capture; //start mouse events capturing
    var p := PointF(X,Y);
    FDragStartLocation := p;
    AreaSelector.Position.Point := p;
    AreaSelector.Size.Size := TSizeF.Create(0 ,0); //there's no TSizeF.Zero (like TPointF.Zero)
    EndUpdate;
    end;
end;

procedure Swap(var a, b: Single); inline; //there's no Swap(Single, Single) or SwapF
begin
  var temp := a;
  a := b;
  b := temp;
end;

procedure Order(var a, b: Single); inline;
begin
  if (a > b) then
    Swap(a, b);
end;

{
procedure Order(var a, b, c: Single); inline;
begin
  if (a > b) then
    Swap(a, b);
  if (b > c) then
    Swap(b, c);
end;
}

procedure TManipulator.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited; //needed so that FMX will know this wasn't a MouseClick (that we did drag between MouseDown and MouseUp) and for event handlers to be fired (e.g. at ancestors)

  if not EditMode then exit;

  if (ssLeft in Shift) then
  begin
    //FDragging := true;
    with AreaSelector do
    begin
      var LX: Single := FDragStartLocation.X; //Delphi 11 compiler issue, gives below at call to Order function error "E2033 Types of actual and formal var parameters must be identical if we skip ": Single", even though it is of type Single
      Order(LX, X);

      var LY: Single := FDragStartLocation.Y; //Delphi 11 compiler issue, gives "E2033 Types of actual and formal var parameters must be identical if we skip ": Single", even though it is of type Single
      Order(LY, Y);

      BoundsRect := RectF(LX, LY, X, Y);
    end;
  end;
end;

procedure TManipulator.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited; //needed for event handlers to be fired (e.g. at ancestors)

  if not EditMode then exit;

  if (ssLeft in Shift) then
    begin
    //fDragging := false;
    ReleaseCapture; //stop mouse events capturing
    end;
end;

procedure TManipulator.MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  Shift := FMouseShift; //TODO: remove if Delphi fixes related bug (more at FMouseShift definition)

  inherited; //needed for event handlers to be fired (e.g. at ancestors)

  if not EditMode then exit;

  if (ssLeft in Shift) then
  begin
    var LObj := Self.ObjectAtPoint(LocalToScreen(PointF(X, Y))); //TODO: define ObjectAtPoint and ObjectAtPointLocal, also add param to not do iteration to grand-children (see internal impl)
    if Assigned(LObj) and (LObj.GetObject is TControl) then
      with AreaSelector do
        BoundsRect := TControl(LObj.GetObject).BoundsRect;
  end;
end;

procedure TManipulator.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  if (ssAlt in Shift) and not EditMode then //TODO: Fix StoryItem hierarchy (DropTarget issue) to use EditMode (WILL GET FIXED WITH option to not get child items most probably), THIS ALSO AFFECTS MOUSECLICK (SO AREASELECTOR DOESN'T GET PLACED AT CLICKED CHILD BOUNDS)
  begin
    var ScreenMousePos := Screen.MousePos;
    var LObj := ObjectAtPoint(ScreenMousePos);
    if Assigned(LObj) then
    begin
      var Control := TControl(LObj.GetObject).ParentControl; //TODO: fix hack - have an ObjectAtPoint/ObjectAtPointLocal that has param to not recurse into children
      var zoom_center := Control.ScreenToLocal(ScreenMousePos);

      var new_scale : single;
      if WheelDelta >= 0
        then new_scale := (1 + (WheelDelta / 120)/5)
        else new_scale := 1 / (1 - (WheelDelta / 120)/5);

      BeginUpdate;
      Control.Size.Size := TSizeF.Create(Control.Width * new_scale, Control.Height * new_scale);

      // correction for position when scaling
      Control.Position.Point := PointF(Control.Position.X + zoom_center.x * (1-new_scale), Control.Position.Y + zoom_center.y * (1-new_scale));
      EndUpdate;

      exit;
    end; //adapted from https://stackoverflow.com/a/66049562/903783
  end;

  inherited; //if we didn't handle the event, needed for event handlers to be fired (e.g. at ancestors)
end;

{$region 'Gestures'}

procedure TManipulator.DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
   inherited;

   if EventInfo.GestureID = igiPan then
    handlePan(EventInfo)
  else if EventInfo.GestureID = igiZoom then
    handleZoom(EventInfo)
  else if EventInfo.GestureID = igiRotate then
    handleRotate(EventInfo)
  else if EventInfo.GestureID = igiPressAndTap then
    handlePressAndTap(EventInfo);

  inherited;
end;

procedure TManipulator.HandlePan(EventInfo: TGestureEventInfo);
begin
  var LObj := Self.ObjectAtPoint(LocalToScreen(EventInfo.Location));
  if Assigned(LObj) then
  begin
    if (not (TInteractiveGestureFlag.gfBegin in EventInfo.Flags)) and
       (LObj.GetObject is TControl) then
    begin
      BeginUpdate;
      var Control := TControl(LObj.GetObject);
      control.Position.Point := PointF(
        EnsureRange(Control.Position.X + (EventInfo.Location.X - FLastPosition.X), 0, Width - Control.Width),
        EnsureRange(Control.Position.Y + (EventInfo.Location.Y - FLastPosition.Y), 0, Height - Control.Height)
      );
      EndUpdate;
    end;

    FLastPosition := EventInfo.Location;
  end;
end;

procedure TManipulator.HandleRotate(eventInfo: TGestureEventInfo);
var
  RotatedControl: IRotatedControl;
begin
  var LObj := Self.ObjectAtPoint(LocalToScreen(EventInfo.Location));
  if Assigned(LObj) then
    if Supports(LObj.GetObject, IRotatedControl, RotatedControl) then
      RotatedControl.RotationAngle := RadToDeg(-EventInfo.Angle);
end;

procedure TManipulator.HandleZoom(EventInfo: TGestureEventInfo);
begin
  var LObj := Self.ObjectAtPoint(LocalToScreen(EventInfo.Location));
  if (not (TInteractiveGestureFlag.gfBegin in EventInfo.Flags)) and
     (LObj.GetObject is TControl) then
  begin
    //BeginUpdate;
    var Control := TControl(LObj.GetObject);
    var dHalfDistance := (EventInfo.Distance - FLastDistance) / 2;
    with Control do
    begin
      Width := Width + dHalfDistance;
      Height := Height + dHalfDistance;
      Position.X := Position.X - dHalfDistance;
      Position.Y := Position.Y - dHalfDistance;
    end;
    //EndUpdate
  end;

  FLastDIstance := EventInfo.Distance;
end;

procedure TManipulator.HandlePressAndTap(EventInfo: TGestureEventInfo);
begin
  var LObj := Self.ObjectAtPoint(LocalToScreen(EventInfo.Location));
  //TODO: delete object?
end;

{$endregion}

{$endregion}

{$ENDREGION}

procedure RegisterClasses;
begin
  RegisterFmxClasses([TManipulator]); //register for persistence (in case they're used standalone)
end;

procedure Register;
begin
  GroupDescendentsWith(TManipulator, TControl);
  RegisterClasses;
  RegisterComponents('Zoomicon', [TManipulator]);
end;

initialization
  RegisterClasses; //don't call Register here, it's called by the IDE automatically on a package installation (fails at runtime)

end.
