unit Zoomicon.Manipulation.FMX.CustomManipulator;

interface

uses
  Zoomicon.Manipulation.FMX.Selector, //for TLocationSelector, TAreaSelector
  System.Classes, //for TShiftState
  System.Math, //for Min, EnsureInRange (inlined)
  System.Types,
  System.UITypes,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.ExtCtrls, //for TDropTarget
  FMX.Gestures, //TODO: is this needed here?
  FMX.Objects; //for TSelection

const
  SELECTION_GRIP_SIZE = 8;
  DEFAULT_AUTOSIZE = false;
  DEFAULT_EDITMODE = false;
  DEFAULT_PROPORTIONAL = false;

type

  {$REGION 'TCustomManipulator' -----------------------------------------------------}

  TCustomManipulator = class(TFrame)
  protected
    FMouseShift: TShiftState; //TODO: if Delphi fixes the Shift parameter to not be empty at MouseClick in the future, remove this

    {Gestures}
    FLastPosition: TPointF;
    FLastDistance: Integer;

    FAreaSelector: TAreaSelector;
    FDropTarget: TDropTarget;
    FAutoSize: Boolean;
    FDragStartLocation: TPointF;

    procedure Loaded; override;
    procedure ApplyEditModeToChild(Control: TControl);
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoAutoSize;

    {AutoSize}
    procedure SetAutoSize(const Value: Boolean);

    {DropTargetVisible}
    function IsDropTargetVisible: Boolean; virtual;
    procedure SetDropTargetVisible(const Value: Boolean); virtual;

    {EditMode}
    function IsEditMode: Boolean; virtual;
    procedure SetEditMode(const Value: Boolean); virtual;

    {Proportional}
    function IsProportional: Boolean; virtual;
    procedure SetProportional(value: Boolean); virtual;

    {$region 'Events'}
    {AreaSelector}
    procedure HandleAreaSelectorMoving(Sender: TObject; const DX, DY: Single; out Canceled: Boolean);
    procedure HandleAreaSelectorMoved(Sender: TObject; const DX, DY: Single);
    procedure HandleAreaSelectorTrack(Sender: TObject);

    {Gestures}
    procedure DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean); override;
    procedure HandlePan(EventInfo: TGestureEventInfo);
    procedure HandleRotate(EventInfo: TGestureEventInfo);
    procedure HandleZoom(EventInfo: TGestureEventInfo);
    procedure HandlePressAndTap(EventInfo: TGestureEventInfo);

    {Mouse}
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;

    {DragDrop}
    procedure DropTargetDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation); virtual;
    procedure DropTargetDropped(Sender: TObject; const Data: TDragObject; const Point: TPointF); overload; virtual;
    procedure DropTargetDropped(const Filepaths: array of string); overload; virtual;
    {$endregion}

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    {$region 'Manipulation'}
    {Z-order}
    procedure BringToFrontElseSendToBack(const Control: TControl); overload; inline;

    {Move}
    procedure MoveControl(const Control: TControl; const DX, DY: Single; const SkipAutoSize: Boolean = false); overload; inline;
    procedure MoveControls(const Controls: TControlList; const DX, DY: Single); overload;
    procedure MoveControls(const DX, DY: Single); overload;
    procedure MoveSelected(const DX, DY: Single);

    {Resize}
    procedure ResizeControl(const Control: TControl; const DW, DH: Single; const SkipAutoSize: Boolean = false); overload; inline;
    procedure ResizeControls(const Controls: TControlList; const DW, DH: Single); overload;
    procedure ResizeControls(const DW, DH: Single); overload;
    procedure ResizeSelected(const DW, DH: Single);

    {Rotate}
    procedure SetControlAngle(const Control: TControl; const Angle: Single); overload; inline;
    procedure RotateControl(const Control: TControl; const DAngle: Single; const SkipAutoSize: Boolean = false); overload; inline;
    procedure RotateControls(const Controls: TControlList; const DAngle: Single); overload;
    procedure RotateControls(const DAngle: Single); overload;
    procedure RotateSelected(const DAngle: Single);
    {$endregion}

    property AreaSelector: TAreaSelector read FAreaSelector stored false;
    property DropTarget: TDropTarget read FDropTarget stored false;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default DEFAULT_AUTOSIZE;
    property DropTargetVisible: Boolean read IsDropTargetVisible write SetDropTargetVisible stored false default DEFAULT_EDITMODE;
    property EditMode: Boolean read IsEditMode write SetEditMode default DEFAULT_EDITMODE;
    property Proportional: Boolean read IsProportional write SetProportional default DEFAULT_PROPORTIONAL;
  end;

  {$ENDREGION}

procedure Register;

implementation

uses
  Zoomicon.Helpers.FMX.Controls.ControlHelpers, //for TControlObjectAtHelper, TControlConvertLocalRectHelper, TControlSubComponentHelper
  Zoomicon.Generics.Functors, //for TF
  Zoomicon.Generics.Collections, //for TListEx
  System.SysUtils; //for Supports

{$R *.fmx}

{$region 'Utils'}

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

{$endregion}

{$REGION 'TCustomManipulator'}

constructor TCustomManipulator.Create(AOwner: TComponent);
begin
  inherited; //this will also load designer resource, which will call Loaded and create subcomponents

  AutoSize := DEFAULT_AUTOSIZE; //must do after CreateAreaSelector
  EditMode := DEFAULT_EDITMODE;
  Proportional := DEFAULT_PROPORTIONAL;
end;

destructor TCustomManipulator.Destroy;
begin
  ReleaseCapture; //make sure we always release Mouse Capture
  inherited;
end;

procedure TCustomManipulator.Loaded;

  procedure CreateAreaSelector;
  begin
     FAreaSelector := TAreaSelector.Create(Self);
     With FAreaSelector do
       begin
       Stored := False; //don't store state
       SetSubComponent(true); //don't show in Designer for descendents
       ParentBounds := true;
       Size.Size := TPointF.Zero;
       Visible := false;
       GripSize := SELECTION_GRIP_SIZE;
       BringToFront; //not really doing something since we've set Visible to false
       OnlyFromTop := true; //TODO: add some keyboard modifier to area selector's MouseDown override or something for SHIFT key to override this temporarily (FOnlyFromTopOverride internal flag)
       OnTrack := HandleAreaSelectorTrack;
       end;
     FAreaSelector.Parent := Self;
  end;

  procedure CreateLocationSelector;
  begin
    var LocationSelector := TLocationSelector.Create(FAreaSelector); //don't use Self so that the middle SelectionPoint doesn't show up in the frame designer
    with LocationSelector do
      begin
      Stored := False; //don't store state
      SetSubComponent(true); //don't show in Designer for descendents
      ParentBounds := false; //can move outside of parent area
      GripSize := SELECTION_GRIP_SIZE;
      Align := TAlignLayout.Center;
      OnParentMoving := HandleAreaSelectorMoving;
      OnParentMoved := HandleAreaSelectorMoved;
      Parent := FAreaSelector;
      end;
  end;

  procedure CreateDropTarget;
  begin
    FDropTarget := TDropTarget.Create(Self);
    with FDropTarget do
    begin
      Stored := False; //don't store state
      SetSubComponent(true); //don't show in Designer for descendents
      HitTest := False; //TODO: check if needed for drag-drop
      Visible := EditMode;
      Opacity := 0.4;
      SendToBack; //always do after setting Visible
      DropTarget.Align := TAlignLayout.Client;

      Enabled := true;
      OnDragOver := DropTargetDragOver;
      OnDropped := DropTargetDropped;
    end;
    DropTarget.Parent := Self;
  end;

begin
  inherited;
  CreateAreaSelector;
  CreateLocationSelector; //must do after CreateAreaSelector
  CreateDropTarget; //must do after CreateAreaSelector (to send below that)
end;

{$region 'Manipulation'}

{$region 'Z-order'}

procedure TCustomManipulator.BringToFrontElseSendToBack(const Control: TControl);
begin
  with Children do
    if IndexOf(Control) < Count - 1 then
      Control.BringToFront
    else
      Control.SendToBack;
end;

{$endregion}

{$region 'Move'}

procedure TCustomManipulator.MoveControl(const Control: TControl; const DX, DY: Single; const SkipAutoSize: Boolean = false);
begin
  if Control.SubComponent then exit; //ignore any subcomponents like the DropTarget (or others added by descendents)

  BeginUpdate;

  with Control.Position do
  begin
    var NewX := X + DX;
    var NewY := Y + DY;

    if AutoSize or (not ClipChildren) or (InRange(NewX, 0, Width - 1) and InRange(NewY, 0, Height - 1)) then //allowing controls to move out of bounds if we're set to not clip them or if they're partially clipped (don't want them to totally disappear)
      Point := PointF(NewX, NewY)
    else
      Point := PointF( EnsureRange(NewX, 0, Width - Control.Width), EnsureRange(NewY, 0, Height - Control.Height) );
  end;

  if not SkipAutoSize then
    DoAutoSize;
  EndUpdate;
end;

procedure TCustomManipulator.MoveControls(const Controls: TControlList; const DX, DY: Single);
begin
  if (DX <> 0) or (DY <> 0) then
    begin
    BeginUpdate;

    TListEx<TControl>.ForEach(Controls,
      procedure (Control: TControl)
      begin
        MoveControl(Control, DX, DY, true); //skip autosizing separately for each control
      end
    );

    DoAutoSize;
    EndUpdate;
    end;
end;

procedure TCustomManipulator.MoveControls(const DX, DY: Single);
begin
  MoveControls(Controls, DX, DY);
end;

procedure TCustomManipulator.MoveSelected(const DX, DY: Single);
begin
  if (DX <> 0) or (DY <> 0) then
    begin
    var TheSelected := AreaSelector.Selected;
    MoveControls(TheSelected, DX, DY);
    FreeAndNil(TheSelected);
    end;
end;

{$endregion}

{$region 'Resize'}

procedure TCustomManipulator.ResizeControl(const Control: TControl; const DW, DH: Single; const SkipAutoSize: Boolean = false);
begin
  if Control.SubComponent then exit; //ignore any subcomponents like the DropTarget (or others added by descendents)

  BeginUpdate;

  with Control.Size do
  begin
    var NewWidth := Width + DW;
    var NewHeight := Height + DH;

    //if AutoSize then //TODO: should we clamp Width/Height for child to always be inside parent?
      Size := TSizeF.Create(NewWidth, NewHeight)
    //else
      //Size := ...EnsureRange...
  end;

  Control.Position.Point := Control.Position.Point - PointF(DW/2, DH/2); //resize from center

  if not SkipAutoSize then
    DoAutoSize;
  EndUpdate;
end;

procedure TCustomManipulator.ResizeControls(const Controls: TControlList; const DW, DH: Single);
begin
  if (DW <> 0) or (DH <> 0) then
    begin
    BeginUpdate;

    TListEx<TControl>.ForEach(Controls,
      procedure (Control: TControl)
      begin
        ResizeControl(Control, DW, DH, true); //skip autosizing separately for each control
      end
    );

    DoAutoSize;
    EndUpdate;
    end;
end;

procedure TCustomManipulator.ResizeControls(const DW, DH: Single);
begin
  ResizeControls(Controls, DW, DH);
end;

procedure TCustomManipulator.ResizeSelected(const DW, DH: Single);
begin
  if (DW <> 0) or (DH <> 0) then
    begin
    var TheSelected := AreaSelector.Selected;
    ResizeControls(TheSelected, DW, DH);
    FreeAndNil(TheSelected);
    end;
end;

{$endregion}

{$region 'Rotate'}

procedure TCustomManipulator.SetControlAngle(const Control: TControl; const Angle: Single);
begin
  with (Control as IRotatedControl) do
    RotationAngle := Angle; //seems RotationAngle is protected, but since TControl implements IRotatedControl we can access that property through that interface
end;

procedure TCustomManipulator.RotateControl(const Control: TControl; const DAngle: Single; const SkipAutoSize: Boolean = false);
begin
  if Control.SubComponent then exit; //ignore any subcomponents like the DropTarget (or others added by descendents)

  BeginUpdate;

  with (Control as IRotatedControl) do
    RotationAngle := RotationAngle + DAngle; //seems RotationAngle is protected, but since TControl implements IRotatedControl we can access that property through that interface

  if not SkipAutoSize then
    DoAutoSize;
  EndUpdate;
end;

procedure TCustomManipulator.RotateControls(const Controls: TControlList; const DAngle: Single);
begin
  if (DAngle <>0) then
    begin
    BeginUpdate;

    TListEx<TControl>.ForEach(Controls,
      procedure (Control: TControl)
      begin
        try
          RotateControl(Control, DAngle); //skip autosizing separately for each control
        except //catch exceptions, in case any controls fail when you try to rotate them
          //NOP //TODO: do some error logging
        end;
      end
    );

    DoAutoSize; //TODO: is this needed? Do bounds change on rotation?
    EndUpdate;
    end;
end;

procedure TCustomManipulator.RotateControls(const DAngle: Single);
begin
  RotateControls(Controls, DAngle);
end;

procedure TCustomManipulator.RotateSelected(const DAngle: Single);
begin
  if (DAngle <> 0) then
    begin
    var TheSelected := AreaSelector.Selected;
    RotateControls(TheSelected, DAngle);
    FreeAndNil(TheSelected);
    end;
end;

{$endregion}

{$endregion}

procedure TCustomManipulator.DoAddObject(const AObject: TFmxObject);
begin
  inherited;
  if Assigned(AreaSelector) then
    AreaSelector.BringToFront;
  if (AObject is TControl) then
    ApplyEditModeToChild(TControl(AObject));
end;

{$REGION 'PROPERTIES'}

{$region 'AutoSize'}

procedure TCustomManipulator.SetAutoSize(const Value: Boolean);
begin
  FAutoSize := Value;
  DoAutoSize; //will act only if FAutoSize
end;

function IsSelection(obj: TFmxObject): Boolean;
begin
  Result := obj is TSelection;
end;

procedure TCustomManipulator.DoAutoSize;

  procedure SetControlsAlign(const List: TControlList; const TheAlignment: TAlignLayout);
  begin
    for var Control in List do
      if not ((Control is TDropTarget) or (Control is TRectangle) or (Control is TAreaSelector)) then //TODO: should have some info on which to excempt? or just do this for ones that have TAlignLayout.Scale? Or use some function that sets size without affecting children
        Control.Align := TheAlignment;
  end;

begin
(* //TODO: FAILS TRYING TO SET HUGE SIZE TO BOUNDSRECT
  if (FAutoSize) then
  begin
    BeginUpdate;

    //temporarily disable Align:=Scale setting of children and set it back again when done
    SetControlsAlign(Controls, TAlignLayout.None);

    var rect := GetChildrenRect;
    if Assigned(Parent) then
      rect := ConvertLocalRectTo(Parent as TControl, rect); //TODO: is there a chance the Parent is nil?

    with BoundsRect do
      if (rect.Left < Left) or (rect.Top < Top) or (rect.Right > Right) or (rect.Bottom > Bottom) then //only AutoSize to expand, never shrink down (else would disappear when there were no children)
        BoundsRect := rect; //TODO: seems to fail (probably should invoke later, not in the Track event of the PositionSelector)

    SetControlsAlign(Controls, TAlignLayout.Scale);

    EndUpdate;
  end;
*)
end;

{$endregion}

{$region 'DropTargetVisible'}

function TCustomManipulator.IsDropTargetVisible: Boolean;
begin
  result := DropTarget.Visible;
end;

procedure TCustomManipulator.SetDropTargetVisible(const Value: Boolean);
begin
  with DropTarget do
  begin
    Visible := Value;
    SendToBack; //always do when Visible changed
  end;
end;

{$endregion}

{$region 'EditMode'}

function TCustomManipulator.IsEditMode: Boolean;
begin
  result := Assigned(AreaSelector) and AreaSelector.Visible;
end;

procedure TCustomManipulator.ApplyEditModeToChild(Control: TControl);
begin
  if not Control.SubComponent then
    begin
    var NotEditing := not EditMode;
    Control.Enabled := NotEditing; //note this will show controls as semi-transparent
    //Control.HitTest := NotEditing; //TODO: seems "HitTest=false" eats up Double-Clicks, they don't propagate to parent control, need to fix
    //Control.SetDesign(EditMode, false);
    end;
end;

procedure TCustomManipulator.SetEditMode(const Value: Boolean);
begin
  if Assigned(FAreaSelector) then
    with FAreaSelector do
    begin
      Visible := Value; //Show or Hide selection UI (this will also hide the move control point) //MUST DO FIRST (AreaSelector.Visible used to detect edit mode)
      BringToFront; //always on top (need to do after setting Visible)
    end;

  DropTargetVisible := Value;

  TListEx<TControl>.ForEach( //TODO: should also do this action when adding new controls (so move the inner proc payload to separate method and call both here and after adding new control [have some InitControl that calls such sub-procs])
    Controls,
    ApplyEditModeToChild
  );
end;

{$endregion}

{$region 'Proportional'}

function TCustomManipulator.IsProportional: Boolean;
begin
  result := AreaSelector.Proportional;
end;

procedure TCustomManipulator.SetProportional(Value: boolean);
begin
  //if Value then Align := TAlignLayout.Fit else Align := TAlignLayout.Scale;
  AreaSelector.Proportional := Value;
end;

{$endregion}

{$ENDREGION}

{$REGION 'EVENTS'}

{$region 'AreaSelector'}

procedure TCustomManipulator.HandleAreaSelectorMoving(Sender: TObject; const DX, DY: Single; out Canceled: Boolean);
begin
  BeginUpdate;
  //Move all controls selected by the AreaSelector (that has just been moved)
  MoveSelected(DX, DY); //this will also call DoAutoSize
  Canceled := false;
  //after this, TSelectionPoint.HandleChangeTracking will move the SelectionArea, then fire OnParentMoved event (which is assigned to HandleAreaSelectorMoved of the manipulator)
end;

procedure TCustomManipulator.HandleAreaSelectorMoved(Sender: TObject; const DX, DY: Single);
begin
  //at this point TSelectionPoint.HandleChangeTracking has moved the SelectionArea
  var SelectionPoint := TSelectionPoint(Sender); //assuming events are sent by TSelectionPoint
  with SelectionPoint do
    begin
    //var ParentPos := ParentControl.Position.Point;
    //var newX := ParentPos.X;
    //var newY := ParentPos.Y;

    //if AutoSize and (AreaSelector.SelectedCount > 0) then //Offset all controls (including this one) by the amount the selector got into negative coordinates (ONLY DOING IT FOR NEGATIVE COORDINATES)
      //MoveControls(TF.Iff<Single>(newX < 0, -newX, 0), TF.Iff<Single>(newY < 0, -newY, 0)); //this will also call DoAutoSize //there's also IfThen from System.Math, but those aren't marked as "Inline"
      //TODO: fix this: should only do for controls that moved into negative coordinates, not if the selector moved into such

    //PressedPosition := TPointF.Zero;
    Position.Point := TPointF.Zero;
    //Align := TAlignLayout.Center;
    end;
  EndUpdate;
end;

var lastAreaSelectorSize: TSizeF;

procedure TCustomManipulator.HandleAreaSelectorTrack(Sender: TObject);
begin
  var dSize := AreaSelector.Size.Size - lastAreaSelectorSize;
  ResizeSelected(dSize.Width, dSize.Height);
  lastAreaSelectorSize := AreaSelector.Size.Size;
end;

{$endregion}

{$region 'Gestures'}
//see https://docwiki.embarcadero.com/CodeExamples/Sydney/en/FMXInteractiveGestures_(Delphi)

procedure TCustomManipulator.DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean);
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

procedure TCustomManipulator.HandlePan(EventInfo: TGestureEventInfo);
begin
  var LObj := ObjectAtLocalPoint(EventInfo.Location, false, true, false); //only checking the immediate children
  if Assigned(LObj) then
  begin
    if (not (TInteractiveGestureFlag.gfBegin in EventInfo.Flags)) and
       (LObj.GetObject is TControl) then
      Movecontrol(TControl(LObj.GetObject),
        EventInfo.Location.X - FLastPosition.X, //DX
        EventInfo.Location.Y - FLastPosition.Y //DY
      );

    FLastPosition := EventInfo.Location;
  end;
end;

procedure TCustomManipulator.HandleRotate(eventInfo: TGestureEventInfo);
begin
  var LObj := ObjectAtLocalPoint(EventInfo.Location, false, true, false); //only checking the immediate children
  if Assigned(LObj) and (LObj.GetObject is TControl) then
    SetControlAngle(TControl(LObj.GetObject), RadToDeg(-EventInfo.Angle));
end;

procedure TCustomManipulator.HandleZoom(EventInfo: TGestureEventInfo);
begin
  var LObj := ObjectAtLocalPoint(EventInfo.Location, false, true, false); //only checking the immediate children
  if (not (TInteractiveGestureFlag.gfBegin in EventInfo.Flags)) and
     (LObj.GetObject is TControl) then
  begin
    var Control := TControl(LObj.GetObject);
    var dHalfDistance := (EventInfo.Distance - FLastDistance) / 2;
    Control.SetBounds(Position.X - dHalfDistance, Position.Y - dHalfDistance, Width + dHalfDistance, Height + dHalfDistance);
  end;

  FLastDIstance := EventInfo.Distance;
end;

procedure TCustomManipulator.HandlePressAndTap(EventInfo: TGestureEventInfo);
begin
  var LObj := ObjectAtLocalPoint(EventInfo.Location, false, true, false); //only checking the immediate children
  //TODO: delete object?
end;

{$endregion}

{$region 'Mouse'}

procedure TCustomManipulator.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FMouseShift := Shift; //TODO: remove if Delphi fixes related bug (more at FMouseShift definition) //Note that MouseUp fires before MouseClick so we do need to have this in MouseDown

  inherited; //needed for event handlers to be fired (e.g. at ancestors)

    if (ssDouble in Shift) then //TODO: must do in EditMode (but has issue detecting it)
      begin
      var LObj := ObjectAtLocalPoint(PointF(X, Y), false, true, false); //only checking the immediate children
      if Assigned(LObj) and (LObj.GetObject is TControl) then
        BringToFrontElseSendToBack(TControl(LObj.GetObject));
      exit;
      end;

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

procedure TCustomManipulator.MouseMove(Shift: TShiftState; X, Y: Single);
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

procedure TCustomManipulator.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited; //needed for event handlers to be fired (e.g. at ancestors)

  if not EditMode then exit;

  if (ssLeft in Shift) then
    begin
    //fDragging := false;
    ReleaseCapture; //stop mouse events capturing
    end;
end;

procedure TCustomManipulator.MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  Shift := FMouseShift; //TODO: remove if Delphi fixes related bug (more at FMouseShift definition)

  inherited; //needed for event handlers to be fired (e.g. at ancestors)

  if not EditMode then exit;

  if (ssLeft in Shift) then
  begin
    var LObj := ObjectAtLocalPoint(PointF(X, Y), false, true, false); //only checking the immediate children
    if Assigned(LObj) and (LObj.GetObject is TControl) then
      with AreaSelector do
        BoundsRect := TControl(LObj.GetObject).BoundsRect;
  end;
end;

procedure TCustomManipulator.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  inherited; //needed for event handlers to be fired (e.g. at ancestors)

  if (ssShift in Shift) then
  begin
    //TODO: should tell any ScrollBox parent to pan horizontally
    exit;
  end;

  if EditMode and (ssAlt in Shift) then
  begin
    var ScreenMousePos := Screen.MousePos;
    var LObj := ObjectAtPoint(ScreenMousePos, false, true, false); //only checking the immediate children
    if Assigned(LObj) then
    begin
      var Control := TControl(LObj.GetObject);
      var zoom_center := Control.ScreenToLocal(ScreenMousePos); //use mouse cursor as center

      var new_scale : Single;
      if WheelDelta >= 0
        then new_scale := (1 + (WheelDelta / 120)/5)
        else new_scale := 1 / (1 - (WheelDelta / 120)/5);

      BeginUpdate; //TODO: would it be enough to do Control.BeginUpdate/Control.EndUpdate instead?

      (*
      if (ssCtrl in Shift) then
      begin
        var old_scale := Control.Scale.X;
        Control.Scale.X := new_scale;
        Control.Scale.Y := new_scale;
        Control.Position.Point := Control.Position.Point + zoom_center * (1-new_scale); //TODO: not working correctly
      end
      else
      *)
      begin
        Control.Size.Size := TSizeF.Create(Control.Width * new_scale, Control.Height * new_scale); //TODO: maybe rescale instead of resize to preserve quality?
        //correction for zoom center position
        Control.Position.Point := Control.Position.Point + zoom_center * (1-new_scale);
      end; //TODO: see why control's children that are set to use "Align=Scale" seem to become larger when object shrinks and vice-versa instead of following its change (probably need to tell them to realign / parent size changed?)

      EndUpdate;

      Handled := true; //needed for parent containers to not scroll
      exit;
    end; //adapted from https://stackoverflow.com/a/66049562/903783
  end;
end;

{$endregion}

{$region 'Drop target'}

procedure TCustomManipulator.DropTargetDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  if EditMode then
    Operation := TDragOperation.Copy;
end;

procedure TCustomManipulator.DropTargetDropped(Sender: TObject; const Data: TDragObject; const Point: TPointF);
begin
  if EditMode then
    DropTargetDropped(Data.Files);
end;

procedure TCustomManipulator.DropTargetDropped(const Filepaths: array of string);
begin
  //NOP (override at descendents)
end;

{$endregion}

{$ENDREGION}

procedure RegisterClasses;
begin
  RegisterFmxClasses([TCustomManipulator]); //register for persistence (in case they're used standalone)
end;

procedure Register;
begin
  GroupDescendentsWith(TCustomManipulator, TControl);
  RegisterClasses;
  //RegisterComponents('Zoomicon', [TCustomManipulator]);
end;

initialization
  RegisterClasses; //don't call Register here, it's called by the IDE automatically on a package installation (fails at runtime)

end.
