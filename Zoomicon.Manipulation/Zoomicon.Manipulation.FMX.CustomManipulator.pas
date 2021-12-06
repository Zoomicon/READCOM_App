unit Zoomicon.Manipulation.FMX.CustomManipulator;

interface

uses
  Zoomicon.Manipulation.FMX.Selector, //for TLocationSelector, TAreaSelector
  System.Types,
  System.UITypes,
  System.Classes,
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

  {$REGION 'TControlObjectAtHelper' -----------------------------------------------------}

  TControlObjectAtHelper = class helper for TControl //TODO: move to other unit
    protected
      function ObjectAtPoint(const AScreenPoint: TPointF; RecursionDepth: Integer = 0): IControl; overload;

    public
      function ObjectAtPoint(const AScreenPoint: TPointF; Recursive: Boolean): IControl; overload; inline;
      function ObjectAtLocalPoint(const ALocalPoint: TPointF; Recursive: Boolean = true): IControl; inline;
  end;

  {$ENDREGION}

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

    procedure ApplyEditModeToChild(Control: TControl);
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoAutoSize;

    {AutoSize}
    procedure SetAutoSize(const Value: Boolean);

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

    {$region 'Manipulation'}
    {Z-order}
    procedure BringToFrontElseSendToBack(const Control: TControl); overload; inline;

    {Move}
    procedure MoveControl(const Control: TControl; const DX, DY: Single); overload; inline;
    procedure MoveControls(const Controls: TControlList; const DX, DY: Single); overload;
    procedure MoveControls(const DX, DY: Single); overload;
    procedure MoveSelected(const DX, DY: Single);

    {Resize}
    procedure ResizeControl(const Control: TControl; const DW, DH: Single); overload; inline;
    procedure ResizeControls(const Controls: TControlList; const DW, DH: Single); overload;
    procedure ResizeControls(const DW, DH: Single); overload;
    procedure ResizeSelected(const DW, DH: Single);

    {Rotate}
    procedure SetControlAngle(const Control: TControl; const Angle: Single); overload; inline;
    procedure RotateControl(const Control: TControl; const DAngle: Single); overload; inline;
    procedure RotateControls(const Controls: TControlList; const DAngle: Single); overload;
    procedure RotateControls(const DAngle: Single); overload;
    procedure RotateSelected(const DAngle: Single);
    {$endregion}

    property AreaSelector: TAreaSelector read FAreaSelector stored false;
    property DropTarget: TDropTarget read FDropTarget stored false;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default DEFAULT_AUTOSIZE;
    property EditMode: Boolean read IsEditMode write SetEditMode default DEFAULT_EDITMODE;
    property Proportional: Boolean read IsProportional write SetProportional default DEFAULT_PROPORTIONAL;
  end;

  {$ENDREGION}

procedure Register;

implementation

uses
  Zoomicon.Generics.Functors, //for TF
  Zoomicon.Generics.Collections, //for TListEx
  System.Math, //for Min, EnsureInRange
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

function TControlObjectAtHelper.ObjectAtPoint(const AScreenPoint: TPointF; RecursionDepth: Integer = 0): IControl; //based on TControl.ObjectAtPoint
begin
  if not ShouldTestMouseHits then
    Exit(nil);

  var LP := AScreenPoint;
  if FScene <> nil then
    LP := FScene.ScreenToLocal(LP);
  if (ClipChildren or SmallSizeControl) and not PointInObject(LP.X, LP.Y) then
    Exit(nil);

  if (RecursionDepth > 0) and (ControlsCount > 0) then
    for var I := GetLastVisibleObjectIndex - 1 downto GetFirstVisibleObjectIndex do
    begin
      var Control := Controls[I];
      if not Control.GetVisible then
        Continue;

      var NewObj := Control.ObjectAtPoint(AScreenPoint, RecursionDepth - 1);
      if Assigned(NewObj) then
        Exit(NewObj);
      end;

  Result := nil;

  if PointInObject(LP.X, LP.Y) {and CheckHitTest(HitTest)} then //TODO: allow to have option to ignore hit test
    Result := Self;
end;

function TControlObjectAtHelper.ObjectAtPoint(const AScreenPoint: TPointF; Recursive: Boolean): IControl;
begin
  if Recursive then
    result := ObjectAtPoint(AScreenPoint)
  else
    result := ObjectAtPoint(AScreenPoint, 1);
end;

function TControlObjectAtHelper.ObjectAtLocalPoint(const ALocalPoint: TPointF; Recursive: Boolean = true): IControl;
begin
  result := ObjectAtPoint(LocalToScreen(ALocalPoint), Recursive);
end;

{$endregion}

{$REGION 'TCustomManipulator'}

constructor TCustomManipulator.Create(AOwner: TComponent);

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
       BringToFront;
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
      HitTest := False;
      SendToBack;
      //DropTarget.Align := TAlignLayout.Client; //TODO: report to Embarcadero that it fails either before or after setting Parent (have to do at ancestor's constructor)
      OnDragOver := DropTargetDragOver;
      OnDropped := DropTargetDropped;
    end;
    DropTarget.Parent := Self;
  end;

begin
  CreateAreaSelector;
  CreateLocationSelector; //must do after CreateAreaSelector
  CreateDropTarget;

  AutoSize := DEFAULT_AUTOSIZE; //must do after CreateAreaSelector
  EditMode := DEFAULT_EDITMODE;
  Proportional := DEFAULT_PROPORTIONAL;

  inherited; //do last since it will also load designer resource
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

procedure TCustomManipulator.MoveControl(const Control: TControl; const DX, DY: Single);
begin
  BeginUpdate;

  with Control.Position do
  begin
    var NewX := X + DX;
    var NewY := Y + DY;

    if AutoSize then
      Point := PointF(NewX, NewY)
    else
      Point := PointF( EnsureRange(NewX, 0, Width - Control.Width), EnsureRange(NewY, 0, Height - Control.Height) );
  end;

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
        MoveControl(Control, DX, DY);
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

procedure TCustomManipulator.ResizeControl(const Control: TControl; const DW, DH: Single);
begin
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
        ResizeControl(Control, DW, DH);
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

procedure TCustomManipulator.RotateControl(const Control: TControl; const DAngle: Single);
begin
  BeginUpdate;

  with (Control as IRotatedControl) do
    RotationAngle := RotationAngle + DAngle; //seems RotationAngle is protected, but since TControl implements IRotatedControl we can access that property through that interface

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
          RotateControl(Control, DAngle);
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
  AreaSelector.BringToFront;
  if (AObject is TControl) then
    ApplyEditModeToChild(TControl(AObject));
end;

{$REGION 'PROPERTIES'}

{$region 'AutoSize'}

procedure TCustomManipulator.SetAutoSize(const Value: Boolean);
begin
  FAutoSize := Value;
  DoAutoSize;
end;

function IsSelection(obj: TFmxObject): Boolean;
begin
  Result := obj is TSelection;
end;

procedure TCustomManipulator.DoAutoSize;
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

function TCustomManipulator.IsEditMode: Boolean;
begin
  result := AreaSelector.Visible;
end;

procedure TCustomManipulator.ApplyEditModeToChild(Control: TControl);
begin
  if not (Control is TAreaSelector) then //no need to use a Predicate<TControl> to select the non-TSelectors, since we can excluse the TSelectorArea here
    begin
    var NotEditing := not EditMode;
    Control.Enabled := NotEditing; //don't use, will show controls as semi-transparent
    //Control.HitTest := NotEditing; //TODO: seems "HitTest=false" eats up Double-Clicks, they don't propagate to parent control, need to fix
    //Control.SetDesign(EditMode, false);
    end;
end;

procedure TCustomManipulator.SetEditMode(const Value: Boolean);
begin
  if Assigned(FAreaSelector) then
    begin
    FAreaSelector.Visible := Value; //Show or Hide selection UI (this will also hide the move control point) //MUST DO FIRST (AreaSelector.Visible used to detect edit mode)
    FAreaSelector.BringToFront; //always on top
    end;

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
  var LObj := ObjectAtLocalPoint(EventInfo.Location, false);
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
  var LObj := ObjectAtLocalPoint(EventInfo.Location, false);
  if Assigned(LObj) and (LObj.GetObject is TControl) then
    SetControlAngle(TControl(LObj.GetObject), RadToDeg(-EventInfo.Angle));
end;

procedure TCustomManipulator.HandleZoom(EventInfo: TGestureEventInfo);
begin
  var LObj := ObjectAtLocalPoint(EventInfo.Location, false);
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
  var LObj := ObjectAtLocalPoint(EventInfo.Location, false);
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
      var LObj := ObjectAtLocalPoint(PointF(X, Y), false); //TODO: define ObjectAtPoint and ObjectAtPointLocal, also add param to not do iteration to grand-children (see internal impl)
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
    var LObj := ObjectAtLocalPoint(PointF(X, Y), false); //TODO: define ObjectAtPoint and ObjectAtPointLocal, also add param to not do iteration to grand-children (see internal impl)
    if Assigned(LObj) and (LObj.GetObject is TControl) then
      with AreaSelector do
        BoundsRect := TControl(LObj.GetObject).BoundsRect;
  end;
end;

procedure TCustomManipulator.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  if EditMode and (ssAlt in Shift) then
  begin
    var ScreenMousePos := Screen.MousePos;
    var LObj := ObjectAtPoint(ScreenMousePos, 1);
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

      Handled := true; //TODO: is this needed?
      exit;
    end; //adapted from https://stackoverflow.com/a/66049562/903783
  end;

  inherited; //needed for event handlers to be fired (e.g. at ancestors)
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
