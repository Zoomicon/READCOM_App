unit Zoomicon.Manipulator;

interface //--------------------------------------------------------------------

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Gestures,
  //FMX.Layouts, //for TLayout
  FMX.Objects; //for TSelection

const
  SELECTION_NAME_PREFIX = 'Sel_';
  SELECTION_GRIP_SIZE = 8;

type

  TManipulator = class(TFrame)
    procedure DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean); override;
    procedure Click; override;

  protected
    FAutoSize: Boolean;
    FEditMode: Boolean;
    FProportional: Boolean;

    procedure DoAutoSize;
    procedure SetAutoSize(const Value: Boolean);
    procedure SetEditMode(const Value: Boolean);
    procedure SetSelectorsVisible(const Value: Boolean);
    procedure SetProportional(value: Boolean);

    function IsSpecialObject(const AObject: TFmxObject): Boolean;
    procedure AddMoveSelectionPoint(const ASelection: TSelection);
    function WrapObjectWithSelection(const AControl: TControl): TSelection;

    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoInsertObject(Index: Integer; const AObject: TFmxObject); override;

    procedure HandleChangeTracking(Sender: TObject; var X, Y: Single);

  public
    constructor Create(AOwner: TComponent); override;
    procedure MoveChildren(const DX, DY: Single);

  published
    property AutoSize: Boolean read FAutoSize write SetAutoSize default true;
    property EditMode: Boolean read FEditMode write SetEditMode default false;
    property Proportional: Boolean read FProportional write SetProportional;

  end;

  TMoverSelectionPoint = class(TSelectionPoint)
  protected
    //procedure DoChangeTracking(var X, Y: Single); override; //unfortunately ancestor method isn't virtual, using OnTrack event handler instead
    procedure HandleChangeTracking(Sender: TObject; var X, Y: Single);
  public
    constructor Create(AOwner: TComponent); override;
  end;

procedure Register;

implementation //---------------------------------------------------------------

uses
  Zoomicon.Collections,
  FMX.Ani,
  FMX.Effects;

{$R *.fmx}

{$REGION 'TMoverSelectionPoint'}

constructor TMoverSelectionPoint.Create(AOwner: TComponent);
begin
  inherited;
  OnTrack := HandleChangeTracking;
end;

procedure TMoverSelectionPoint.HandleChangeTracking(Sender: TObject; var X, Y: Single);
begin
  if (Parent is TControl) then
    begin
    var ParentControl := TControl(Parent);
    var Pos := ParentControl.Position;
    ParentControl.Position := TPosition.Create(TPointF.Create(Pos.X + X, Pos.Y + Y)); //Move parent control
    end;
end;

{$endregion}

{$REGION 'TManipulator'}

constructor TManipulator.Create(AOwner: TComponent);
begin
  inherited;
  FAutoSize := True;
end;

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

    //temporarily disable Align:=Scale setting of all selectors and set it back again when done
    //var selections := TObjectListEx<TControl>.GetAllOfClass<TSelection>(Controls);
    //for var item in selections do item.Align := TAlignLayout.None;

    var rect := GetChildrenRect;
    SetSize(rect.Width, rect.Height);

    //for var item in selections do item.Align := TAlignLayout.Scale;
    //FreeAndNil(selections);

    EndUpdate;
    end;
end;

{$endregion}

{$region 'EditMode'}

procedure TManipulator.SetEditMode(const Value: Boolean);
begin
  FEditMode := Value;
  SetSelectorsVisible(Value);
end;

procedure TManipulator.SetSelectorsVisible(const Value: boolean);
begin
  for var i := 0 to ChildrenCount-1 do
  begin
    var c := Children[i];
    if (c is TSelection) then
       begin
       var Selection := TSelection(c);
       //Show or Hide selection UI
       Selection.HideSelection := not Value; //not using Visible since it would show/hide contents too
       //Show or Hide any SelectionPoint children
       with Selection do
         for var si := 0 to ChildrenCount-1 do
           begin
           var sc := Children[si];
           if (sc is TSelectionPoint) then
             TSelectionPoint(sc).Visible := Value;
           end;
       end;
  end;
end;

{$endregion}

{$region 'Proportional'}

procedure TManipulator.SetProportional(value: boolean);
var
  i: Integer;
  c: TFmxObject;
begin
  //if Value then Align := TAlignLayout.Fit else Align := TAlignLayout.Scale;

  fProportional := value;
  for i := 0 to ChildrenCount-1 do
  begin
    c := Children[i];
    if (c is TSelection) then
       begin
       TSelection(c).Proportional := value;
       end;
  end;
end;

{$endregion}

{$region 'TSelection wrappers'}

procedure TManipulator.AddMoveSelectionPoint(const ASelection: TSelection);
begin
  var SelectionPoint := TSelectionPoint.Create(ASelection); //don't use Self so that the middle SelectionPoint doesn't show up in the frame designer
  with SelectionPoint do
    begin
    ParentBounds := false;
    GripSize := SELECTION_GRIP_SIZE;
    Align := TAlignLayout.Center;
    Parent := ASelection;
    //SetSubComponent(true); //do not use, no need to persist the settings of the SelectionPoint component with the frame
    //BringToFront;
    OnTrack := HandleChangeTracking;
    end;
end;

function TManipulator.WrapObjectWithSelection(const AControl: TControl): TSelection;
begin
  var Selection:= TSelection.Create(Self); //can use AOwner as Owner maybe
  with Selection do
    begin
    //SetSubComponent(true); //store settings directly in the frame when onwer isn't Self
    BoundsRect := AControl.BoundsRect;
    //Align := TAlignLayout.Scale; //TODO: doesn't work with DoAutoSize
    HideSelection := not EditMode;
    Proportional := self.Proportional; //to Preserve aspect ratio when resizing
    GripSize := SELECTION_GRIP_SIZE;
    end;

  With AControl do //reparent
    begin
    Align := TAlignLayout.Contents; //fit Selection parent
    Parent := Selection;
    end;

  {}AddMoveSelectionPoint(Selection); //add SelectionPoint at center to move control

  result := Selection;
end;

function TManipulator.IsSpecialObject(const AObject: TFmxObject): Boolean;
begin
  //ShowMessage(AObject.QualifiedClassName);
  Result :=
    (csDesigning in Self.ComponentState) or
    (AObject is TEffect) or
    (AObject is TAnimation) or
    (AObject is TSelection) or
    (AObject = Self) or
    (AObject.Parent = Self) or
    not (AObject Is TControl);
end;

procedure TManipulator.DoAddObject(const AObject: TFmxObject);
begin
  if IsSpecialObject(AObject) then
    inherited
  else //for other objects added to the frame
    begin
    var Control := AObject As TControl;
    var Selection := WrapObjectWithSelection(Control);
    inherited DoAddObject(Selection); //don't call AddObject (will do infinite loop since it calls DoAddObject)
    Selection.Name := SELECTION_NAME_PREFIX + Control.Name; //only do this here, before that AObject hasn't yet gotten a unique name from its TFrame Owner
    //Selection.InsertComponent(Control);
    //Control.SetSubComponent(true);
    end;
end;

procedure TManipulator.DoInsertObject(Index: Integer; const AObject: TFmxObject);
begin
  if IsSpecialObject(AObject) then
    inherited
  else //for other objects added to the frame
    begin
    var Control := AObject As TControl;
    var Selection := WrapObjectWithSelection(Control);
    inherited DoInsertObject(Index, Selection); //don't call InsertObject (will do infinite loop since it calls DoInsertObject)
    Selection.Name := SELECTION_NAME_PREFIX + Control.Name; //only do this here, before that AObject hasn't yet gotten a unique name from its TFrame Owner
    //Selection.InsertComponent(Control);
    //Control.SetSubComponent(true);
    end;
end;

{$endregion}

{$region 'Events handling'}

procedure TManipulator.MoveChildren(const DX, DY: Single);
begin
  if (DX = 0) and (DY = 0) then
    exit;

  BeginUpdate;
  for var control in Controls do
    control.Position.Point.Offset(DX, DY);
  DoAutoSize;
  EndUpdate;
end;

procedure TManipulator.HandleChangeTracking(Sender: TObject; var X, Y: Single);
begin
  BeginUpdate;
  var SelectionPoint := TSelectionPoint(Sender); //assuming events are sent by TSelectionPoint
  with SelectionPoint do
    begin
    var Pos := ParentControl.Position.Point;
    var PressedPos := SelectionPoint.PressedPosition;

    //Move dragged control:
    var newX := Pos.X + X - PressedPos.X/2 - ParentControl.Width/2;
    var newY := Pos.Y + Y - PressedPos.Y/2 - ParentControl.Height/2;
    ParentControl.Position.Point := TPointF.Create(newX, newY); //Move parent control //not creating TPosition objects to avoid leaking (TPointF is a record)

    //Offset all children (including this one) by the amount this control got into negative coordinates:
    MoveChildren((abs(newX)-newX)/2, (abs(newY)-newY)/2); //this will also call DoAutoSize

    //SelectionPoint.PressedPosition := TPointF.Zero;
    SelectionPoint.Position.Point := TPointF.Zero;
    //SelectionPoint.Align := TAlignLayout.Center;
    end;
  EndUpdate;
end;

procedure TManipulator.Click;
begin
  ShowMessage('Click');
  //TODO
  inherited;
end;

procedure TManipulator.DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  ShowMessage('Gesture');
  //TODO
  inherited;
end;

{$endregion}

{$endregion}

procedure Register;
begin
  GroupDescendentsWith(TManipulator, TControl);
  RegisterFmxClasses([TManipulator, TMoverSelectionPoint]); //register for persistence (needed if we use as SubComponent)
  RegisterComponents('Zoomicon', [TManipulator, TMoverSelectionPoint]);
end;

end.
