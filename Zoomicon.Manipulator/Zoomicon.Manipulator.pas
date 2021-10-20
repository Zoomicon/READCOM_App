unit Zoomicon.Manipulator;

interface //--------------------------------------------------------------------

uses
  Zoomicon.Collections, //for TListEx
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Gestures,
  //FMX.Layouts, //for TLayout
  FMX.Objects; //for TSelection

const
  SELECTION_NAME_PREFIX = 'Sel_';
  SELECTION_GRIP_SIZE = 8;
  SELECTION_DEFAULT_WIDTH = 100;
  SELECTION_DEFAULT_HEIGHT = 100;

type
  TRectFPredicate = reference to function(Rectangle: TRectF): Boolean;

  {$region 'AreaSelector'}

  TAreaSelector = class(TSelection)
  protected
    function GetControls(const RectPicker: TRectFPredicate): TControlList;
    function GetIntersected: TControlList; virtual;
    function GetContained: TControlList; virtual;
  published
    property Intersected: TControlList read GetIntersected stored false;
    property Selected: TControlList read GetContained stored false;
  end;

  TAreaSelectorList = TListEx<TAreaSelector>;

  {$endregion}

  TManipulator = class(TFrame)
    procedure DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean); override;
    procedure Click; override;

  protected
    FAreaSelector: TAreaSelector;
    FAutoSize: Boolean;

    procedure DoAutoSize;
    procedure SetAutoSize(const Value: Boolean);

    {EditMode}
    function IsEditMode: Boolean;
    procedure SetEditMode(const Value: Boolean);

    {Proportional}
    function IsProportional: Boolean;
    procedure SetProportional(value: Boolean);

    procedure HandleChangeTracking(Sender: TObject; var X, Y: Single);

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
    property AutoSize: Boolean read FAutoSize write SetAutoSize default true;
    property EditMode: Boolean read IsEditMode write SetEditMode default false;
    property Proportional: Boolean read IsProportional write SetProportional;

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
  FMX.Ani,
  FMX.Effects;

{$R *.fmx}

{$REGION 'TAreaSelector' ------------------------------------------------------}

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
      result := TObjectListEx<TFmxObject>.GetAllOfClass<TControl>(Parent.Children, nil,
        function(AControl: TControl): Boolean
        begin
          result := RectPicker(AControl.BoundsRect);
        end
      );
    end;
end;

{$ENDREGION ...................................................................}

{$REGION 'TMoverSelectionPoint' -----------------------------------------------}

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

{$ENDREGION ...................................................................}

{$REGION 'TManipulator' -------------------------------------------------------}

constructor TManipulator.Create(AOwner: TComponent);

  procedure CreateAreaSelector;
  begin
     FAreaSelector := TAreaSelector.Create(Self);
     With FAreaSelector do
       begin
       Size.Size := TSizeF.Create(SELECTION_DEFAULT_WIDTH, SELECTION_DEFAULT_HEIGHT);
       Visible := false;
       GripSize := SELECTION_GRIP_SIZE;
       end;
     FAreaSelector.Parent := Self;
  end;

  procedure CreateMoveSelector;
  begin
    var SelectionPoint := TSelectionPoint.Create(FAreaSelector); //don't use Self so that the middle SelectionPoint doesn't show up in the frame designer
    with SelectionPoint do
      begin
      ParentBounds := false;
      GripSize := SELECTION_GRIP_SIZE;
      Align := TAlignLayout.Center;
      Parent := FAreaSelector;
      OnTrack := HandleChangeTracking;
      end;
  end;

begin
  inherited;
  CreateAreaSelector;
  CreateMoveSelector; //must do after CreateAreaSelector
  FAutoSize := true; //must do after CreateAreaSelector
end;

{$region Manipulation}

procedure TManipulator.MoveControls(const Controls: TControlList; const DX, DY: Single);
begin
  if (DX <> 0) or (DY <> 0) then
    begin
    BeginUpdate;

    TListEx<TControl>.ForEach(Controls,
      procedure (Control: TControl)
      begin
        Control.Position.Point.Offset(DX, DY);
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

    TObjectListEx<TControl>.ForEach(Controls,
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
      //for var item in theSelections do item.Align := TAlignLayout.None;

      var rect := GetChildrenRect;
      SetSize(rect.Width, rect.Height);

      //for var item in theSelections do item.Align := TAlignLayout.Scale;
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
  with AreaSelector do
  begin
  //Show or Hide selection UI
  Visible := Value; //this will also hide the move control point
  //Show or Hide any SelectionPoint children
  for var si := 0 to ChildrenCount-1 do
    begin
    var sc := Children[si];
    if (sc is TSelectionPoint) then
     TSelectionPoint(sc).Visible := Value;
    end;
  end;
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
    MoveControls((abs(newX)-newX)/2, (abs(newY)-newY)/2); //this will also call DoAutoSize

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

{$ENDREGION ...................................................................}

procedure Register;
begin
  GroupDescendentsWith(TManipulator, TControl);
  GroupDescendentsWith(TAreaSelector, TManipulator);
  GroupDescendentsWith(TMoverSelectionPoint, TManipulator);
  RegisterFmxClasses([TManipulator, TAreaSelector, TMoverSelectionPoint]); //register for persistence (needed if we use as SubComponent)
  RegisterComponents('Zoomicon', [TManipulator, TAreaSelector, TMoverSelectionPoint]);
end;

end.
