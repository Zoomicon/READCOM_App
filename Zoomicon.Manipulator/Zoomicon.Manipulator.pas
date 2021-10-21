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
  SELECTION_DEFAULT_WIDTH = 100;
  SELECTION_DEFAULT_HEIGHT = 100;

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
    function GetControls(const RectPicker: TRectFPredicate): TControlList;
    function GetIntersected: TControlList; virtual;
    function GetContained: TControlList; virtual;
  published
    property Intersected: TControlList read GetIntersected stored false;
    property Selected: TControlList read GetContained stored false;
  end;

  {$ENDREGION .................................................................}

  {$REGION 'TManipulator' -----------------------------------------------------}

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

    procedure HandleAreaSelectorMoving(Sender: TObject; const DX, DY: Single; out Canceled: Boolean);
    procedure HandleAreaSelectorMoved(Sender: TObject; const DX, DY: Single);

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

  {$ENDREGION .................................................................}

procedure Register;

implementation //---------------------------------------------------------------

uses
  FMX.Ani,
  FMX.Effects,
  Zoomicon.Generics.Functors, //for TF
  Zoomicon.Generics.Collections; //for TListEx

{$R *.fmx}

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
      FRestorePosition := Position.Point - TPointF.Create(DX, DY)
    else
      begin
      with ParentControl.Position do
        Point := Point + TPointF.Create(DX, DY); //Move parent control

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

  procedure CreateLocationSelector;
  begin
    var LocationSelector := TLocationSelector.Create(FAreaSelector); //don't use Self so that the middle SelectionPoint doesn't show up in the frame designer
    with LocationSelector do
      begin
      ParentBounds := false; //can move outside of parent area
      GripSize := SELECTION_GRIP_SIZE;
      Align := TAlignLayout.Center;
      Parent := FAreaSelector;
      OnParentMoving := HandleAreaSelectorMoving;
      OnParentMoved := HandleAreaSelectorMoved;
      end;
  end;

begin
  inherited;
  CreateAreaSelector;
  CreateLocationSelector; //must do after CreateAreaSelector
  FAutoSize := true; //must do after CreateAreaSelector
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
          Point := Point + TPointF.Create(DX, DY);
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
  GroupDescendentsWith(TLocationSelector, TManipulator);
  GroupDescendentsWith(TAreaSelector, TManipulator);
  GroupDescendentsWith(TManipulator, TControl);
  RegisterFmxClasses([TLocationSelector, TAreaSelector, TManipulator]); //register for persistence (needed if we use as SubComponent)
  RegisterComponents('Zoomicon', [TLocationSelector, TAreaSelector, TManipulator]);
end;

end.
