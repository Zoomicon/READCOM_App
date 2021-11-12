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
    function DoGetUpdateRect: TRectF; override; //used to fix bug in TSelection that doesn't consider usage inside a TScaledLayout
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
    {Gestures}
    procedure DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean); override;

    {Mouse}
    procedure MouseDown(Button: TMouseButton;Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

  protected
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
  Zoomicon.Generics.Functors, //for TF
  Zoomicon.Generics.Collections, //for TListEx
  FMX.Ani,
  FMX.Effects,
  System.Math; //for Min

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
  FAutoSize := true; //must do after CreateAreaSelector
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

procedure TManipulator.DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  ShowMessage('Gesture');
  //TODO
  inherited;
end;

procedure TManipulator.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited; //needed for event handlers to be fired (e.g. at ancestors)

  if (ssLeft in Shift) then
    begin
    BeginUpdate;
    Capture; //start mouse events capturing
    var p := TPointF.Create(X,Y);
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

  if (ssLeft in Shift) then
  begin
    //FDragging := true;
    with AreaSelector do
    begin
      var LX: Single := FDragStartLocation.X; //Delphi 11 compiler issue, gives below at call to Order function error "E2033 Types of actual and formal var parameters must be identical if we skip ": Single", even though it is of type Single
      Order(LX, X);

      var LY: Single := FDragStartLocation.Y; //Delphi 11 compiler issue, gives "E2033 Types of actual and formal var parameters must be identical if we skip ": Single", even though it is of type Single
      Order(LY, Y);

      BoundsRect := TRectF.Create(LX, LY, X, Y);
    end;
  end;
end;

procedure TManipulator.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited; //needed for event handlers to be fired (e.g. at ancestors)

  if (ssLeft in Shift) then
    begin
    //fDragging := false;
    ReleaseCapture; //stop mouse events capturing
    end;
end;

{$endregion}

{$ENDREGION ...................................................................}

procedure RegisterClasses;
begin
  RegisterFmxClasses([TLocationSelector, TAreaSelector, TManipulator]); //register for persistence (in case they're used standalone)
end;

procedure Register;
begin
  GroupDescendentsWith(TLocationSelector, TControl);
  GroupDescendentsWith(TAreaSelector, TControl);
  GroupDescendentsWith(TManipulator, TControl);
  RegisterClasses;
  RegisterComponents('Zoomicon', [TLocationSelector, TAreaSelector, TManipulator]);
end;

initialization
  RegisterClasses; //don't call Register here, it's called by the IDE automatically on a package installation (fails at runtime)

end.
