unit Zoomicon.Puzzler.Classes;

interface
  uses
    Zoomicon.Puzzler.Models, //for {IShuffler,} IHasSource, IHasTarget
    System.Classes, //for TComponent
    System.Types, //for TPointF
    FMX.Objects, //for TLine, TCircle
    FMX.Controls, //for TControl
    FMX.Graphics, //for TStrokeBrush
    Zoomicon.Helpers.FMX.Controls.ControlHelpers; //to keep helper chain (Delphi compiler only sees last helper used for specific class if we don't define inheritance chain)

{$REGION 'Shuffler'}

type
  TControlShufflerHelper = class helper(TControlFocusHelper{, IShuffler}) for TControl
    //procedure IShuffler.ShuffleItems = ShufflePositions;
    procedure ShuffleItems;
    procedure ShufflePositions;
  end;

{$ENDREGION}

{$REGION 'HasTarget'}

const
  DEFAULT_TARGET_TOLERANCE : Single = -1; //not using target tolerance value by default (using Min(Target.Width, Target.Height)/2 instead)

type
  TControlHasTargetHelper = class helper(TControlShufflerHelper{, IHasTarget}) for TControl //NOTE: Make sure we use an inheritance chain in the order the helpers for TControl are declared here, else only the latest one is applied (when multiple helpers for same class with no inheritance between those helpers are declared)
  public
    {TargetTolerance}
    class var TargetTolerance: Single;
    class constructor Create; //initializes TargetTolerance to DEFAULT_TARGET_TOLERANCE

  protected
    {Target}
    function GetTarget: TControl; virtual;
    procedure SetTarget(const Target: TControl); virtual;

    {DistanceToTarget}
    function GetDistanceToTarget: Single; virtual;

    {OverTarget}
    function IsOverTarget: Boolean; virtual;

  public
    property Target: TControl read GetTarget write SetTarget;
    property DistanceToTarget: Single read GetDistanceToTarget;
    property OverTarget: Boolean read IsOverTarget;
  end;

type
  TControlMultipleHasTargetHelper = class helper(TControlHasTargetHelper{, IMultipleHasTarget}) for TControl //NOTE: Make sure we use an inheritance chain in the order the helpers for TControl are declared here, else only the latest one is applied (when multiple helpers for same class with no inheritance between those helpers are declared)
  protected
    procedure PaintTargetLines(const Opacity: Single = 1; const Brush: TStrokeBrush = nil);

    {AllOverTarget}
    function AreAllOverTarget: Boolean;

  public
    property AllOverTarget: Boolean read AreAllOverTarget;
  end;

{$ENDREGION}

{$region 'TArrow'} //TODO: move to separate package


type
  TArrow = class(TLine{, IArrow}) //TODO: add IArrow interface
  const
    DEFAULT_TIP_SIZE = 6;

  protected
    FTip: TShape;
    FInverted: Boolean;

  public
    {TipSize}
    function GetTipSize: Single;
    procedure SetTipSize(const Value: Single);

    {Source}
    function GetSourcePoint: TPointF;
    procedure SetSourcePoint(const Value: TPointF);

    {Target}
    function GetTargetPoint: TPointF;
    procedure SetTargetPoint(const Value: TPointF);

  public
    constructor Create(AOwner: TComponent); override;

  published
    property TipSize: Single read GetTipSize write SetTipSize;
    property SourcePoint: TPointF read GetSourcePoint write SetSourcePoint;
    property TargetPoint: TPointF read GetTargetPoint write SetTargetPoint;
  end;

{$endregion}

{$region 'TLink'}

  TLink = class(TArrow, {IArrow, ILink,} IHasSource, IHasTarget) //TODO: declare IArrow and ILink (=IHasSource+IHasTarget) interfaces when added
  protected
    FSource, FTarget: TControl;

    {Source}
    function GetSource: TControl; virtual;
    procedure SetSource(const Value: TControl); virtual;

    {Target}
    procedure SetTarget(const Value: TControl); override;

  published
    property Source: TControl read GetSource write SetSource;
  end;

{$endregion}

procedure Register;

implementation
  uses
    System.SysUtils, //for FreeAndNil
    FMX.Types, //for RegisterFmxClasses
    Math, //for Min
    Zoomicon.Generics.Collections; //for TListEx

{$REGION 'TControlShufflerHelper'}

procedure TControlShufflerHelper.ShuffleItems;
begin
  ShufflePositions;
end;

procedure TControlShufflerHelper.ShufflePositions;
var pos: TListEx<TPointF>;
begin
  //get positions
  TListEx<TControl>.ForEach(Controls, procedure(control: TControl)
    begin
      pos.Add(Control.Position.Point);
    end
  );

  //shuffle positions
  pos.Shuffle();

  //reset positions
  var i : Integer := 0;
  TListEx<TControl>.ForEach(Controls, procedure(control: TControl)
    begin
    Control.Position.Point := pos[i];
    inc(i);
    end
  );
end;

{$ENDREGION}

{$REGION 'TControlHasTargetHelper'}

{$region 'Target'}

function TControlHasTargetHelper.GetTarget: TControl;
begin
  result := TControl(Tag);
end;

procedure TControlHasTargetHelper.SetTarget(const Target: TControl);
begin
  Tag := NativeInt(Target);
end;

{$endregion}

{$region 'TargetTolerance'}

class constructor TControlHasTargetHelper.Create;
begin
  TargetTolerance := DEFAULT_TARGET_TOLERANCE;
end;

{$endregion}

{$region 'DistanceToTarget'}

function TControlHasTargetHelper.GetDistanceToTarget: Single;
begin
  if Assigned(Target) then //TODO: maybe check if in same parent
    result := Position.Point.Distance(Target.Position.Point) //Note: assumes the two controls are inside the same Parent
  else
    result := 0; //TODO: maybe return -1 for invalid (but check at IsOverTarget)
end;

{$endregion}

{$region 'IsOverTarget'}

function TControlHasTargetHelper.IsOverTarget: Boolean;
begin
  if TargetTolerance <0 then
    result := DistanceToTarget <= Min(Target.Width, Target.Height)/2 //don't use "Max", else could get True for points outside the Target control when (Width <> Height)
  else
    result := DistanceToTarget <= TargetTolerance;
end;

{$endregion}

{$ENDREGION}

{$REGION 'TControlMultipleHasTargetHelper'}

function TControlMultipleHasTargetHelper.AreAllOverTarget: Boolean;
begin
  result :=
    TObjectListEx<TControl>.AllInterfaces<IHasTarget>(Controls, function(item: IHasTarget): Boolean
      begin
        result := item.OverTarget;
      end
    );
end;

procedure TControlMultipleHasTargetHelper.PaintTargetLines(const Opacity: Single = 1; const Brush: TStrokeBrush = nil);
begin
  TObjectListEx<TControl>.ForEachInterface<IHasTarget>(Controls, procedure(item: IHasTarget)
    begin
      if not Assigned(Target) then exit;

      var TargetToleranceTwice := TargetTolerance * 2;
      var TargetRect := TRectF.Create(Target.Position.Point, TargetToleranceTwice, TargetToleranceTwice);
      TargetRect.Offset(-TargetTolerance, -TargetTolerance);
      if Assigned(Brush) then
        begin
        Canvas.DrawLine((item As TControl).Position.Point, Target.Position.Point, Opacity, Brush);
        Canvas.DrawEllipse(TargetRect, Opacity, Brush);
        end
      else
        begin
        Canvas.DrawLine((item As TControl).Position.Point, Target.Position.Point, Opacity);
        Canvas.DrawEllipse(TargetRect, Opacity);
        end;
    end
  );
end;

{$ENDREGION}

{$REGION 'TArrow'}

constructor TArrow.Create(AOwner: TComponent); //TODO: not implemented correctly, bounds always have top-left and bottom-right ordered, need to also keep a mode=1,2,3,4 depending on which opposite corners have source and target and draw accordingly (say via scale=-scale for other line corners and move the target circle)

  procedure AddTipShape;
  begin
    FTip := TCircle.Create(Self);
    with FTip do
      begin
      Stored := false;
      SetSubComponent(true); //don't show in Designer
      end;
    FTip.Parent := Self;
  end;

begin
  inherited;
  LineType := TLineType.Diagonal;
  AddTipShape;
  SetTipSize(DEFAULT_TIP_SIZE); //must do after AddTipShape;
end;

{$region 'TipSize'}

function TArrow.GetTipSize: Single;
begin
  result := FTip.Width;
end;

procedure TArrow.SetTipSize(const Value: Single);
begin
  var HalfValue := Value/2;
  with FTip do
    begin
    Position.Point := PointF(-HalfValue, -HalfValue); //note the TargetPoint is linked to (0,0) so that local coordinate of target point stays the same and the target decoractor doesn't need moving
    Size.Size := TSizeF.Create(Value, Value);
    end;
end;

{$endregion}

{$region 'SourcePoint'}

function TArrow.GetSourcePoint: TPointF;
begin
  result := ShapeRect.BottomRight;
end;

procedure TArrow.SetSourcePoint(const Value: TPointF);
begin
  SetBoundsRect(TRectF.Create(Position.Point, Value, true));
end;

{$endregion}

{$region 'TargetPoint'}

function TArrow.GetTargetPoint: TPointF;
begin
  result := Position.Point;
end;

procedure TArrow.SetTargetPoint(const Value: TPointF);
begin
  Position.Point := Value;
end;

{$endregion}

{$ENDREGION}

{$REGION 'TLink'}

{$region 'Source'}

function TLink.GetSource: TControl;
begin
  result := FSource;
end;

procedure TLink.SetSource(const Value: TControl);
begin
  FSource := Value;
  SetSourcePoint(Value.Position.Point);
end;

{$endregion}

{$region 'Target'}

procedure TLink.SetTarget(const Value: TControl);
begin
  inherited;
  SetTargetPoint(Value.Position.Point);
end;

{$endregion}

{$ENDREGION}

procedure RegisterClasses;
begin
  RegisterFmxClasses([TArrow, TLink]); //register for persistence (in case they're used standalone)
end;

procedure Register;
begin
  GroupDescendentsWith(TArrow, TControl);
  GroupDescendentsWith(TLink, TControl);
  RegisterClasses;
  RegisterComponents('Zoomicon', [TArrow, TLink]);
end;

initialization
  RegisterClasses; //don't call Register here, it's called by the IDE automatically on a package installation (fails at runtime)

end.
