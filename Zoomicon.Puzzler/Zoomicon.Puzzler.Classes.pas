unit Zoomicon.Puzzler.Classes;

interface
  uses
    //Zoomicon.Puzzler.Models, //for IShuffler, IHasTarget
    FMX.Controls; //for TControl

{$REGION 'Shuffler'}

type
  TControlShufflerHelper = class helper for TControl//(IShuffler)
    //procedure IShuffler.ShuffleItems = ShufflePositions;
    procedure ShuffleItems;
    procedure ShufflePositions;
  end;

{$ENDREGION}

{$REGION 'HasTarget'}

const
  DEFAULT_TARGET_TOLERANCE : Single = 10;

type
  TControlHasTargetHelper = class helper(TControlShufflerHelper) for TControl//(IHasTarget) //NOTE: Make sure we use an inheritance chain in the order the helpers for TControl are declared here, else only the latest one is applied (when multiple helpers for same class with no inheritance between those helpers are declared)
  public
    {TargetTolerance}
    class var TargetTolerance: Single;
    class constructor Create; //initializes TargetTolerance to DEFAULT_TARGET_TOLERANCE

    {Target}
    function GetTarget: TControl;
    procedure SetTarget(const Target: TControl);

    {DistanceToTarget}
    function GetDistanceToTarget: Single;

    {OverTarget}
    function IsOverTarget: Boolean;

    property Target: TControl read GetTarget write SetTarget;
    property DistanceToTarget: Single read GetDistanceToTarget;
    property OverTarget: Boolean read IsOverTarget;
  end;

type
  TControlMultipleHasTargetHelper = class helper(TControlHasTargetHelper) for TControl//(IMultipleHasTarget) //NOTE: Make sure we use an inheritance chain in the order the helpers for TControl are declared here, else only the latest one is applied (when multiple helpers for same class with no inheritance between those helpers are declared)
  public
    {AllOverTarget}
    function AreAllOverTarget: Boolean;

    property AllOverTarget: Boolean read AreAllOverTarget;
  end;

{$ENDREGION}

implementation
  uses
    Zoomicon.Puzzler.Models, //for IHasTarget
    Zoomicon.Generics.Collections, //for TListEx
    System.Types; //for PointF

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
  if Assigned(Target) then
    result := Position.Point.Distance(Target.Position.Point)
  else
    result := 0;
end;

{$endregion}

{$region 'IsOverTarget'}

function TControlHasTargetHelper.IsOverTarget: Boolean;
begin
  result := DistanceToTarget <= TargetTolerance;
end;

{$endregion}

{$ENDREGION}

{$REGION 'TControlMultipleHasTargetHelper'}

function TControlMultipleHasTargetHelper.AreAllOverTarget: Boolean;
begin
  result := TObjectListEx<TControl>.GetAllInterface<IHasTarget>(Controls).All(function(item: IHasTarget): Boolean
      begin
        result := item.OverTarget;
      end
    );
end;

{$ENDREGION}

end.
