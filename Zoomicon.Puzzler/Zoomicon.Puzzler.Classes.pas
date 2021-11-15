unit Zoomicon.Puzzler.Classes;

interface
  uses
    //Zoomicon.Puzzler.Models, //for IShuffler
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
  TControlHasTargetHelper = class helper for TControl//(IHasTarget)
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

{$ENDREGION}

implementation
  uses
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

{$endregion}

class constructor TControlHasTargetHelper.Create;
begin
  TargetTolerance := DEFAULT_TARGET_TOLERANCE;
end;

function TControlHasTargetHelper.GetDistanceToTarget: Single;
begin
  if Assigned(Target) then
    result := Position.Point.Distance(Target.Position.Point)
  else
    result := 0;
end;

function TControlHasTargetHelper.IsOverTarget: Boolean;
begin
  result := DistanceToTarget <= TargetTolerance;
end;

{$ENDREGION}

end.
