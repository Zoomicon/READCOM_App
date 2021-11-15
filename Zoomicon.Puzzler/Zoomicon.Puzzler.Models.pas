unit Zoomicon.Puzzler.Models;

interface
  uses
    FMX.Controls; //for TControls

type

  IShuffler = interface
    ['{CEB6CC35-6FCA-4C8C-8F4F-CD2CA832A130}']
    procedure ShuffleItems;
  end;

  IHasTarget = interface
    ['{54AB2AEC-5158-44B7-904B-00E85E0F169A}']

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

implementation

end.
