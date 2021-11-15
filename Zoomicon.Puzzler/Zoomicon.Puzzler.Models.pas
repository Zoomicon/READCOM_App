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

  IMultipleHasTarget = interface
    ['{8B845E1F-1C06-42CD-8657-38A869FE68BC}']

    {AllOverTarget}
    function AreAllOverTarget: Boolean;

    property AllOverTarget: Boolean read AreAllOverTarget;
  end;

implementation

end.
