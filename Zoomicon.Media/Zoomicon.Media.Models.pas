unit Zoomicon.Media.Models;

interface

type
  IMediaPlayer = interface
    ['{5D5F02BF-8066-49C0-B552-2B127DC8A6AD}']
    //-- Methods
    procedure Play;
    procedure Rewind;
    procedure Pause;
    procedure Stop;
    {Playing}
    function IsPlaying: Boolean;
    procedure SetPlaying(const Value: Boolean);
    {AtStart}
    function IsAtStart: Boolean;
    {AtEnd}
    function IsAtEnd: Boolean;
    {Finished}
    function IsFinished: Boolean;
    {AutoPlaying}
    function IsAutoPlaying: Boolean;
    procedure SetAutoPlaying(const Value: Boolean);
    {Looping}
    function IsLooping: Boolean;
    procedure SetLooping(const Value: Boolean);

    //-- Properties
    property Playing: Boolean read IsPlaying write SetPlaying; //stored false
    property AtStart: Boolean read IsAtStart;
    property AtEnd: Boolean read IsAtEnd;
    property Finished: Boolean read IsFinished;
    property AutoPlaying: Boolean read IsAutoPlaying write SetAutoPlaying;
    property Looping: Boolean read IsLooping write SetLooping;
  end;

implementation

end.
