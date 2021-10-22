unit Zoomicon.Media.Classes;

interface

uses
  Zoomicon.Media.Models, //for IMediaPlayer
  FMX.Media, //for TMediaPlayer
  FMX.Types, //for TTimer, RegisterFmxClasses
  System.Classes; //for GroupDescendentsWith, RegisterComponents

const
  TIMER_INTERVAL = 10; //ms

type
  TMediaPlayerEx = class(TMediaPlayer, IMediaPlayer)
  //-- Fields

  protected
    FAutoPlaying: Boolean;
    FLooping: Boolean;
    FTimer: TTimer;

  //-- Methods

  protected
    {Playing}
    function IsPlaying: Boolean; virtual;
    procedure SetPlaying(const Value: Boolean); virtual;
    {AtStart}
    function IsAtStart: Boolean;
    {AtEnd}
    function IsAtEnd: Boolean;
    {Finished}
    function IsFinished: Boolean;
    {AutoPlaying}
    function IsAutoPlaying: Boolean;
    procedure SetAutoPlaying(const Value: Boolean); //TODO: override media loading to do autoplay (check state if media is available)
    {Looping}
    function IsLooping: Boolean; virtual;
    procedure SetLooping(const Value: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Play;
    procedure Rewind;
    procedure Pause; virtual;
    procedure Stop;

  //-- Properties

  published
    property Playing: Boolean read IsPlaying write SetPlaying stored false;
    property AtStart: Boolean read IsAtStart;
    property AtEnd: Boolean read IsAtEnd;
    property Finished: Boolean read IsFinished;
    property AutoPlaying: Boolean read IsAutoPlaying write SetAutoPlaying;
    property Looping: Boolean read IsLooping write SetLooping;
  end;

procedure Register;

implementation

{$REGION 'TMediaPlayerEx'}

constructor TMediaPlayerEx.Create(AOwner: TComponent);
begin
  FTimer := TTimer.Create(self);
  FTimer.Interval := TIMER_INTERVAL;
  //FTimer.OnTimer := //TODO: set up timer to send even when Finished and to do Looping
  inherited;
end;

procedure TMediaPlayerEx.Play;
begin
  if not IsPlaying then
    inherited Play;
end;

procedure TMediaPlayerEx.Rewind;
begin
  CurrentTime := 0;
end;

procedure TMediaPlayerEx.Pause;
begin
  if IsPlaying then
    inherited Stop; //this Pauses
end;

procedure TMediaPlayerEx.Stop;
begin
  Pause;
  Rewind;
end;

{$region 'Playing'}

function TMediaPlayerEx.IsPlaying: Boolean;
begin
  result := (State = TMediaState.Playing);
end;

procedure TMediaPlayerEx.SetPlaying(const Value: Boolean);
begin
  if Value then
    Play
  else
    Pause;
end;

{$endregion}

{$region 'AtStart'}

function TMediaPlayerEx.IsAtStart: Boolean;
begin
  result := (CurrentTime = 0);
end;

{$endregion}

{$region 'AtEnd'}

function TMediaPlayerEx.IsAtEnd: Boolean;
begin
  result := (CurrentTime = Duration);
end;

{$endregion}

{$region 'Finished'}

function TMediaPlayerEx.IsFinished: Boolean;
begin
  result := (not Playing) and AtEnd;
end;

{$endregion}

{$region 'AutoPlaying'}

function TMediaPlayerEx.IsAutoPlaying: Boolean;
begin
  result := FAutoPlaying;
end;

procedure TMediaPlayerEx.SetAutoPlaying(const Value: Boolean);
begin
  FAutoPlaying := Value;
end;

{$endregion}

{$region 'Looping'}

function TMediaPlayerEx.IsLooping: Boolean;
begin
  result := FLooping;
end;

procedure TMediaPlayerEx.SetLooping(const Value: Boolean);
begin
  FLooping := Value;
end;

{$endregion}

{$ENDREGION}

procedure RegisterClasses;
begin
  RegisterFmxClasses([TMediaPlayerEx]); //register for persistence
end;

procedure Register;
begin
  GroupDescendentsWith(TMediaPlayerEx, TComponent);
  RegisterClasses;
  RegisterComponents('Zoomicon', [TMediaPlayerEx]);
end;

initialization
  RegisterClasses; //don't call Register here, it's called by the IDE automatically on a package installation (fails at runtime)

end.
