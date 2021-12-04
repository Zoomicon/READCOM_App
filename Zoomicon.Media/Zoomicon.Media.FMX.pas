unit Zoomicon.Media.FMX;

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
    FVolumeBeforeMuting: Single; //0-1
    FMuted: Boolean;
    FAutoPlaying: Boolean;
    FLooping: Boolean;
    FTimer: TTimer;
    FLastTime: TMediaTime;
    FStream: TStream;
    {Events}
    FOnPlay: TOnPlay;
    FOnPause: TOnPause;
    FOnStop: TOnStop;
    FOnAtStart: TOnAtStart;
    FOnAtEnd: TOnAtEnd;
    FOnCurrentTimeChange: TCurrentTimeChange;

  //-- Methods

  protected
    procedure HandleTimer(Sender: TObject); virtual;
    procedure DoAtStart; virtual;
    procedure DoAtEnd; virtual;
    {TimerStarted}
    function IsTimerStarted: Boolean; virtual;
    procedure SetTimerStarted(const Value: Boolean); virtual;
    {MediaLoaded}
    function IsMediaLoaded: Boolean;
    {Playing}
    function IsPlaying: Boolean; virtual;
    procedure SetPlaying(const Value: Boolean); virtual;
    {AtStart}
    function IsAtStart: Boolean; virtual;
    {AtEnd}
    function IsAtEnd: Boolean; virtual;
    {Finished}
    function IsFinished: Boolean; virtual;
    {Muted}
    function IsMuted: Boolean; virtual;
    procedure SetMuted(const Value: Boolean); virtual;
    {AutoPlaying}
    function IsAutoPlaying: Boolean; virtual;
    procedure SetAutoPlaying(const Value: Boolean); virtual;
    {Looping}
    function IsLooping: Boolean; virtual;
    procedure SetLooping(const Value: Boolean); virtual;
    {Filename}
    function GetFilename: String;
    procedure SetFileName(const Value: string);
    {Stream}
    function GetStream: TStream;
    procedure SetStream(const Value: TStream);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Play; virtual;
    procedure Rewind; virtual;
    procedure Pause; virtual;
    procedure Stop; virtual;
    procedure Clear; virtual;

  //-- Properties

  protected
    property TimerStarted: Boolean read IsTimerStarted write SetTimerStarted;

  published
    property MediaLoaded: Boolean read IsMediaLoaded;
    property Playing: Boolean read IsPlaying write SetPlaying stored false;
    property AtStart: Boolean read IsAtStart;
    property AtEnd: Boolean read IsAtEnd;
    property Finished: Boolean read IsFinished;
    property Muted: Boolean read IsMuted write SetMuted;
    property AutoPlaying: Boolean read IsAutoPlaying write SetAutoPlaying;
    property Looping: Boolean read IsLooping write SetLooping;
    property FileName: string read GetFilename write SetFilename;
    property Stream: TStream read GetStream write SetStream stored false;
    {Events}
    property OnPlay: TOnPlay read FOnPlay write FOnPlay;
    property OnPause: TOnPause read FOnPause write FOnPause;
    property OnStop: TOnStop read FOnStop write FOnStop;
    property OnAtStart: TOnAtStart read FOnAtStart write FOnAtStart;
    property OnAtEnd: TOnAtEnd read FOnAtEnd write FOnAtEnd;
    property OnCurrentTimeChange: TCurrentTimeChange read FOnCurrentTimeChange write FOnCurrentTimeChange;
  end;

procedure Register;

implementation

uses
  System.IOUtils, //for TPath
  System.SysUtils; //for fmWrite

{$REGION 'TMediaPlayerEx'}

constructor TMediaPlayerEx.Create(AOwner: TComponent);
begin
  FTimer := TTimer.Create(self);
  FTimer.Interval := TIMER_INTERVAL;
  FTimer.OnTimer := HandleTimer;
  inherited;
end;

destructor TMediaPlayerEx.Destroy;
begin
  Stream := nil; //must do (calls SetStream) to free any temporary file we had created //do not free FStream, we hadn't created it
  inherited; //do last
end;

procedure TMediaPlayerEx.Play;
begin
  if not IsPlaying then
    begin
    TimerStarted := true; //start timer used to detect and send OnCurrentTimeChange event
    inherited Play;
    if Assigned(FOnPlay) then
      FOnPlay;
    end;
end;

procedure TMediaPlayerEx.Rewind;
begin
  CurrentTime := 0;
end;

procedure TMediaPlayerEx.Pause;
begin
  if IsPlaying then
    begin
    inherited Stop; //this Pauses
    TimerStarted := false; //stop timer used to detect and send OnCurrentTimeChange event (to conserve resources)
    if Assigned(FOnPause) then
      FOnPause;
    end;
end;

procedure TMediaPlayerEx.Stop;
begin
  Pause; //this will also call StopTimer;
  Rewind;
  if Assigned(FOnStop) then
    FOnStop;
end;

procedure TMediaPlayerEx.Clear;
begin
  Stop;
  inherited Clear; //this does FreeAndNil(Media) and (at the ancestor level) also Filename:=''
end;

procedure TMediaPlayerEx.HandleTimer;
begin
  if FAutoPlaying and MediaLoaded then //since TimerStarted=true we're either playing or waiting for media to load to autoplay it
    Play; //does nothing if Playing

  var newTime := CurrentTime; //keep locally since it changes
  if (FLastTime <> newTime) then
    begin
      FLastTime := newTime;
      if Assigned(OnCurrentTimeChange) then
        OnCurrentTimeChange(Self, newTime);

      if IsAtStart then
        DoAtStart;
      if IsAtEnd then //if media has Duration=0 (though for CurrentTime to have changed, other media would have to be loaded before) we'll fire OnAtStart and then OnAtEnd
        DoAtEnd;
    end;
end;

procedure TMediaPlayerEx.DoAtStart;
begin
  if Assigned(FOnAtStart) then
    FOnAtStart;
end;

procedure TMediaPlayerEx.DoAtEnd;
begin
  if Assigned(FOnAtEnd) then
    FOnAtEnd;
  if FLooping then
    CurrentTime := 0; //TODO: depending on Timer resolution this may fail to fire OnAtStart (but if we call DoAtStart it may fire twice)
end;

{$region 'TimerStarted'}

function TMediaPlayerEx.IsTimerStarted: Boolean;
begin
  result := FTimer.Enabled;
end;

procedure TMediaPlayerEx.SetTimerStarted(const Value: Boolean);
begin
  if Value then //Start timer
  begin
    if not IsTimerStarted then
    begin
      FLastTime := CurrentTime; //keep CurrentTime before starting the timer (do it before it gets started, since OnTimer event handler also updates FLastTime)
      FTimer.Enabled := true; //we know (Value = true)
    end
  else //Stop timer
    if IsTimerStarted then
      begin
      FTimer.Enabled := false; //we know (Value = false)
      FLastTime := CurrentTime; //keep CurrentTime after stopping the timer (do it after it gets stopped, since OnTimer event handler also updates FLastTime)
      end;
  end;
end;

{$endregion}

{$region 'MediaLoaded'}

function TMediaPlayerEx.IsMediaLoaded: Boolean;
begin
  result := (State <> TMediaState.Unavailable);
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

{$region 'Muted'}

function TMediaPlayerEx.IsMuted: Boolean;
begin
  result := FMuted;
end;

procedure TMediaPlayerEx.SetMuted(const Value: Boolean);
begin
  if Value and (not FMuted) then
    begin
      FVolumeBeforeMuting := Volume;
      Volume := 0;
      FMuted := Value;
    end
  else if (not Value) and FMuted then
    begin
    Volume := FVolumeBeforeMuting;
    FMuted := Value;
    end;
end;

{$endregion}

{$region 'AutoPlaying'}

function TMediaPlayerEx.IsAutoPlaying: Boolean; //IMediaPlayer implementation
begin
  result := FAutoPlaying;
end;

procedure TMediaPlayerEx.SetAutoPlaying(const Value: Boolean);
begin
  FAutoPlaying := Value;
  if Value and MediaLoaded then
    Play;        
end;

{$endregion}

{$region 'Looping'}

function TMediaPlayerEx.IsLooping: Boolean; //IMediaPlayer implementation
begin
  result := FLooping;
end;

procedure TMediaPlayerEx.SetLooping(const Value: Boolean);
begin
  FLooping := Value;
end;

{$endregion}

{$region 'Filename'}

function TMediaPlayerEx.GetFilename: String;
begin
  result := inherited Filename;
end;

procedure TMediaPlayerEx.SetFileName(const Value: string);
begin
  if (Value.IsEmpty) then
    Clear //this also calls Stop in our implementation (and via "inherited Clear" does FreeAndNil(Media) and [at the ancestor level] Filename:='')
  else
    begin
    if FAutoPlaying then
      TimerStarted := true; //HandleTimer also checks if media is loaded
    inherited Filename := Value;
    end;
end;

{$endregion}

{$region 'Stream'}

function TMediaPlayerEx.GetStream: TStream;
begin
  result := FStream;
end;

procedure TMediaPlayerEx.SetStream(const Value: TStream);
begin
  if Assigned(Value) then
  begin
    var TempFileName := TPath.GetTempFileName;
    var F := TFileStream.Create(TempFilename, fmOpenWrite);
    try
      F.CopyFrom(Value);
    finally
      FreeAndNil(F);
    end;
    Filename := TempFilename;
  end
  else
    begin
    if Assigned(Stream) then
      TFile.Delete(Filename); //delete temp file we had allocated for Stream
    FStream := nil; //do not free FStream, we hadn't created it
    Filename := ''; //this will call Clear
    end;
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
