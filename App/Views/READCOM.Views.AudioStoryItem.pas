//Description: READ-COM AudioStoryItem View
//Author: George Birbilis (http://zoomicon.com)

unit READCOM.Views.AudioStoryItem;

interface

uses
  Zoomicon.Media.FMX, //for TMediaPlayerEx
  READCOM.App.Models, //for TStoryItem, IAudioStoryItem, IStoryItem
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  READCOM.Views.StoryItem, FMX.ExtCtrls, FMX.Objects,
  FMX.SVGIconImage, FMX.Media;

const
  EXT_MP3 = '.mp3';
  FILTER_MP3 = 'MP3 audio (*.mp3)|*.mp3';

type
  TAudioStoryItem = class(TStoryItem , IAudioStoryItem, IStoryItem, IStoreable)
    GlyphImage: TSVGIconImage;
    MediaPlayer: TMediaPlayerEx;
    procedure FrameTap(Sender: TObject; const Point: TPointF);
    procedure FrameClick(Sender: TObject);

  //--- Methods ---
  public
    constructor Create(AOwner: TComponent); override;

    {$region 'IStoreable'}
    function GetLoadFilesFilter: String; override;
    procedure Load(const Stream: TStream; const ContentFormat: String = EXT_READCOM); overload; override;
    procedure Load(const Filepath: String); overload; override;
    procedure LoadMP3(const Stream: TStream); virtual;
    {$endregion}

    {$region 'IPlayable'}
    procedure Play;
    procedure Pause;
    procedure Stop;
    {$endregion}

  protected
    { Muted }
    function IsMuted: Boolean;
    procedure SetMuted(const Value: Boolean);

    { AutoPlaying }
    function IsAutoPlaying: Boolean;
    procedure SetAutoPlaying(const Value: Boolean);

    { Looping }
    function IsLooping: Boolean;
    procedure SetLooping(const Value: Boolean);

    { PlayOnce }
    function IsPlayOnce: Boolean;
    procedure SetPlayOnce(const Value: Boolean);

    { Audio }
    function GetAudio: TMediaPlayerEx;
    procedure SetAudio(const Value: TMediaPlayerEx);

  //--- Properties ---

  published
    property Muted: Boolean read IsMuted write SetMuted stored false; //shouldn't store this so that interacting with the story won't store it disabled
    property AutoPlaying: Boolean read IsAutoPlaying write SetAutoPlaying;
    property PlayOnce: Boolean read IsPlayOnce write SetPlayOnce;
    property Audio: TMediaPlayerEx read GetAudio write SetAudio stored false;
    //TODO: persist the audio data
  end;

  procedure Register;

implementation

{$R *.fmx}

{ TAudioStoryItem }

constructor TAudioStoryItem.Create(AOwner: TComponent);
begin
  inherited;
  MediaPlayer.Stored := false; //don't store state, should use state from designed .FMX resource
  GlyphImage.Stored := false; //don't store state, should use state from designed .FMX resource
end;

{$region 'IStoreable'}

function TAudioStoryItem.GetLoadFilesFilter: String;
begin
  result := FILTER_MP3;
end;

procedure TAudioStoryItem.Load(const Stream: TStream; const ContentFormat: String = EXT_READCOM);
begin
  if (ContentFormat = EXT_MP3) then //load EXT_MP3
    LoadMP3(Stream)
  else
    inherited; //load EXT_READCOM
end;

procedure TAudioStoryItem.Load(const Filepath: String);
begin
  var FileExt := ExtractFileExt(Filepath);
  if (FileExt = EXT_MP3) then
    MediaPlayer.Filename := Filepath
  else
    raise EFeatureNotSupported.CreateFmt('AudioStoryItem: loading %s files not supported', [FileExt]);
end;

procedure TAudioStoryItem.LoadMP3(const Stream: TStream);
begin
  raise EFeatureNotSupported.Create('AudioStoryItem: loading from Stream not supported');
end;

{$endregion}

{$region 'IPlayable'}

procedure TAudioStoryItem.Play;
begin
  MediaPlayer.Play; //TODO: if Disabled don't play (play random child?)
end;

procedure TAudioStoryItem.Pause;
begin
  MediaPlayer.Stop; //this Pauses
end;

procedure TAudioStoryItem.Stop;
begin
  MediaPlayer.Stop; //this only Pauses...
  MediaPlayer.CurrentTime := 0; //...so we also reset CurrentTime to 0
end;

{$endregion}

{$region 'Muted'}

function TAudioStoryItem.IsMuted: Boolean;
begin
  result := MediaPlayer.Muted;
end;

procedure TAudioStoryItem.SetMuted(const Value: Boolean); //TODO: should rename this in interface (call it enabled) and Stop the MediaPlayer instead of muting the audio
begin
  MediaPlayer.Muted := Value;
end;

{$endregion}

{$region 'AutoPlay'}

function TAudioStoryItem.IsAutoPlaying: Boolean;
begin
  result := MediaPlayer.AutoPlaying;
end;

procedure TAudioStoryItem.SetAutoPlaying(const Value: Boolean);
begin
  MediaPlayer.AutoPlaying := Value;
end;

{$endregion}

{$region 'Looping'}

function TAudioStoryItem.IsLooping: Boolean;
begin
  result := MediaPlayer.Looping;
end;

procedure TAudioStoryItem.SetLooping(const Value: Boolean);
begin
  MediaPlayer.Looping := Value;
end;

{$endregion}

{$region 'PlayOnce'}

function TAudioStoryItem.IsPlayOnce: Boolean;
begin
  //TODO
end;

procedure TAudioStoryItem.SetPlayOnce(const Value: Boolean);
begin

end;

{$endregion}

{$region 'Audio'}

function TAudioStoryItem.GetAudio: TMediaPlayerEx;
begin
  result := MediaPlayer;
end;

procedure TAudioStoryItem.SetAudio(const Value: TMediaPlayerEx);
begin
  MediaPlayer.FileName := Value.FileName;
end;

{$endregion}

{$REGION 'EVENTS'}

procedure TAudioStoryItem.FrameClick(Sender: TObject);
begin
  inherited;
  Play;
end;

procedure TAudioStoryItem.FrameTap(Sender: TObject; const Point: TPointF);
begin
  inherited;
  Play;
end;

{$ENDREGION}

procedure RegisterClasses;
begin
  RegisterFmxClasses([TAudioStoryItem]); //register for persistence
end;

procedure Register;
begin
  GroupDescendentsWith(TAudioStoryItem, TControl);
  RegisterClasses;
  RegisterComponents('Zoomicon', [TAudioStoryItem]);
end;

initialization
  RegisterClasses; //don't call Register here, it's called by the IDE automatically on a package installation (fails at runtime)

end.

