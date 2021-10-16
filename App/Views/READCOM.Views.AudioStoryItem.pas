unit READCOM.Views.AudioStoryItem;

interface

uses
  READCOM.App.Models, //for TStoryItem, IAudioStoryItem, IStoryItem
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  READCOM.Views.StoryItem, Zoomicon.Manipulator, FMX.ExtCtrls, FMX.Objects,
  FMX.SVGIconImage, FMX.Media;

const
  EXT_MP3 = '.mp3';
  FILTER_MP3 = 'MP3 audio (*.mp3)|*.mp3';

type
  TAudioStoryItem = class(TStoryItem , IAudioStoryItem, IStoryItem, IStoreable)
    MediaPlayer: TMediaPlayer;
    GlyphImage: TSVGIconImage;

  //--- Methods ---
  public
   {$region 'IStoreable'}
    function GetLoadFilesFilter: String; override;
    procedure Load(const Stream: TStream; const ContentFormat: String = EXT_READCOM); overload; override;
    procedure Load(const Filepath: String); overload; override;
    procedure LoadMP3(const Stream: TStream); virtual;
    {$endregion}

    procedure Play;

  protected
    { Muted }
    function IsMuted: Boolean;
    procedure SetMuted(const Value: Boolean);

    { AutoPlay }
    function IsAutoPlay: Boolean;
    procedure SetAutoPlay(const Value: Boolean);

    { Looping }
    function IsLooping: Boolean;
    procedure SetLooping(const Value: Boolean);

    { PlayOnce }
    function IsPlayOnce: Boolean;
    procedure SetPlayOnce(const Value: Boolean);

    { Audio }
    function GetAudio: TMediaPlayer;
    procedure SetAudio(const Value: TMediaPlayer);

  //--- Properties ---

  published
    property Muted: Boolean read IsMuted write SetMuted;
    property AutoPlay: Boolean read IsAutoPlay write SetAutoPlay;
    property PlayOnce: Boolean read IsPlayOnce write SetPlayOnce;
    property Audio: TMediaPlayer read GetAudio write SetAudio; //stored false
  end;

implementation

{$R *.fmx}

{ TAudioStoryItem }

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

procedure TAudioStoryItem.Play;
begin
  MediaPlayer.Play;
end;

{$region 'Muted'}

function TAudioStoryItem.IsMuted: Boolean;
begin
  result := (MediaPlayer.Volume = 0);
end;

procedure TAudioStoryItem.SetMuted(const Value: Boolean);
begin
  var vol := 1;
  if Value then vol := 0; //Mute
  MediaPlayer.Volume := vol;
end;

{$endregion}

{$region 'AutoPlay'}

function TAudioStoryItem.IsAutoPlay: Boolean;
begin

end;

procedure TAudioStoryItem.SetAutoPlay(const Value: Boolean);
begin

end;

{$endregion}

{$region 'Looping'}

function TAudioStoryItem.IsLooping: Boolean;
begin

end;

procedure TAudioStoryItem.SetLooping(const Value: Boolean);
begin

end;

{$endregion}

{$region 'PlayOnce'}

function TAudioStoryItem.IsPlayOnce: Boolean;
begin

end;

procedure TAudioStoryItem.SetPlayOnce(const Value: Boolean);
begin

end;

{$endregion}

{$region 'Audio'}

function TAudioStoryItem.GetAudio: TMediaPlayer;
begin
  result := MediaPlayer;
end;

procedure TAudioStoryItem.SetAudio(const Value: TMediaPlayer);
begin
  MediaPlayer.Assign(Value);
end;

{$endregion}

end.
