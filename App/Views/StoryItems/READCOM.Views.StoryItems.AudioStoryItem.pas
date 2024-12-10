//Description: READ-COM AudioStoryItem View
//Author: George Birbilis (http://zoomicon.com)

unit READCOM.Views.StoryItems.AudioStoryItem;

interface
  {$region 'Used units'}
  uses
    System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
    FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
    FMX.ExtCtrls, FMX.Objects,
    FMX.Layouts,
    FMX.Media,
    //
    Zoomicon.Media.FMX.Models, //for IMediaPlayer
    Zoomicon.Media.FMX.MediaDisplay, //for TMediaDisplay (Glyph)
    Zoomicon.Media.FMX.MediaPlayerEx, //for TMediaPlayerEx
    //
    READCOM.Models, //for IClipboardEnabled, IStoreable, EXT_READCOM
    READCOM.Models.Stories, //for IStoryItem, IAudioStoryItem
    READCOM.Views.StoryItems.StoryItem; //for TStoryItem
  {$endregion}

  const
    EXT_MP3 = '.mp3';
    FILTER_AUDIO_TITLE = 'Audio (*.mp3)';
    FILTER_AUDIO_EXTS = '*' + EXT_MP3;
    FILTER_AUDIO = FILTER_AUDIO_TITLE + '|' + FILTER_AUDIO_EXTS;

  type

    {$REGION 'TAudioStoryItem' --------------------------------------------------------}

    TAudioStoryItem = class(TStoryItem, IAudioStoryItem, IStoryItem, IClipboardEnabled, IStoreable)
      MediaPlayer: TMediaPlayerEx;

    //--- Methods ---
    public
      constructor Create(AOwner: TComponent); override;

      {$region 'IStoreable'}
      function GetLoadFilesFilter: String; override;
      function Load(const Stream: TStream; const ContentFormat: String = EXT_READCOM; const CreateNew: Boolean = false): TObject; overload; override;
      function Load(const Filepath: String; const CreateNew: Boolean = false): TObject; overload; override;
      function LoadMP3(const Stream: TStream): TObject; virtual;
      {$endregion}

      {$region 'IPlayable'}
      procedure Play;
      procedure Pause;
      procedure Stop;
      {$endregion}

    protected
      FPlayedOnce: Boolean;
      FIsPlayOnce: Boolean;

      {DefaultSize}
      function GetDefaultSize: TSizeF; override;

      {Active}
      procedure SetActive(const Value: Boolean); override;

      {EditMode}
      procedure SetEditMode(const Value: Boolean); override;

      {Muted}
      function IsMuted: Boolean;
      procedure SetMuted(const Value: Boolean);

      {AutoPlaying}
      function IsAutoPlaying: Boolean;
      procedure SetAutoPlaying(const Value: Boolean);

      {Looping}
      function IsLooping: Boolean;
      procedure SetLooping(const Value: Boolean);

      {PlayOnce}
      function IsPlayOnce: Boolean;
      procedure SetPlayOnce(const Value: Boolean);

      {Audio}
      function GetAudio: IMediaPlayer;
      procedure SetAudio(const Value: IMediaPlayer);

    //--- Events ---

    protected
      procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override; //preferring overriden methods instead of event handlers that get stored with saved state
      procedure Tap(const Point:TPointF); override;

    //--- Properties ---

    published
      property Hidden default true; //set to true in overriden constructor
      property Muted: Boolean read IsMuted write SetMuted stored false; //shouldn't store this so that interacting with the story won't store it disabled
      property AutoPlaying: Boolean read IsAutoPlaying write SetAutoPlaying;
      property PlayOnce: Boolean read IsPlayOnce write SetPlayOnce;
      property Audio: IMediaPlayer read GetAudio write SetAudio stored false; //TODO: make the MediaPlayerEx a subcomponent and persist its content together with the AudioStoryItem's so that audio data are also persisted)
    end;

    {$ENDREGION .......................................................................}

    {$REGION 'TAudioStoryItemFactory' -------------------------------------------------}

    TAudioStoryItemFactory = class(TInterfacedObject, IStoryItemFactory)
      function New(const AOwner: TComponent = nil): IStoryItem;
    end;

    {$ENDREGION .......................................................................}

    procedure Register;

implementation
  uses
    READCOM.Views.StoryItems.StoryItemFactory; //for StoryItemFactories, StoryItemAddFileFilter

  {$R *.fmx}

  {$REGION 'TAudioStoryItem'}

  constructor TAudioStoryItem.Create(AOwner: TComponent);
  begin
    inherited;

    FPlayedOnce := false;

    with MediaPlayer do
    begin
      Stored := false; //don't store state, should use state from designed .FMX resource
      SetSubComponent(true);
      //Note: don't try to set HitTest and Visible here, this is not a visible control, doesn't have such properties (would act upon the form instead)
    end;

    Hidden := true;
  end;

  {$region 'IStoreable'}

  function TAudioStoryItem.GetLoadFilesFilter: String;
  begin
    result := FILTER_AUDIO + '|' + inherited;
  end;

  function TAudioStoryItem.Load(const Stream: TStream; const ContentFormat: String = EXT_READCOM; const CreateNew: Boolean = false): TObject;
  begin
    if (ContentFormat = EXT_MP3) then //load EXT_MP3
      result := LoadMP3(Stream)
    else
      result := inherited; //load EXT_READCOM
  end;

  function TAudioStoryItem.Load(const Filepath: String; const CreateNew: Boolean = false): TObject;
  begin
    var FileExt := ExtractFileExt(Filepath);
    if (FileExt = EXT_MP3) then
    begin
      MediaPlayer.Filename := Filepath;
      result := Self;
    end
    else
      result := inherited; //load EXT_READCOM
  end;

  function TAudioStoryItem.LoadMP3(const Stream: TStream): TObject; //TODO: do we need to have this to persist audio in .readcom files?
  begin
    MediaPlayer.Stream := Stream;
    result := Self;
  end;

  {$endregion}

  {$region 'IPlayable'}

  procedure TAudioStoryItem.Play;
  begin
    if (not FIsPlayOnce) or (not FPlayedOnce) then //equivalent to "if not (FIsPlayOnce and FPlayedOnce)"
      if Assigned(MediaPlayer) then
      begin
        MediaPlayer.Play; //TODO: if Disabled don't play (play random child?)
        FPlayedOnce := true;
      end;
  end;

  procedure TAudioStoryItem.Pause;
  begin
    if Assigned(MediaPlayer) then
      MediaPlayer.Stop; //this Pauses
  end;

  procedure TAudioStoryItem.Stop;
  begin
    if Assigned(MediaPlayer) then
    begin
      MediaPlayer.Stop; //this only Pauses...
      MediaPlayer.CurrentTime := 0; //...so we also reset CurrentTime to 0
    end;
  end;

  {$endregion}

  {$REGION 'PROPERTIES' ---------------------}

  {$region 'DefaultSize'}

  function TAudioStoryItem.GetDefaultSize: TSizeF;
  begin
    Result := TSizeF.Create(64, 64);
  end;

  {$endregion}

  {$region 'Active'}

  procedure TAudioStoryItem.SetActive(const Value: Boolean);
  begin
    inherited;
    Play;
  end;

  {$region 'EditMode'}

  procedure TAudioStoryItem.SetEditMode(const Value: Boolean);
  begin
    inherited;
    Hidden := not (EditMode or (Assigned(ParentStoryItem) and ParentStoryItem.EditMode)); //TODO: need to hide/unhide the Glyph when the item is in EditMode or when its parent is in EditMode (Hidden is reapplied in that case - see StoryItem code [should see though how the parent now makes the child appear in editmode to remove that extra code and also should tell the sturcureview to show even non-visible storyitems])
  end;

  {$endregion}

  {$region 'Muted'}

  function TAudioStoryItem.IsMuted: Boolean;
  begin
    if Assigned(MediaPlayer) then
      result := MediaPlayer.Muted
    else
      result := true;
  end;

  procedure TAudioStoryItem.SetMuted(const Value: Boolean); //TODO: should rename this in interface (call it enabled) and Stop the MediaPlayer instead of muting the audio
  begin
    if Assigned(MediaPlayer) then
      MediaPlayer.Muted := Value;
  end;

  {$endregion}

  {$region 'AutoPlay'}

  function TAudioStoryItem.IsAutoPlaying: Boolean;
  begin
    if Assigned(MediaPlayer) then
      result := MediaPlayer.AutoPlaying
    else
      result := false;
  end;

  procedure TAudioStoryItem.SetAutoPlaying(const Value: Boolean);
  begin
    if Assigned(MediaPlayer) then
      MediaPlayer.AutoPlaying := Value;
  end;

  {$endregion}

  {$region 'Looping'}

  function TAudioStoryItem.IsLooping: Boolean;
  begin
    if Assigned(MediaPlayer) then
      result := MediaPlayer.Looping
    else
      result := false;
  end;

  procedure TAudioStoryItem.SetLooping(const Value: Boolean);
  begin
    if Assigned(MediaPlayer) then
      MediaPlayer.Looping := Value;
  end;

  {$endregion}

  {$region 'PlayOnce'}

  function TAudioStoryItem.IsPlayOnce: Boolean;
  begin
    result := FIsPlayOnce;
  end;

  procedure TAudioStoryItem.SetPlayOnce(const Value: Boolean);
  begin
    FIsPlayOnce := Value;
    FPlayedOnce := false; //reset the FPlayedOnce gate field
  end;

  {$endregion}

  {$region 'Audio'}

  function TAudioStoryItem.GetAudio: IMediaPlayer;
  begin
    result := MediaPlayer;
  end;

  procedure TAudioStoryItem.SetAudio(const Value: IMediaPlayer);
  begin
    if Assigned(MediaPlayer) then
      MediaPlayer.FileName := Value.FileName;
  end;

  {$endregion}

  {$ENDREGION ...............................}

  {$REGION 'EVENTS'}

  procedure TAudioStoryItem.MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  begin
    inherited;
    Play;
  end;

  procedure TAudioStoryItem.Tap(const Point:TPointF);
  begin
    inherited;
    Play;
  end;

  {$ENDREGION}

  {$ENDREGION}

  {$REGION 'TAudioStoryItemFactory'}

  function TAudioStoryItemFactory.New(const AOwner: TComponent = nil): IStoryItem;
  begin
    result := TAudioStoryItem.Create(AOwner);
  end;

  {$ENDREGION}

  {$region 'Registration'}

  procedure RegisterSerializationClasses;
  begin
    RegisterFmxClasses([TAudioStoryItem]);
  end;

  procedure Register;
  begin
    GroupDescendentsWith(TAudioStoryItem, TControl);
    RegisterSerializationClasses;
    RegisterComponents('Zoomicon', [TAudioStoryItem]);
  end;

  {$endregion}

initialization
  StoryItemFactories.Add([EXT_MP3], TAudioStoryItemFactory.Create);
  AddStoryItemFileFilter(FILTER_AUDIO_TITLE, FILTER_AUDIO_EXTS);

  RegisterSerializationClasses; //don't call Register here, it's called by the IDE automatically on a package installation (fails at runtime)

end.

