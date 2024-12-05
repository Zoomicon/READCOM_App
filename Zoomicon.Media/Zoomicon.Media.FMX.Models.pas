unit Zoomicon.Media.FMX.Models;

interface
  uses
    FMX.Media, //for TMediaTime
    System.Classes; //for TStream

  {$REGION 'IMediaPlayer'}

  type
    TOnPlay = procedure of object;
    TOnPause = procedure of object;
    TOnStop = procedure of object;
    TOnAtStart = procedure of object;
    TOnAtEnd = procedure of object;
    TCurrentTimeChange = procedure(Sender: TObject; const NewTime: TMediaTime) of object;

    IMediaPlayer = interface
      ['{5D5F02BF-8066-49C0-B552-2B127DC8A6AD}']
      //-- Methods
      procedure Play;
      procedure Rewind;
      procedure Pause;
      procedure Stop;
      {MediaLoaded}
      function IsMediaLoaded: Boolean;
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
      {Filename}
      function GetFilename: String;
      procedure SetFilename(const Value: String);
      {Stream}
      function GetStream: TStream;
      procedure SetStream(const Value: TStream);

      //-- Properties
      property MediaLoaded: Boolean read IsMediaLoaded; //stored false
      property Playing: Boolean read IsPlaying write SetPlaying; //stored false
      property AtStart: Boolean read IsAtStart;
      property AtEnd: Boolean read IsAtEnd;
      property Finished: Boolean read IsFinished;
      property AutoPlaying: Boolean read IsAutoPlaying write SetAutoPlaying;
      property Looping: Boolean read IsLooping write SetLooping;
      property Filename: String read GetFilename write SetFilename;
      property Stream: TStream read GetStream write SetStream; //stored false
    end;

  {$ENDREGION}

  {$REGION 'IMediaDisplay'}

  const
    {$IF DEFINED(MSWINDOWS)}
    EXT_BMP = '.bmp';
    {$ENDIF}
    EXT_SVG = '.svg';
    EXT_PNG = '.png';
    EXT_JPG = '.jpg';
    EXT_JPEG = '.jpeg';
    //TODO: add more using Skia4Delphi

    SVG_BLANK = '<svg xmlns="http://www.w3.org/2000/svg"></svg>';

  type
    IMediaDisplay = interface
      procedure Load(const Stream: TStream; const ContentFormat: String);
      //
      procedure LoadBitmap(const Stream: TStream; const ContentFormat: String);
      procedure LoadSVG(const Stream: TStream);
      //TODO: add more using Skia4Delphi
    end;

  {$ENDREGION}

implementation

end.
