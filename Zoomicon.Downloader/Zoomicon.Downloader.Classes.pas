unit Zoomicon.Downloader.Classes;

interface
  uses
    Zoomicon.Cache.Models, //for IContentCache
    Zoomicon.Downloader.Models, //for IDownloader
    System.Classes, //for TThread
    System.Net.URLClient, //for TURI
    System.SyncObjs; //for TEvent, TWaitResult

  var
    SleepTimeMS: integer = 1;

  type

    TDownloader = class;

    TDownloaderThread = class(TThread)
      protected
        FDownloader: TDownloader;

      public
        constructor Create(const TheDownloader: TDownloader);
        destructor Destroy; override;
        procedure Execute; override;
        function WaitForTermination(Timeout: cardinal = INFINITE): TWaitResult;
        procedure SetTerminationEvent;

        property Downloader: TDownloader read FDownloader write FDownloader;
    end;

    TDownloader = class(TComponent, IDownloader)
      protected
        FContentCache: IContentCache;
        FDownloaderThread: TDownloaderThread;
        FDownloaderThreadTerminationEvent: TEvent; //for other threads synchronization with downloader thread

        FLastSessionElapsedTime, FTotalElapsedTime, FSessionStartTime: Cardinal;
        FLastSessionReadCount, FTotalReadCount, FTotalContentLength, FStartPosition, FEndPosition: Int64;
        FShouldResume: Boolean;

        FResumable: Boolean;
        FPaused: Boolean;

        FHeaderAccept: String;

        FContentURIstr: String;
        FData: TStream;

        FOnDownloadProgress: TDownloadProgressEvent;
        FOnDownloadTerminated: TDownloadTerminationEvent;
        FOnDownloadComplete: TDownloadCompletionEvent;

        function GetContentURI: TURI;

        function Download(const StartPosition, EndPosition: Int64): integer; overload; virtual; //returns HTTP status code
        procedure ReceiveHandler(const Sender: TObject; ContentLength, ReadCount: Int64; var Abort: Boolean);
        function Execute: integer; virtual; //returns HTTP status code //called by TDownloaderThread

      public
        constructor Create(AOwner: TComponent; const TheContentURI: TURI; const TheData: TStream; const TheContentCache: IContentCache = nil; const AutoStart: Boolean = false; const TheStartPosition: Int64 = 0; const TheEndPosition: Int64 = 0); reintroduce;
        procedure Initialize(const TheContentURI: TURI; const TheData: TStream; const TheContentCache: IContentCache = nil; const AutoStart: Boolean = false; const TheStartPosition: Int64 = 0; const TheEndPosition: Int64 = 0);

        procedure Start; virtual;
        function WaitForDownload(Timeout: cardinal = INFINITE): TWaitResult;

        procedure SetPaused(const Value: Boolean);
        function IsTerminated: Boolean;

      published
        property ContentCache: IContentCache read FContentCache write FContentCache;
        property DownloaderThreadTerminationEvent: TEvent read FDownloaderThreadTerminationEvent;
        property Resumable: Boolean read FResumable write FResumable;
        property Paused: Boolean read FPaused write SetPaused;
        property Terminated: Boolean read IsTerminated;

        property HeaderAccept: String read FHeaderAccept write FHeaderAccept;
        //TODO: consider also adding Header properties for AcceptCharSet, AcceptEncoding, AcceptLanguage, ContentType, UserAgent

        property ContentURI: TURI read GetContentURI;
        property Data: TStream read FData;

        property TotalElapsedtime: Cardinal read FTotalElapsedTime;
        property TotalReadCount: Int64 read FTotalReadCount;
        property TotalContentLength: Int64 read FTotalContentLength;

        property OnDownloadProgress: TDownloadProgressEvent write FOnDownloadProgress;
        property OnDownloadTerminated: TDownloadTerminationEvent write FOnDownloadTerminated;
        property OnDownloadComplete: TDownloadCompletionEvent write FOnDownloadComplete;
    end;

    TFileDownloader = class(TDownloader, IFileDownloader)
      protected
        FFilepath: String;
        function Execute: integer; override; //returns HTTP status code //called by TDownloaderThread

      public
        constructor Create(const TheContentURI: TURI; const TheFilepath: String; const TheContentCache: IContentCache = nil; const AutoStart: Boolean = false; const TheStartPosition: Int64 = 0; const TheEndPosition: Int64 = 0); overload;
        destructor Destroy; override;

        procedure Initialize(const TheContentURI: TURI; const TheFilepath: String; const TheContentCache: IContentCache = nil; const AutoStart: Boolean = false; const TheStartPosition: Int64 = 0; const TheEndPosition: Int64 = 0); overload;
        procedure Start; override;

      published
        property Filepath: String read FFilepath write FFilepath;
    end;

implementation
uses
  {$ifdef WINDOWS}
  //Winapi.Windows, //for OutputDebugStr
  {$endif}
  System.IOUtils, //for TPath, TDirectory
  System.NetConsts, //to expand inline THTTPClient.SetAccept
  System.Net.HttpClient, //for THTTPClient
  System.SysUtils; //for fmOpenWrite, fmShareDenyNone

{$region 'TDownloaderThread'}

constructor TDownloaderThread.Create(const TheDownloader: TDownloader);
begin
  inherited Create(true); //Create suspended
  //FreeOnTerminate := false; //this is the default

  FDownloader := TheDownloader;
end;

destructor TDownloaderThread.Destroy;
begin
  SetTerminationEvent; //notify any threads waiting on our event object
  inherited; //do last
end;

procedure TDownloaderThread.Execute; //returns HTTP status code
begin
  try
    ReturnValue := Downloader.Execute; //return HTTP status code
  finally
    SetTerminationEvent; //notify any threads waiting on our event object
  end;
end;

function TDownloaderThread.WaitForTermination(Timeout: cardinal = INFINITE): TWaitResult;
begin
  if Assigned(FDownloader) then
    begin
    var TerminationEvent := FDownloader.DownloaderThreadTerminationEvent;
    if Assigned(TerminationEvent) then
      begin
      result := TerminationEvent.WaitFor(Timeout);
      exit;
      end;
    end;

  result := TWaitResult.wrAbandoned;
end;

procedure TDownloaderThread.SetTerminationEvent;
begin
  if Assigned(FDownloader) then
    begin
    var TerminationEvent := FDownloader.DownloaderThreadTerminationEvent;
    if Assigned(TerminationEvent) then
      TerminationEvent.SetEvent;
    end;
end;

{$endregion}

{$REGION 'TDownloader'}

constructor TDownloader.Create(AOwner: TComponent; const TheContentURI: TURI; const TheData: TStream; const TheContentCache: IContentCache = nil; const AutoStart: Boolean = false; const TheStartPosition: Int64 = 0; const TheEndPosition: Int64 = 0);
begin
  inherited Create(AOwner);
  Initialize(TheContentURI, TheData, TheContentCache, AutoStart, TheStartPosition, TheEndPosition);
end;

procedure TDownloader.Initialize(const TheContentURI: TURI; const TheData: TStream; const TheContentCache: IContentCache = nil; const AutoStart: Boolean = false; const TheStartPosition: Int64 = 0; const TheEndPosition: Int64 = 0);
begin
  FContentCache := TheContentCache;

  FDownloaderThread := TDownloaderThread.Create(self);
  FDownloaderThreadTerminationEvent := TEvent.Create();

  FContentURIstr := TheContentURI.ToString;
  FData := TheData;

  FStartPosition := TheStartPosition;
  FEndPosition := TheEndPosition;

  FLastSessionReadCount := TheStartPosition;

  if AutoStart then
    Start;
end;

function TDownloader.IsTerminated: Boolean;
begin
  result := Assigned(FDownloaderThread) and FDownloaderThread.CheckTerminated; //TODO: maybe should return true if not Assigned(FDownloaderThread)
end;

procedure TDownloader.Start;
begin
  if (not FDownloaderThread.Started) then
    FDownloaderThread.Start;
  Paused := false;
end;

function TDownloader.WaitForDownload(Timeout: cardinal): TWaitResult;
begin
  if Assigned(FDownloaderThread) then
    result := FDownloaderThread.WaitForTermination(Timeout)
  else
    result := TWaitResult.wrAbandoned;
end;

function TDownloader.Execute: integer; //returns HTTP status code //called by TDownloaderThread
begin
  //If requested URI is cached, return its data from there...
  if Assigned(FContentCache) then
    begin
    var CachedData := FContentCache.GetContent(FContentURIstr);
    if Assigned(CachedData) then
      begin
      FData.CopyFrom(CachedData); //copy from CachedData to output stream
      FreeAndNil(CachedData); //free CachedData handle
      result := STATUS_OK;
      exit;
      end;
    end;

  //Download...
  result := Download(FStartPosition, FEndPosition);

  while (not FDownloaderThread.CheckTerminated) and FResumable do
  begin
    if FShouldResume then
    begin
      FShouldResume := False;
      FPaused := False;
      result := Download(FLastSessionReadCount, FEndPosition);
      {$ifdef WINDOWS}
      //OutputDebugStr(IntToStr(result));
      {$endif}
    end;

    Sleep(SleepTimeMS); //sleep a bit, being polite to other threads
  end;

  //If data was downloaded succesfully, can cache downloaded data with URI as key...
  if Assigned(FContentCache) and (result = STATUS_OK) then
    FContentCache.PutContent(FContentURIstr, FData); //copies from start of stream
end;

function TDownloader.GetContentURI: TURI;
begin
  result := TURI.Create(FContentURIstr);
end;

procedure TDownloader.SetPaused(const Value: Boolean);
begin
  if Value then
    FPaused := True
  else
    FShouldResume := True;
end;

function TDownloader.Download(const StartPosition, EndPosition: Int64): Integer; //this is called by Execute which is called by TDownloaderThread
begin
  var HttpClient := THTTPClient.Create;
  HttpClient.Accept := FHeaderAccept;
  HttpClient.OnReceiveData := ReceiveHandler;

  var StatusCode: Integer := 0; //e.g. HTTP_OK=200

  try
    FSessionStartTime := FDownloaderThread.GetTickCount;

    if FEndPosition = 0 then
      StatusCode := HttpClient.Get(FContentURIstr, FData).StatusCode
    else
      begin
      FData.Seek(StartPosition, TSeekOrigin.soBeginning);
      StatusCode := HttpClient.GetRange(FContentURIstr, StartPosition, EndPosition, FData).StatusCode; //synchronous-blocking call (executes ReceiveHandler callback periodically)
      end;

    if Assigned(FOnDownloadTerminated) then
      FOnDownloadTerminated(Self, StatusCode);

    if Assigned(FOnDownloadComplete) and (StatusCode = STATUS_OK) then
      FOnDownloadComplete(Self, FData);

  finally
    HttpClient.Free;
    result := StatusCode;
    FDownloaderThread.Terminate; //note that this method is running on that thread (called via its Execute method which calls our Execute)
  end;
end;

{ See: https://docwiki.embarcadero.com/Libraries/Sydney/en/System.Net.HttpClient.THTTPClient.OnReceiveData
    Occurs one or more times while your HTTP client receives response data for one or more requests,
    and it indicates the current progress of the response download for the specified request.
    The event handler of OnReceiveData receives the following parameters:
    - Sender is the HTTP request that triggered the response.
    - ContentLength is the expected length of the response, in number of bytes.
    - ReadCount is the length of the response data that has been downloaded so far, in number of bytes.
    - Abort is an incoming variable parameter the event handler can set to True to abort data reception.
}
procedure TDownloader.ReceiveHandler(const Sender: TObject; ContentLength, ReadCount: Int64; var Abort: Boolean);
begin
  var SessionTime := FDownloaderThread.GetTickCount - FSessionStartTime; //in msec

  FTotalElapsedTime := FLastSessionElapsedTime + SessionTime;
  FTotalReadCount := FLastSessionReadCount + ReadCount;
  FTotalContentLength := FLastSessionReadCount + ContentLength;

  //First send download progress...
  if Assigned(FOnDownloadProgress) then
    begin
    var DownloadSpeed: Integer;
    if SessionTime = 0 then
      DownloadSpeed := 0
    else
      DownloadSpeed := (ReadCount * 1000) div SessionTime; //doing *1000 since ElapsedTime is in msec

    FOnDownloadProgress(Self, FTotalElapsedTime, DownloadSpeed, FTotalReadCount, FTotalContentLength, Abort);
    end;

  //...then abort any further downloading if needed
  if FDownloaderThread.CheckTerminated or FPaused then
    begin
    FLastSessionElapsedTime := FTotalElapsedTime;
    FLastSessionReadCount := FTotalReadCount;
    Abort := true;
    end;
end;

{$endregion}

{$region 'TFileDownloader'}

constructor TFileDownloader.Create(const TheContentURI: TURI; const TheFilePath: String; const TheContentCache: IContentCache = nil; const AutoStart: Boolean = false; const TheStartPosition: Int64 = 0; const TheEndPosition: Int64 = 0);
begin
  Initialize(TheContentURI, TheFilePath, TheContentCache, AutoStart, TheStartPosition, TheEndPosition);
end;

destructor TFileDownloader.Destroy;
begin
  if Assigned(FDownloaderThread) then
    begin
    FreeAndNil(FDownloaderThread);

    if FDownloaderThread.ReturnValue <> STATUS_OK then //If failed to download, delete partially downloaded file //TODO: maybe only do it when non-resumable?
      TFile.Delete(Filepath); //Note: must first do FreeAndNil to release handle to file, then delete it
    end;

  FreeAndNil(FData); //when it's a TFileDownloader we weren't given a data stream, we created it, so have to free it

  inherited; //do last
end;

procedure TFileDownloader.Initialize(const TheContentURI: TURI; const TheFilepath: string; const TheContentCache: IContentCache = nil; const AutoStart: Boolean = false; const TheStartPosition: Int64 = 0; const TheEndPosition: Int64 = 0);
begin
  Filepath := TheFilepath; //this will call SetFilepath (don't just assign FFilePath)
  Initialize(TheContentURI, FData, TheContentCache, AutoStart, TheStartPosition, TheEndPosition);
end;

procedure TFileDownloader.Start;
begin
  //only create the output file when download is to be started
  TDirectory.CreateDirectory(ExtractFileDir(FFilepath)); //create any missing subdirectories
  FData := TFileStream.Create(FFilepath, fmCreate {or fmShareDenyNone}); //overwrite any existing file //TODO: fmShareDenyNote probably needed for Android
  inherited;
end;

function TFileDownloader.Execute: integer; //returns HTTP status code //called by TDownloaderThread
begin
  result := 0;
  try
    result := inherited;
  finally
    FreeAndNil(FData); //when it's a TFileDownloader we weren't given a data stream, we created it, so have to free it
    if result <> STATUS_OK then //If failed to download, delete partially downloaded file //TODO: maybe only do it when non-resumable?
      TFile.Delete(Filepath); //Note: must first do FreeAndNil to release handle to file, then delete it
  end;
end;

{$endregion}

end.
