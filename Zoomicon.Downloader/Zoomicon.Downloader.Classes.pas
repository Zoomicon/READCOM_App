unit Zoomicon.Downloader.Classes;

//TODO: add capability to download to TMemoryStream?

interface
  uses
    Zoomicon.Downloader.Models, //for IDownloader
    System.Classes, //for TThread
    System.Net.URLClient; //for TURI

  var
    SleepTimeMS: integer = 1;

  type

    TDownloader = class;

    TDownloaderThread = class(TThread)
      protected
        FDownloader: TDownloader;
      public
        constructor Create(const TheDownloader: TDownloader);
        procedure Execute; override;
      published
       property Downloader: TDownloader read FDownloader write FDownloader;
    end;

    TDownloader = class(TInterfacedObject, IDownloader)
      protected
        FDownloaderThread: TDownloaderThread;
        FLastSessionElapsedTime, FTotalElapsedTime, FSessionStartTime: Cardinal;
        FLastSessionReadCount, FTotalReadCount, FTotalContentLength, FStartPosition, FEndPosition: Int64;
        FShouldResume: Boolean;

        FResumable: Boolean;
        FPaused: Boolean;

        FContentURIstr: String;
        FData: TStream;

        FOnDownloadProgress: TDownloadProgressEvent;
        FOnDownloadTerminated: TDownloadTerminationEvent;
        FOnDownloadComplete: TDownloadCompletionEvent;

        function GetContentURI: TURI;
        procedure Download(const StartPosition, EndPosition: Int64); overload;
        procedure ReceiveHandler(const Sender: TObject; ContentLength, ReadCount: Int64; var Abort: Boolean);
        procedure Initialize(const TheContentURI: TURI; const TheData: TStream; const TheStartPosition: Int64 = 0; const TheEndPosition: Int64 = 0);
        procedure Execute;

      public
        class function Download(const ContentURI: TURI; const Data: TStream): IDownloader; overload;

        constructor Create(const TheContentURI: TURI; const TheData: TStream; const TheStartPosition: Int64 = 0; const TheEndPosition: Int64 = 0);
        procedure Start;
        procedure SetPaused(const Value: Boolean);
        function IsTerminated: Boolean;
      published
        property Resumable: Boolean read FResumable write FResumable;
        property Paused: Boolean read FPaused write SetPaused;
        property Terminated: Boolean read IsTerminated;
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
        procedure SetFilepath(Value: String);

      public
        class function Download(const ContentURI: TURI; const Filepath: String): IFileDownloader; overload;

        constructor Create(const TheContentURI: TURI; const TheFilepath: String; const TheStartPosition: Int64 = 0; const TheEndPosition: Int64 = 0); overload;
        destructor Destroy; override;

        procedure Initialize(const TheContentURI: TURI; const TheFilepath: String; const TheStartPosition: Int64 = 0; const TheEndPosition: Int64 = 0); overload;

      published
        property Filepath: String read FFilepath write SetFilepath;
    end;

implementation
uses
  System.IOUtils, //for TPath, TDirectory
  System.Net.HttpClient, //for THTTPClient
  System.SysUtils; //for fmOpenWrite, fmShareDenyNone

{$region 'TDownloaderThread'}

constructor TDownloaderThread.Create(const TheDownloader: TDownloader);
begin
  inherited Create(true); //Create suspended
  //FreeOnTerminate := false; //This is the default
  Downloader := TheDownloader;
end;

procedure TDownloaderThread.Execute;
begin
  Downloader.Execute;
end;

{$endregion}

{$REGION 'TDownloader'}

class function TDownloader.Download(const ContentURI: TURI; const Data: TStream): IDownloader;
begin
  result := TDownloader.Create(ContentURI, Data);
  //result.Start; //TODO: should have TDownloader not autostart if we are to call this here
end;

constructor TDownloader.Create(const TheContentURI: TURI; const TheData: TStream; const TheStartPosition: Int64 = 0; const TheEndPosition: Int64 = 0);
begin
  Initialize(TheContentURI, TheData, TheStartPosition, TheEndPosition);
end;

procedure TDownloader.Initialize(const TheContentURI: TURI; const TheData: TStream; const TheStartPosition: Int64 = 0; const TheEndPosition: Int64 = 0);
begin
  FDownloaderThread := TDownloaderThread.Create(self);

  FContentURIstr := TheContentURI.ToString;
  FData := TheData;

  FStartPosition := TheStartPosition;
  FEndPosition := TheEndPosition;

  FLastSessionReadCount := TheStartPosition;

  FDownloaderThread.Start;
end;

function TDownloader.IsTerminated: Boolean;
begin
  result := Assigned(FDownloaderThread) and FDownloaderThread.CheckTerminated;
end;

procedure TDownloader.Start;
begin
  if (not FDownloaderThread.Started) then
    FDownloaderThread.Start;
  Paused := false;
end;

procedure TDownloader.Execute;
begin
  Download(FStartPosition, FEndPosition);

  while (not FDownloaderThread.CheckTerminated) and FResumable do
  begin
    if FShouldResume then
    begin
      FShouldResume := False;
      FPaused := False;
      Download(FLastSessionReadCount, FEndPosition);
    end;

    Sleep(SleepTimeMS); //sleep a bit, being polite to other threads
  end;
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

procedure TDownloader.Download(const StartPosition, EndPosition: Int64);
begin
  var HttpClient := THTTPClient.Create;
  HttpClient.OnReceiveData := ReceiveHandler;

  try
    FSessionStartTime := FDownloaderThread.GetTickCount;

    var StatusCode: Integer; //e.g. HTTP_OK=200

    if FEndPosition = 0 then
      StatusCode := HttpClient.Get(FContentURIstr, FData).StatusCode
    else
      begin
      FData.Seek(StartPosition, TSeekOrigin.soBeginning);
      StatusCode := HttpClient.GetRange(FContentURIstr, StartPosition, EndPosition, Data).StatusCode; //synchronous-blocking call (executes ReceiveHandler callback periodically)
      end;

    if Assigned(FOnDownloadTerminated) then
      FOnDownloadTerminated(Self, StatusCode);

    if Assigned(FOnDownloadComplete) and (StatusCode = STATUS_OK) then
      FOnDownloadComplete(Self, FData);

    FDownloaderThread.Terminate;

  finally
    HttpClient.Free;
  end;
end;

{   See: https://docwiki.embarcadero.com/Libraries/Sydney/en/System.Net.HttpClient.THTTPClient.OnReceiveData
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

class function TFileDownloader.Download(const ContentURI: TURI; const Filepath: String): IFileDownloader;
begin
  result := TFileDownloader.Create(ContentURI, Filepath);
  result.Start;
end;

constructor TFileDownloader.Create(const TheContentURI: TURI; const TheFilePath: String; const TheStartPosition: Int64 = 0; const TheEndPosition: Int64 = 0);
begin
  Initialize(TheContentURI, TheFilePath, TheStartPosition, TheEndPosition);
end;

destructor TFileDownloader.Destroy;
begin
  inherited;
  FreeAndNil(FDownloaderThread);
  FreeAndNil(FData); //when it's a TFileDownloader we weren't given a data stream, we created it, so have to free it
end;

procedure TFileDownloader.Initialize(const TheContentURI: TURI; const TheFilepath: string; const TheStartPosition: Int64 = 0; const TheEndPosition: Int64 = 0);
begin
  Filepath := TheFilepath; //this will call SetFilepath (don't just assign FFilePath)
  Initialize(TheContentURI, FData, TheStartPosition, TheEndPosition);
end;

procedure TFileDownloader.SetFilepath(Value: String);
begin
  FFilePath := Value;
  TDirectory.CreateDirectory(ExtractFileDir(Value)); //create any missing subdirectories
  FData := TFileStream.Create(Value, fmCreate {or fmShareDenyNone}); //TODO: fmShareDenyNote probably needed for Android
end;

{$endregion}

end.
