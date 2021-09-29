unit Zoomicon.Downloader;

interface
  uses
    System.Classes, //for TThread
    System.SyncObjs, //for TEvent
    System.Net.URLClient; //for TURI

  var
    SleepTimeMS: integer = 1;

  type
    TDownloadProgressEvent = procedure(const Sender: TObject; const TotalElapsedTime: Cardinal; const Speed: Integer; const TotalReadCount: Int64; const TotalContentLength: Int64; var Abort: Boolean) of object;
    TDownloadTerminationEvent = procedure(const Sender: TObject; const Status: Integer) of object;

    TDownloaderThread = class(TThread)
      protected
        FShouldResume: Boolean;
        FLastSessionElapsedTime, FTotalElapsedTime, FSessionStartTime: Cardinal;
        FLastSessionReadCount, FTotalReadCount, FTotalContentLength, FStartPosition, FEndPosition: Int64;

        FContentURIstr: String;
        FSaveFilePath: String;
        FResumable: Boolean;
        FPaused: Boolean;

        FOnDownloadProgress: TDownloadProgressEvent;
        FOnDownloadComplete: TDownloadTerminationEvent;

        function GetContentURI: TURI;
        procedure Download(const StartPosition, EndPosition: Int64);
        procedure ReceiveHandler(const Sender: TObject; ContentLength, ReadCount: Int64; var Abort: Boolean);

      published
        constructor Create(const TheContentURI: TURI; const TheSaveFilePath: string; const TheStartPosition, TheEndPosition: Int64);

        procedure Execute; override;
        procedure SetPaused(const Value: Boolean);

        property Resumable: Boolean read FResumable write FResumable;
        property IsPaused: Boolean read FPaused write SetPaused;
        property ContentURI: TURI read GetContentURI;
        property SaveFilePath: String read FSaveFilePath;

        property TotalElapsedtime: Cardinal read FTotalElapsedTime;
        property TotalReadCount: Int64 read FTotalReadCount;
        property TotalContentLength: Int64 read FTotalContentLength;

        property OnDownloadProgress: TDownloadProgressEvent write FOnDownloadProgress;
        property OnDownloadComplete: TDownloadTerminationEvent write FOnDownloadComplete;
    end;

implementation
uses
  System.IOUtils, //for TPath, TDirectory
  System.Net.HttpClient, //for THTTPClient
  System.SysUtils; //for fmOpenWrite, fmShareDenyNone

{ TDownloaderThread }

constructor TDownloaderThread.Create(const TheContentURI: TURI; const TheSaveFilePath: String; const TheStartPosition, TheEndPosition: Int64);
begin
  inherited Create(True); //Create suspended

  FreeOnTerminate := true;

  FContentURIstr := TheContentURI.ToString;
  FSaveFilePath := TheSaveFilePath;
  FStartPosition := TheStartPosition;
  FEndPosition := TheEndPosition;

  FLastSessionReadCount := TheStartPosition;
end;

procedure TDownloaderThread.Execute;
begin
  Download(FStartPosition, FEndPosition);

  while (not Terminated) and FResumable do
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

function TDownloaderThread.GetContentURI: TURI;
begin
  result := TURI.Create(FContentURIstr);
end;

procedure TDownloaderThread.SetPaused(const Value: Boolean);
begin
  if Value then
    FPaused := True
  else
    FShouldResume := True;
end;

procedure TDownloaderThread.Download(const StartPosition, EndPosition: Int64);
begin
  TDirectory.CreateDirectory(TPath.GetDirectoryName(FSaveFilePath)); //create any missing subdirectories

  var OutputStream := TFileStream.Create(FSaveFilePath, fmOpenWrite or fmShareDenyNone);
  try
    var HttpClient := THTTPClient.Create();
    HttpClient.OnReceiveData := ReceiveHandler;

    try
      FSessionStartTime := GetTickCount;

      var StatusCode: Integer;

      if FEndPosition = 0 then
        StatusCode := HttpClient.Get(FContentURIstr, OutputStream).StatusCode
      else
        begin
        OutputStream.Seek(StartPosition, TSeekOrigin.soBeginning);
        StatusCode := HttpClient.GetRange(FContentURIstr, StartPosition, EndPosition, OutputStream).StatusCode; //synchronous-blocking call (executes ReceiveHandler callback periodically)
        end;

      if Assigned(FOnDownloadComplete) then
        FOnDownloadComplete(Self, StatusCode);

      Terminate;

    finally
      HttpClient.Free;
    end;
  finally
    OutputStream.Free;
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
procedure TDownloaderThread.ReceiveHandler(const Sender: TObject; ContentLength, ReadCount: Int64; var Abort: Boolean);
begin
  var SessionTime := GetTickCount - FSessionStartTime; //in msec

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
  if Terminated or FPaused then
    begin
    FLastSessionElapsedTime := FTotalElapsedTime;
    FLastSessionReadCount := FTotalReadCount;
    Abort := true;
    end;
end;

end.
