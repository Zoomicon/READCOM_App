unit Zoomicon.GitStore.Classes;

interface
  uses
    Zoomicon.Cache.Models, //for IContentCache
    Zoomicon.Downloader.Models, //for TDownloadCompletionEvent, IFileDownloader
    Zoomicon.GitStore.Models, //for IGitItem, IGitFile, IGitFolder, IGitStore
    FireDAC.Comp.Client, //for TFDMemTable
    System.Classes, //for TStream
    System.Generics.Collections, //for TList
    System.Net.URLClient; //for TURI

  type

    TGitStore = class;

    TGitItem = class(TComponent, IGitItem)
      protected
        FSHA: String;
        FPath: String;
        FGitStore: TGitStore;

      public
        constructor Create(AOwner: TComponent; SHA: String; Path: String); reintroduce;
        function GetSHA: String;
        function GetPath: String;
    end;

    TGitFile = class(TGitItem, IGitFile)
      protected
        function GetFileURI: TURI;

      public
        procedure Download(const DownloadCompleteHandler: TDownloadCompletionEvent);

      published
        property FileURI: TURI read GetFileURI;
    end;

    TGitFolder = class(TGitItem, IGitFolder)
      public
        function GetItems(const recursive: Boolean = false): TList<IGitItem>;
        function GetFolders(const recursive: Boolean = false): TList<IGitFolder>;
        function GetFiles(const recursive: Boolean = false): TList<IGitFile>;
    end;

    TGitStore = class(TComponent, IGitStore)
      protected
        FContentCache: IContentCache;
        FTimeout: integer; //in msec

        FRepositoryOwner: String;
        FRepository: String;
        FBranch: String;

        function GetDownloadURI: String;
        function GetAPIURI: String;
        function GetBranchAPIURI: String;
        function GetLatestCommitAPIURI: String;
        function GetBaseFolderSHA: String;
        function GetTreeAPIURI(SHA: string; Recursive: Boolean = true): String;

      public
        constructor Create(AOwner: TComponent; const TheRepositoryOwner: String; const TheRepository: String; const TheBranch: String; const TheContentCache: IContentCache = nil); reintroduce;

        function LoadTreeContents(const SHA: string; Recursive: Boolean = true): TFDMemTable;
        function LoadContents: TFDMemTable;

        function GetBaseFolder: IGitFolder;

        function GetItem(SHA: string): IGitItem;
        function GetFile(SHA: string): IGitFile;
        function GetFolder(SHA: string): IGitFolder;

      published
        property Timeout: Integer read FTimeout write FTimeout;
        property RepositoryOwner: String read FRepositoryOwner write FRepositoryOwner;
        property Repository: String read FRepository write FRepository;
        property Branch: String read FBranch write FBranch;
        property ContentCache: IContentCache read FContentCache write FContentCache;
        property DownloadURI: String read GetDownloadURI;
        property APIURI: String read GetAPIURI;
    end;

implementation
  uses
    Zoomicon.Downloader.Classes, //for TFileDownloader
    FireDAC.Stan.Option, //for TFDFetchOptionValue, TFDUpdateOptionValue
    REST.Client, //for TRESTClient
    REST.Response.Adapter, //for TRESTResponseDataSetAdapter
    REST.Types, //for TRESTRequestParameterOption
    System.IOUtils, //for TPath
    System.Net.HttpClient, //for THttpClient
    System.SyncObjs, //for TWaitResult
    System.SysUtils; //for ENotImplemented

{$region 'TGitItem'}

constructor TGitItem.Create(AOwner: TComponent; SHA, Path: String);
begin
  inherited Create(AOwner);

  FSHA := SHA;
  FPath := Path;
end;

function TGitItem.GetSHA: String;
begin
  result := FSHA;
end;

function TGitItem.GetPath: String;
begin
  result := FPath;
end;

{$endregion}

{$region 'TGitFile'}

function TGitFile.GetFileURI: TURI;
begin
  result := TURI.Create(FGitStore.DownloadURI + '/' + FGitStore.Branch + '/' + FPath);
end;

procedure TGitFile.Download(const DownloadCompleteHandler: TDownloadCompletionEvent);
begin
  if (not Assigned(DownloadCompleteHandler)) or (not Assigned(FGitStore)) then exit;

  var Data := TMemoryStream.Create;
  var Downloader := TDownloader.Create(Self, FileURI, Data, FGitStore.FContentCache);
  Downloader.OnDownloadComplete := DownloadCompleteHandler;
  Downloader.Start;

  raise ENotImplemented.Create('Not implemented yet');
end;

{$endregion}

{$region 'TGitFolder'}

function TGitFolder.GetItems(const recursive: Boolean): TList<IGitItem>;
begin
  raise ENotImplemented.Create('Not implemented yet');
end;

function TGitFolder.GetFolders(const recursive: Boolean = false): TList<IGitFolder>;
begin
  raise ENotImplemented.Create('Not implemented yet');
end;

function TGitFolder.GetFiles(const recursive: Boolean = false): TList<IGitFile>;
begin
  raise ENotImplemented.Create('Not implemented yet');
end;

{$endregion}

{$region 'TGitStore'}

constructor TGitStore.Create(AOwner: TComponent; const TheRepositoryOwner: String; const TheRepository: String; const TheBranch: String; const TheContentCache: IContentCache = nil);
begin
  inherited Create(AOwner);

  FRepositoryOwner := TheRepositoryOwner;
  FRepository := TheRepository;
  FBranch := TheBranch;
  FContentCache := TheContentCache;
end;

{$region 'URI helpers'}

function TGitStore.GetDownloadURI: String;
begin
  result := Format('https://raw.githubusercontent.com/%s/%s/%s/', [FRepositoryOwner, FRepository, FBranch]);
end;

function TGitStore.GetAPIURI: String;
begin
  result := Format('https://api.github.com/repos/%s/%s/', [FRepositoryOwner, FRepository]);
end;

function TGitStore.GetBranchAPIURI: String;
begin
  result := APIURI + Format('branches/%s', [FBranch]);
end;

function TGitStore.GetLatestCommitAPIURI: String;
begin
  result := APIURI + Format('commits/%s', [FBranch]);
end;

function TGitStore.GetTreeAPIURI(SHA: string; Recursive: Boolean = true): String;
var RecursiveStr: String;
begin
  if (SHA = '') then
    raise Exception.Create('GitStore.GetTreeAPI: SHA can''t be empty');

  if Recursive then RecursiveStr := '?recursive=1' else RecursiveStr := '';
  result := APIURI + Format('git/trees/%s%s', [SHA, RecursiveStr]);
end;

{$endregion}

{//NOT WORKING: Using other implementation below
function TGitStore.GetBaseFolderSHA: String;
var Reader: TReader;
begin
  var Data := TMemoryStream.Create; //read to memory
  var Downloader := TDownloader.Create(TURI.Create(GetLatestCommitAPIURI), Data, nil, false); //do not use caching, do not autostart
  Downloader.HeaderAccept := 'application/vnd.github.VERSION.sha';

  if (Downloader.WaitForDownload(FTimeout) = TWaitResult.wrSignaled) then
    begin
    Reader := TReader.Create(Data, $FF); //2nd param is buffer size
    result := Reader.ReadStr;
    end
  else
    result := '';

  //Cleanup
  FreeAndNil(Reader);
  FreeAndNil(Data);
  FreeAndNil(Downloader);
end;
}

function TGitStore.GetBaseFolderSHA: String;
begin
  var RESTClient := TRESTClient.Create('https://api.github.com/repos/zoomicon/READCOM_Gallery/commits/main');

  try

    {$region 'init'}

    var RESTResponse := TRESTResponse.Create(RESTClient);

    var RESTRequest := TRESTRequest.Create(RESTClient);
    with RESTRequest do
      begin
      AssignedValues := [TCustomRESTRequest.TAssignedValue.rvConnectTimeout, TCustomRESTRequest.TAssignedValue.rvReadTimeout];
      ConnectTimeout := FTimeout;
      ReadTimeout := FTimeout;
      Client := RESTClient;

      {with Params.AddItem do
      begin
        Kind := TRESTRequestParameterKind.pkHTTPHEADER;
        Name := 'Accept';
        Value := 'application/vnd.github.VERSION.sha';
        Options := Options + [TRESTRequestParameterOption.poDoNotEncode];
      end;}
      Params.AddHeader('Accept', 'application/vnd.github.VERSION.sha').Options := [TRESTRequestParameterOption.poDoNotEncode];

      Response := RESTResponse;
    end;

    {$endregion}

    RESTRequest.Execute;
    result := RESTResponse.Content;

  finally
    RESTClient.Free; //this should free the other owned objects too
  end;

end;

function TGitStore.LoadContents: TFDMemTable;
begin
  result := LoadTreeContents(GetBaseFolderSHA, true); //Recursive
end;

function TGitStore.LoadTreeContents(const SHA: string; Recursive: Boolean = true): TFDMemTable;
begin
  var RESTClient := TRESTClient.Create(nil);

  try

   {$region 'init'} //based on output of REST Debugger (https://www.embarcadero.com/free-tools/rest-debugger)

    RESTClient.BaseURL := GetTreeAPIURI(SHA, Recursive);

    var RESTResponse := TRESTResponse.Create(RESTClient);
    RESTResponse.RootElement := 'tree';

    var RESTRequest := TRESTRequest.Create(RESTClient);
    with RESTRequest do
      begin
      AssignedValues := [TCustomRESTRequest.TAssignedValue.rvConnectTimeout, TCustomRESTRequest.TAssignedValue.rvReadTimeout];
      ConnectTimeout := FTimeout;
      ReadTimeout := FTimeout;
      Client := RESTClient;
      Response := RESTResponse;
      end;

    var FDMemTable := TFDMemTable.Create(nil);
    with FDMemTable do
      begin
      with FetchOptions do
        begin
        AssignedValues := [TFDFetchOptionValue.evMode];
        Mode := fmAll;
        end;
      with ResourceOptions do
        begin
        AssignedValues := [rvSilentMode];
        SilentMode := True;
        end;
      with UpdateOptions do
        begin
        AssignedValues := [TFDUpdateOptionValue.uvUpdateChngFields, TFDUpdateOptionValue.uvUpdateMode, TFDUpdateOptionValue.uvLockMode, TFDUpdateOptionValue.uvLockPoint, TFDUpdateOptionValue.uvLockWait, TFDUpdateOptionValue.uvRefreshMode, TFDUpdateOptionValue.uvFetchGeneratorsPoint, TFDUpdateOptionValue.uvCheckRequired, TFDUpdateOptionValue.uvCheckReadOnly, TFDUpdateOptionValue.uvCheckUpdatable];
        LockWait := True;
        FetchGeneratorsPoint := gpNone;
        CheckRequired := False;
        end;
      end;

    var RESTResponseDataSetAdapter := TRESTResponseDataSetAdapter.Create(RESTClient);
    with RESTResponseDataSetAdapter do
      begin
      Dataset := FDMemTable;
      Response := RESTResponse;
      end;

    {$endregion}

    try
      RESTRequest.Execute;
      result := FDMemTable;
    except
      FreeAndNil(FDMemTable);
      raise;
    end;

  finally
    RESTClient.Free; //this should free the other owned objects too
  end;
end;

function TGitStore.GetBaseFolder: IGitFolder;
begin
  raise ENotImplemented.Create('Not implemented yet');
end;

function TGitStore.GetItem(SHA: string): IGitItem;
begin
  raise ENotImplemented.Create('Not implemented yet');
end;

function TGitStore.GetFile(SHA: string): IGitFile;
begin
  raise ENotImplemented.Create('Not implemented yet');
end;

function TGitStore.GetFolder(SHA: string): IGitFolder;
begin
  raise ENotImplemented.Create('Not implemented yet');
end;

{$endregion}

end.
