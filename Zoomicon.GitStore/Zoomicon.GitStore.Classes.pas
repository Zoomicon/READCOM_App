unit Zoomicon.GitStore.Classes;

interface
  uses
    Zoomicon.Cache.Models, //for IContentCache
    Zoomicon.Downloader.Models, //for TDownloadCompletionEvent, IFileDownloader
    Zoomicon.GitStore.Models, //for IGitItem, IGitFile, IGitFolder, IGitStore
    System.Classes, //for TStream
    System.Generics.Collections, //for TList
    System.Net.URLClient; //for TURI

  type

    TGitStore = class;

    TGitItem = class(TInterfacedObject, IGitItem)
      protected
        FSHA: String;
        FPath: String;
        FGitStore: TGitStore;

      public
        constructor Create(SHA: String; Path: String);
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

    TGitStore = class(TInterfacedObject, IGitStore)
      protected
        FContentCache: IContentCache;
        FDownloadBranchName: String;
        FDownloadURI: String;

      public
        constructor Create(const TheContentCache: IContentCache = nil);

        function GetRepositoryURI: TURI;
        procedure SetRepositoryURI(const Value: TURI);

        function GetAuthKey: String;
        procedure SetAuthKey(const Value: String);

        function GetBaseFolder: IGitFolder;

        function GetItem(SHA: string): IGitItem;
        function GetFile(SHA: string): IGitFile;
        function GetFolder(SHA: string): IGitFolder;

      published
        property ContentCache: IContentCache read FContentCache write FContentCache;
        property DownloadBranchName: String read FDownloadBranchName write FDownloadBranchName;
        property DownloadURI: String read FDownloadURI;
    end;

implementation
  uses
    Zoomicon.Downloader.Classes, //for TFileDownloader
    System.IOUtils, //for TPath
    System.Net.HttpClient, //for THttpClient
    System.SysUtils; //for ENotImplemented

{$region 'TGitItem'}

constructor TGitItem.Create(SHA, Path: String);
begin
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
  result := TURI.Create(FGitStore.DownloadURI + '/' + FGitStore.DownloadBranchName + '/' + FPath);
end;

procedure TGitFile.Download(const DownloadCompleteHandler: TDownloadCompletionEvent);
begin
  if (not Assigned(DownloadCompleteHandler)) or (not Assigned(FGitStore)) then exit;

  var Data := TMemoryStream.Create;
  var Downloader := TDownloader.Create(FileURI, Data, FGitStore.FContentCache);
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

constructor TGitStore.Create(const TheContentCache: IContentCache = nil);
begin
  FContentCache := TheContentCache;
end;

{$region 'RepositoryURI'}

function TGitStore.GetRepositoryURI: TURI;
begin
  raise ENotImplemented.Create('Not implemented yet');
end;

procedure TGitStore.SetRepositoryURI(const Value: TURI);
begin
  raise ENotImplemented.Create('Not implemented yet');
end;

{$endregion}

{$region 'AuthKey'}

function TGitStore.GetAuthKey: String;
begin
  raise ENotImplemented.Create('Not implemented yet');
end;

procedure TGitStore.SetAuthKey(const Value: String);
begin
  raise ENotImplemented.Create('Not implemented yet');
end;

{$endregion}

function TGitStore.GetBaseFolder: IGitFolder;
begin
  raise ENotImplemented.Create('Not implemented yet');
end;

function TGitStore.GetItem(SHA: string): IGitItem;
begin
  raise ENotImplemented.Create('Not implemented yet');
end;

function TGitStore.GetFolder(SHA: string): IGitFolder;
begin
  raise ENotImplemented.Create('Not implemented yet');
end;

function TGitStore.GetFile(SHA: string): IGitFile;
begin
  raise ENotImplemented.Create('Not implemented yet');
end;

{$endregion}

end.
