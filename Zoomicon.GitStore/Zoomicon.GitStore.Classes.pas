unit Zoomicon.GitStore.Classes;

interface
  uses
    Zoomicon.GitStore.Models, //for IGitItem, IGitFile, IGitFolder, IGitStore
    System.Classes, //for TStream
    System.Generics.Collections, //for TList
    System.Net.URLClient; //for TURI

  type

    TGitItem = class(TInterfacedObject, IGitItem)
      protected
        FSHA: String;
        FPath: String;
      public
        constructor Create(SHA: String; Path: String);
        function GetSHA: String;
        function GetPath: String;
    end;

    TGitFile = class(TGitItem, IGitFile)
      function Download: TStream;
    end;

    TGitFolder = class(TGitItem, IGitFolder)
      function GetItems(const recursive: Boolean = false): TList<IGitItem>;
      function GetFolders(const recursive: Boolean = false): TList<IGitFolder>;
      function GetFiles(const recursive: Boolean = false): TList<IGitFile>;
    end;

    TGitStore = class(TInterfacedObject, IGitStore)
      function GetRepositoryURI: TURI;
      procedure SetRepositoryURI(const Value: TURI);

      function GetAuthKey: String;
      procedure SetAuthKey(const Value: String);

      function GetBaseFolder: IGitFolder;

      function GetItem(SHA: string): IGitItem;
      function GetFile(SHA: string): IGitFile;
      function GetFolder(SHA: string): IGitFolder;
    end;

implementation
  uses
    Zoomicon.Downloader, //for TDownloaderThread
    System.IOUtils, //for TPath
    System.Net.HttpClient, //for THttpClient
    System.SysUtils; //for ENotImplemented

{ TGitItem }

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

{ TGitFile }

function TGitFile.Download: TStream;
begin
  //var DownloaderThread := TDownloaderThread.Create(...)
  raise ENotImplemented.Create('Not implemented yet');
end;

{ TGitFolder }

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

{ TGitStore }

function TGitStore.GetRepositoryURI: TURI;
begin
  raise ENotImplemented.Create('Not implemented yet');
end;

procedure TGitStore.SetRepositoryURI(const Value: TURI);
begin
  raise ENotImplemented.Create('Not implemented yet');
end;

function TGitStore.GetAuthKey: String;
begin
  raise ENotImplemented.Create('Not implemented yet');
end;

procedure TGitStore.SetAuthKey(const Value: String);
begin
  raise ENotImplemented.Create('Not implemented yet');
end;

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

end.
