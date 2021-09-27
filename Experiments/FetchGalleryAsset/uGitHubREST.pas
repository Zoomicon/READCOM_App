unit uGitHubREST;

interface
  uses
   System.Classes, //for TStream
   System.Generics.Collections; //for TList

  type
    TGitItem = class(TObject)
      protected
        FSHA: String;
        FPath: String;
        FFilename: String;
      published
        property SHA: String read FSHA write FSHA;
        property Path: String read FPath write FPath;
        property Filename: String read FFilename write FFilename;
    end;

    TGitFile = class(TGitItem)
    end;

    TGitFolder = class(TGitItem)
      protected
        FFolders: TList<TGitFolder>;
        FFiles: TList<TGitFile>;
        function GetFolders: TList<TGitFolder>;
        function GetFiles: TList<TGitFile>;
      published
        property Folders: TList<TGitFolder> read GetFolders;
        property Files: TList<TGitFile> read GetFiles;
    end;

    TGitHubREST = class
      protected
        FAuthKey: String;
      public
        function GetFolder(SHA: string): TGitFolder;
        function GetFolders(ParentSHA: string): TList<TGitFolder>;
        function DownloadFile(theFile: TGitFile): TStream;
      published
        property AuthKey: String read FAuthKey write FAuthKey;
    end;

implementation
  uses
    System.SysUtils; //for ENotImplemented

{ TGitHubREST }

function TGitHubREST.DownloadFile(theFile: TGitFile): TMemoryStream;
begin
  raise ENotImplemented.Create('Not implemented');
end;

function TGitHubREST.ListFiles(theFolder: TGitFolder): TList<TGitFile>;
begin
  raise ENotImplemented.Create('Not implemented');
end;

function TGitHubREST.ListFolders(theFolder: TGitFolder): TList<TGitFolder>;
begin
  raise ENotImplemented.Create('Not implemented');
end;

{ TGitFolder }

function TGitFolder.GetFiles: TList<TGitFile>;
begin
  if (FFiles <> nil) then
    begin
    result:= FFiles;
    exit;
    end;

  var theFiles := TList<TGitFile>.Create();

  //TODO: read files

  FFiles := theFiles;
  result := theFiles;
end;

function TGitFolder.GetFolders: TList<TGitFolder>;
begin

end;

end.
