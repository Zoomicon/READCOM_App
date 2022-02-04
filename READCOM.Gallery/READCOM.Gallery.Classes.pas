unit READCOM.Gallery.Classes;

interface
  uses
    READCOM.Gallery.Models, //for IGallery, IGalleryFolder, IGalleryFile
    Zoomicon.Downloader.Models, //for TDownloadCompletionEvent
    Zoomicon.GitStore.Models, //for IGitStore
    System.Classes, //for TStream
    System.Generics.Collections; //for TList

  type
    TGallery = class(TInterfacedObject, IGallery)
      protected
        FStorage: IGitStore;
      public
        constructor Create(const Storage: IGitStore);
        //IGallery
        function GetHomeFolder: IGalleryFolder;
    end;

    TGalleryFolder = class(TInterfacedObject, IGalleryFolder)
      public
        //IGalleryFolder
        function GetFolders: TList<IGalleryFolder>;
        function GetFiles: TList<IGalleryFile>;
    end;

    TGalleryFile = class(TInterfacedObject, IGalleryFile)
      protected
        FGitFile : IGitFile;
      public
        //IGalleryFile
        function IsCached: Boolean;
        procedure DownloadContent(const DownloadCompletedHandler: TDownloadCompletionEvent);
    end;

implementation
  uses
    System.SysUtils; //for ENotImplemented

{TGallery}

constructor TGallery.Create(const Storage: IGitStore);
begin
  FStorage := Storage;
end;

function TGallery.GetHomeFolder: IGalleryFolder;
begin
  raise ENotImplemented.Create('Not implemented');
end;

{TGalleryFolder}

function TGalleryFolder.GetFiles: TList<IGalleryFile>;
begin
  raise ENotImplemented.Create('Not implemented');
end;

function TGalleryFolder.GetFolders: TList<IGalleryFolder>;
begin
  raise ENotImplemented.Create('Not implemented');
end;

{TGalleryFile}

procedure TGalleryFile.DownloadContent(const DownloadCompletedHandler: TDownloadCompletionEvent);
begin
  if Assigned(FGitFile) then
    FGitFile.Download(DownloadCompletedHandler);
end;

function TGalleryFile.IsCached: Boolean;
begin
  raise ENotImplemented.Create('Not implemented');
end;

end.
