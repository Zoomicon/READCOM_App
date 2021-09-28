unit READCOM.Classes.Gallery;

interface
  uses
    READCOM.Models.Gallery, //for IGallery, IGalleryFolder, IGalleryFile
    READCOM.Models.Cache, //for IContentCache
    Zoomicon.GitStore.Models, //for IGitStore
    System.Classes, //for TStream
    System.Generics.Collections; //for TList

  type
    TGallery = class(TInterfacedObject, IGallery)
      protected
        FCache: IContentCache;
        FStorage: IGitStore;
      public
        constructor Create(const Cache: IContentCache; const Storage: IGitStore);
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
      public
        //IGalleryFile
        function IsCached: Boolean;
        function GetContent: TStream;
    end;

implementation
  uses
    System.SysUtils; //for ENotImplemented

{ TGallery }

constructor TGallery.Create(const Cache: IContentCache; const Storage: IGitStore);
begin
  FCache := Cache;
  FStorage := Storage;
end;

function TGallery.GetHomeFolder: IGalleryFolder;
begin
  raise ENotImplemented.Create('Not implemented');
end;

{ TGalleryFolder }

function TGalleryFolder.GetFiles: TList<IGalleryFile>;
begin
  raise ENotImplemented.Create('Not implemented');
end;

function TGalleryFolder.GetFolders: TList<IGalleryFolder>;
begin
  raise ENotImplemented.Create('Not implemented');
end;

{ TGalleryFile }

function TGalleryFile.GetContent: TStream;
begin
  raise ENotImplemented.Create('Not implemented');
end;

function TGalleryFile.IsCached: Boolean;
begin
  raise ENotImplemented.Create('Not implemented');
end;

end.
