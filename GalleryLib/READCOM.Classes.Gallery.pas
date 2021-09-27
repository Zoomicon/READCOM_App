unit READCOM.Classes.Gallery;

interface
  uses
    READCOM.Models.Gallery, //for IGallery, IGalleryFolder, IGalleryFile
    System.Classes, //for TStream
    System.Generics.Collections; //for TList<T>

  type
    TGallery = class(TInterfacedObject, IGallery)
      function GetHomeFolder: IGalleryFolder;
    end;

    TGalleryFolder = class(TInterfacedObject, IGalleryFolder)
      function GetFolders: TList<IGalleryFolder>;
      function GetFiles: TList<IGalleryFile>;
    end;

    TGalleryFile = class(TInterfacedObject, IGalleryFile)
      function IsCached: Boolean;
      function GetContent: TStream;
    end;

implementation
  uses
    System.SysUtils; //for ENotImplemented

{ TGallery }

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
