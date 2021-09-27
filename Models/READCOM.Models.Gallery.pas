unit READCOM.Models.Gallery;

interface
  uses
    System.Classes, //for TStream
    System.Generics.Collections; //for TList<T>

  type
    IGalleryFile = interface
      ['{7DB119EE-76FE-4619-97C8-F5E1605570DB}']
      function IsCached: Boolean;
      function GetContent: TStream;
    end;

    IGalleryFolder = interface
      ['{159272E4-9A5E-4C50-A6D9-DEC807B1C71E}']
      function GetFolders: TList<IGalleryFolder>;
      function GetFiles: TList<IGalleryFile>;
    end;

    IGallery = interface
      ['{FC9A5426-FF28-4AEA-86EC-AFEB88506D2A}']
      function GetHomeFolder: IGalleryFolder;
    end;

implementation

end.
