unit Zoomicon.Cache.Models;

interface
  uses System.Classes; //for TStream

  type

    IContentCache = interface
      ['{0E920B3E-7362-4653-8F5C-866745C1204F}']
      function HasContent(const Key: String): Boolean;
      function GetContent(const Key: String): TStream;
      procedure PutContent(const Key: String; const Content: TStream);
    end;

    IFileCache = interface(IContentCache)
      ['{FB74770E-59F9-4FDD-AAFE-1ADE32B0A63E}']
      function GetFilepath(const Key: String): String;
    end;

implementation

end.
