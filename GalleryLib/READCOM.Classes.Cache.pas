unit READCOM.Classes.Cache;

interface
  uses
    READCOM.Models.Cache, //for IFileBasedContentCache
    System.Classes; //for TStream

  type
    TFileCache = class(TInterfacedObject, IContentCache)
      protected
       function GetFilename(const Key: String): String;
      public
        {IContentCache}
        function HasContent(const Key: String): Boolean;
        function GetContent(const Key: String): TStream; virtual;
        procedure PutContent(const Key: String; const Content: TStream); virtual;
    end;

    TZCompressionFileCache = class(TFileCache)
      function GetContent(const Key: String): TStream; override;
      procedure PutContent(const Key: String; const Content: TStream); override;
    end;

implementation
  uses
    System.SysUtils, //for ENotImplemented, FileExists
    System.IOUtils, //for TPath
    System.ZLib; //for TZCompressionStream, TZDecompressionStream

function TFileCache.GetFilename(const Key: String): String;
begin
  result := TPath.Combine(TPath.Combine(TPath.GetCachePath, UnitName), Key); //using UnitName in the path too since it also contains READCOM in it (in some platforms there's no per-app cache folder)
end;

{ FileCache }

function TFileCache.HasContent(const Key: String): Boolean;
begin
  result := FileExists(GetFilename(Key));
end;

function TFileCache.GetContent(const Key: String): TStream;
begin
  result := TFileStream.Create(GetFilename(Key), fmOpenRead);
end;

procedure TFileCache.PutContent(const Key: String; const Content: TStream);
begin
  var CacheFile := TFileStream.Create(Key, fmOpenWrite);
  try
    CacheFile.CopyFrom(Content);
  finally
    FreeAndNil(CacheFile);
  end;
end;

{ TCompressedFileCache }

function TZCompressionFileCache.GetContent(const Key: String): TStream;
begin
  result := TZDecompressionStream.Create(inherited GetContent(Key));
end;

procedure TZCompressionFileCache.PutContent(const Key: String; const Content: TStream);
begin
  var ZCompressedContent := TZCompressionStream.Create(Content);
  try
    inherited PutContent(Key, ZCompressedContent);
  finally
    FreeAndNil(ZCompressedContent);
  end;
end;

end.
