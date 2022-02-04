unit Zoomicon.Cache.Classes;

interface
  uses
    Zoomicon.Cache.Models, //for IContentCache
    System.Classes; //for TStream

  type
    TFileCache = class(TInterfacedObject, IContentCache, IFileCache)
      //note: declaring both as implemented, even though IContentCache is ancestor of IFileCache: needed in Delphi if we want to pass as the ancestor interface

      protected
        function GetKeyHash(const Key: String): String; virtual;

      public
        function GetFilepath(const Key: String): String;
        function HasContent(const Key: String): Boolean;
        function GetContent(const Key: String): TStream; virtual;
        procedure PutContent(const Key: String; const Content: TStream); virtual;
    end;

    TZCompressedFileCache = class(TFileCache)
      function GetContent(const Key: String): TStream; override;
      procedure PutContent(const Key: String; const Content: TStream); override;
    end;

implementation
  uses
    System.Hash, //for THashSHA2
    System.SysUtils, //for ENotImplemented, FileExists
    System.IOUtils, //for TPath, TDirectory
    System.ZLib; //for TZCompressionStream, TZDecompressionStream

function TFileCache.GetKeyHash(const Key: String): String;
begin
  result := THashSHA2.GetHashString(Key, THashSHA2.TSHA2Version.SHA512).ToUpper; //See https://en.wikipedia.org/wiki/SHA-2
end;

function TFileCache.GetFilepath(const Key: String): String;
begin
  result := TPath.Combine(TPath.Combine(TPath.GetCachePath, UnitName), GetKeyHash(Key));
  //Using UnitName in the path too since it also contains READCOM in it (in some platforms there's no per-app cache folder)
  //Using GetKeyHash (does SHA-512) to generate a unique HEX chars string from given Key (which could contain illegal file chars, e.g. a URL)
end;

{FileCache}

function TFileCache.HasContent(const Key: String): Boolean;
begin
  result := FileExists(GetFilepath(Key));
end;

function TFileCache.GetContent(const Key: String): TStream;
begin
  var Filepath := GetFilepath(Key);
  if FileExists(Filepath) then
    result := TFileStream.Create(Filepath, fmOpenRead)
  else
    result := nil;
end;

procedure TFileCache.PutContent(const Key: String; const Content: TStream);
begin
  var Filepath := GetFilepath(Key);

  TDirectory.CreateDirectory(TPath.GetDirectoryName(Filepath)); //create any missing subdirectories

  var CacheFile := TFileStream.Create(Filepath, fmCreate {or fmShareDenyNone}); //overwrite any existing file //TODO: fmShareDenyNote probably needed for Android
  try
    CacheFile.CopyFrom(Content); //copies from start of stream
  finally
    FreeAndNil(CacheFile);
  end;
end;

{TCompressedFileCache}

function TZCompressedFileCache.GetContent(const Key: String): TStream;
begin
  var ZCompressedContent := inherited GetContent(Key);
  if Assigned(ZCompressedContent) then
    result := TZDecompressionStream.Create(ZCompressedContent)
  else
    result := nil;
end;

procedure TZCompressedFileCache.PutContent(const Key: String; const Content: TStream);
begin
  var ZCompressedContent := TZCompressionStream.Create(Content);
  try
    inherited PutContent(Key, ZCompressedContent);
  finally
    FreeAndNil(ZCompressedContent);
  end;
end;

end.

