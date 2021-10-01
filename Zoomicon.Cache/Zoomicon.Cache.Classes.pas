unit Zoomicon.Cache.Classes;

interface
  uses
    Zoomicon.Cache.Models, //for IContentCache
    System.Classes; //for TStream

  type
    TFileCache = class(TInterfacedObject, IFileCache)
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
    System.SysUtils, //for ENotImplemented, FileExists
    System.IOUtils, //for TPath, TDirectory
    System.ZLib; //for TZCompressionStream, TZDecompressionStream

function TFileCache.GetFilepath(const Key: String): String;
begin
  result := TPath.Combine(TPath.Combine(TPath.GetCachePath, UnitName), Key); //using UnitName in the path too since it also contains READCOM in it (in some platforms there's no per-app cache folder)
end;

{ FileCache }

function TFileCache.HasContent(const Key: String): Boolean;
begin
  result := FileExists(GetFilepath(Key));
end;

function TFileCache.GetContent(const Key: String): TStream;
begin
  var Filepath := GetFilepath(Key);
  if FileExists(Filepath) then
    result := TFileStream.Create(GetFilepath(Key), fmOpenRead)
  else
    result := nil;
end;

procedure TFileCache.PutContent(const Key: String; const Content: TStream);
begin
  var Filepath := GetFilepath(Key);

  TDirectory.CreateDirectory(TPath.GetDirectoryName(Filepath)); //create any missing subdirectories

  var CacheFile := TFileStream.Create(Filepath, fmOpenWrite); //overwrite any existing file
  try
    CacheFile.CopyFrom(Content);
  finally
    FreeAndNil(CacheFile);
  end;
end;

{ TCompressedFileCache }

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
