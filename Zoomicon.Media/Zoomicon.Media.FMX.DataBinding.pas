unit Zoomicon.Media.FMX.DataBinding;

interface
  uses
    System.Classes, //for TPersistent
    Data.Bind.ObjectScope, //for TListBindSourceAdapter
    FMX.Graphics, //for TBitmap
    FMX.Objects, //for TImage
    Zoomicon.Generics.Collections; //for TListEx

type

  {$region 'TFileSystemItem'}

  TFileSystemItem = class(TPersistent) //descending from TPersistent since "published" properties are used
  private
    FParentPath: String;
    FItemName: String;

  protected
    function GetItemPath: String;
    function GetItemFullPath: String;
    function GetThumbnail: TBitmap;

  public
    constructor Create(const APath: String);

  published
    property ParentPath: String read FParentPath;
    property ItemName: String read FItemName;
    property ItemPath: String read GetItemPath;
    property ItemFullPath: String read GetItemFullPath;
    property Thumbnail: TBitmap read GetThumbnail;
  end;

  {$endregion}

  {$region 'TFileItem/TFolderItem'}

  TFileItem = class; //forward declaration
  TFolderItem = class; //forward declaration

  TFileSystemItemList = TListEx<TFileSystemItem>;
  TFileItemList = TListEx<TFileItem>;
  TFolderItemList = TListEx<TFolderItem>;

  TFileItem = class(TFileSystemItem);

  TFolderItem = class(TFileSystemItem)
    class function GetFilesList(const APath: String): TFileItemList;
    class function GetFoldersList(const APath: String; const IncludeParent: Boolean = false): TFolderItemList;
    class function GetFoldersAndFilesList(const APath: String; const IncludeParent: Boolean = false): TFileSystemItemList;
  end;

  {$endregion}

  {$region 'TLocalFilesystemBindSourceAdapter'}

  TLocalFilesystemBindSourceAdapter = class(TListBindSourceAdapter<TFileSystemItem>)
  private
    FFolderPath: String;
    FShowFiles: Boolean;
    FShowFolders: Boolean;
  protected
    procedure SetFolderPath(const Value: String);
    procedure SetShowFiles(const Value: Boolean);
    procedure SetShowFolders(const Value: Boolean);
  public
    constructor Create(const AOwner: TComponent; const AFolderPath: String); overload;
    procedure Update;
  published
    property FolderPath: String read FFolderPath write SetFolderPath;
    property ShowFiles: Boolean read FShowFiles write SetShowFiles;
    property ShowFolders: Boolean read FShowFolders write SetShowFolders;
  end;

  {$endregion}

procedure Register;

implementation
  uses
    System.IOUtils, //for TDirectory
    System.RTLConsts, //for SPathNotFound
    System.SysUtils, //for EDirectoryNotFoundException, ExpandFileName
    FMX.Types; //for RegisterFmxClasses

{$region 'TFileSystemItem'}

constructor TFileSystemItem.Create(const APath: String);
begin
  FParentPath := ExtractFilePath(APath); //see what this returns for folders
  FItemName := ExtractFileName(APath);
end;

function TFileSystemItem.GetItemPath: String;
begin
  result := TPath.Combine(FParentPath, FItemName);
end;

function TFileSystemItem.GetItemFullPath: String;
begin
  result := ExpandFileName(ItemPath); //TODO: see if this works for Folders, else use TPath.GetFullPath for those
end;

function TFileSystemItem.GetThumbnail: TBitmap;
begin
  var ThumbPath := TPath.Combine(ParentPath, '.thumbs');
  ThumbPath := TPath.Combine(ThumbPath, ItemName + '.png');
  if TFile.Exists(ThumbPath) then
    result := TBitmap.CreateFromFile(ThumbPath)
  else
    result := nil;
end;

{$endregion}

{$region 'TFolderItem'}

class function TFolderItem.GetFilesList(const APath: String): TFileItemList;
begin
  var LFilenames := TDirectory.GetFiles(APath); //TODO: add SearchPattern etc. support
  var LFilesList := TFileItemList.Create(); //AOwnsItem = True by default
  LFilesList.Capacity := Length(LFilenames);
  for var LFilename in LFilenames do
    LFilesList.Add(TFileItem.Create(LFilename));
  result := LFilesList;
end;

class function TFolderItem.GetFoldersList(const APath: String; const IncludeParent: Boolean = false): TFolderItemList;
begin
  var LFoldernames := TDirectory.GetDirectories(APath); //TODO: add SearchPattern etc. support
  var LFoldersList := TFolderItemList.Create(); //AOwnsItem = True by default

  var LCapacity := Length(LFoldernames);
  if IncludeParent then
    inc(LCapacity);
  LFoldersList.Capacity := LCapacity;

  if IncludeParent then
    LFoldersList.Add(TFolderItem.Create(
      //TPath.Combine(APath, '..') //TODO: see why this fails when the parent item is picked, seems to try to generate a long chain of ../../..
      ExpandFileName(TPath.Combine(IncludeTrailingPathDelimiter(APath), '..'))
    )); //adding the parent folder first to be able to pick it and return to it //regarding IncludeTrailingPathDelimeter, see https://stackoverflow.com/questions/25669923/delphi-tpath-combinec-myfile-txt-leaves-out-dirseperator

  for var LFoldername in LFoldernames do
    LFoldersList.Add(TFolderItem.Create(LFoldername));
  result := LFoldersList;
end;

class function TFolderItem.GetFoldersAndFilesList(const APath: String; const IncludeParent: Boolean = false): TFileSystemItemList;
begin
  var LFoldersList := TFileSystemItemList(GetFoldersList(APath, IncludeParent));
  var LFilesList := TFileSystemItemList(GetFilesList(APath));

  var LFoldersAndFilesList := TFileSystemItemList.Create; //create an empty list
  with LFoldersAndFilesList do
  begin
    Capacity := LFoldersList.Count + LFilesList.Count; //allocate enough elements to add both folders and files
    AddRange(LFoldersList);
    AddRange(LFilesList);
  end;

  result := LFoldersAndFilesList;
end;

{$endregion}

{$region 'TLocalFilesystemAdapter'}

constructor TLocalFilesystemBindSourceAdapter.Create(const AOwner: TComponent; const AFolderPath: String);
begin
  inherited Create(AOwner);
  FFolderPath := AFolderPath;
end;

procedure TLocalFilesystemBindSourceAdapter.SetFolderPath(const Value: String);
begin
  if (FFolderPath <> Value) then
  begin
    if (Value <> '') and (not TDirectory.Exists(Value)) then
      raise EDirectoryNotFoundException.CreateRes(@SPathNotFound, Value);
    FFolderPath := Value;
    Update;
  end;
end;

procedure TLocalFilesystemBindSourceAdapter.SetShowFiles(const Value: Boolean);
begin
  if (FShowFiles <> Value) then
  begin
    FShowFiles := Value;
    Update;
  end;
end;

procedure TLocalFilesystemBindSourceAdapter.SetShowFolders(const Value: Boolean);
begin
  if (FShowFolders <> Value) then
  begin
    FShowFolders := Value;
    Update;
  end;
end;

procedure TLocalFileSystemBindSourceAdapter.Update;
var
  LList: TFileSystemItemList;
begin
  if (FFolderPath = '') then
    LList := nil
  else
    if (FShowFolders and FShowFiles) then
      LList := TFolderItem.GetFoldersAndFilesList(FFolderPath, true) //including parent folder
    else if FShowFiles then
      LList := TFileSystemItemList(TFolderItem.GetFilesList(FFolderPath))
    else if FShowFolders then
      LList := TFileSystemItemList(TFolderItem.GetFoldersList(FFolderPath, true)) //including parent folder
    else
      LList := nil;

  SetList(LList); //this should free the old list since the default is AOwnsItem=true
  Active := Assigned(LList);
  //if Active then Refresh; //TODO: is this needed? (seems not since we create and assing new List)
end;

{$endregion}

procedure RegisterSerializationClasses;
begin
  RegisterFmxClasses([TLocalFilesystemBindSourceAdapter]);
end;

procedure Register;
begin
  GroupDescendentsWith(TLocalFilesystemBindSourceAdapter, TComponent);
  RegisterSerializationClasses;
  RegisterComponents('Zoomicon', [TLocalFilesystemBindSourceAdapter]);
end;

initialization
  RegisterSerializationClasses; //don't call Register here, it's called by the IDE automatically on a package installation (fails at runtime)

end.
