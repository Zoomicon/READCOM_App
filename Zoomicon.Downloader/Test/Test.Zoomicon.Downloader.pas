unit Test.Zoomicon.Downloader; //Delphi DUnit Test Cases

interface
  uses
    Zoomicon.Cache.Models, //for IContentCache
    TestFramework,
    System.Net.URLClient,
    System.Classes;

  const
    DOWNLOAD_TIMEOUT: Cardinal = 10000; //10 sec (can also use INFINITE)

    DOWNLOAD_URI_STR = 'https://raw.githubusercontent.com/zoomicon/READCOM_Gallery/master/Gallery/README.md';
    SAVE_FILENAME = 'Test1.md';

    DOWNLOAD_URI_STR_NOCACHE = 'https://raw.githubusercontent.com/zoomicon/READCOM_Gallery/master/Gallery/Assets/Images/README.md';
    SAVE_FILENAME_NOCACHE = 'Test2.md';

    DOWNLOAD_URI_WRONG = 'blablabla';
    SAVE_FILENAME_WRONGURI = 'Test3.md';

  type

    // Test methods for class TFileDownloader

    TestTFileDownloader = class(TTestCase)
      strict private
        FContentCache: IContentCache;

      private
        procedure DoDownload(const TheContentURI: TURI; const TheSaveFilepath: String; const TheContentCache: IContentCache);
        procedure DoTestSingleDownload(const URIstr: String; const SaveFilename: String; const cache: IContentCache; const ExpectedFileExists: Boolean = true);
        procedure DoTestMultipleDownloads(const URIstr: String; const SaveFilename: String; const cache: IContentCache; const ExpectedFileExists: Boolean = true);

      public
        procedure SetUp; override;
        procedure TearDown; override;

      published //need to pubish Test cases for them to be used
        procedure TestSingleDownload;
        procedure TestMultipleDownloadWithoutCache;
        procedure TestMultipleDownload;
        procedure TestSingleDownloadWithoutCache;
        procedure TestWrongUrlDownload;
    end;

implementation
  uses
    Zoomicon.Cache.Classes, //for TFileCache
    Zoomicon.Downloader.Models,
    Zoomicon.Downloader.Classes,
    System.SysUtils,
    System.IOUtils;

{$region 'SetUp / TearDown'}

procedure TestTFileDownloader.SetUp;
begin
  FContentCache := TFileCache.Create As IContentCache;
end;

procedure TestTFileDownloader.TearDown;
begin
  //FContentCache is reference counted, so object it points to will be released automatically
end;

{$endregion}

{$region 'Helpers'}

procedure TestTFileDownloader.DoDownload(const TheContentURI: TURI; const TheSaveFilepath: String; const TheContentCache: IContentCache);
begin
  if FileExists(TheSaveFilepath) then
    TFile.Delete(TheSaveFilepath); //remove download file if existing so that we can check if it was created later

  var FileDownloader := TFileDownloader.Create(nil, TheContentURI, TheSaveFilepath, TheContentCache, true); //AutoStart
  //FileDownloader.OnlyFallbackCache := true; //would use this if we only wanted to fallback to cache in case of download errors / offline case
  FileDownloader.WaitForDownload(DOWNLOAD_TIMEOUT); //Note: this can freeze the main thread
  FreeAndNil(FileDownloader); //cleanup resources
end;

procedure TestTFileDownloader.DoTestSingleDownload(const URIstr: String; const SaveFilename: String; const cache: IContentCache; const ExpectedFileExists: Boolean = true);
begin
  var ContentURI := TURI.Create(URIstr);
  var SaveFilepath := TPath.Combine(ExtractFileDir(ParamStr(0)), SaveFilename);

  DoDownload(ContentURI, SaveFilepath, cache);
  CheckEquals(ExpectedFileExists, TFile.Exists(SaveFilepath), 'File ' + SaveFilepath + ' was not downloaded');
  {}TFile.Delete(SaveFilepath);
end;

procedure TestTFileDownloader.DoTestMultipleDownloads(const URIstr: String; const SaveFilename: String; const cache: IContentCache; const ExpectedFileExists: Boolean = true);
  var SaveFileDirectory, SaveFilenameWithoutExt, SaveFilenameExt: String;

  function GetNumberedSaveFilepath(const i: Integer): String;
  begin
    result := TPath.Combine(SaveFileDirectory, SaveFilenameWithoutExt + '_' + IntToStr(i) + SaveFilenameExt);
  end;

begin
  var ContentURI := TURI.Create(URIstr);
  var SaveFilepath := TPath.Combine(ExtractFileDir(ParamStr(0)), SaveFilename);

  SaveFileDirectory := TPath.GetDirectoryName(SaveFilepath);
  SaveFilenameWithoutExt := TPath.GetFileNameWithoutExtension(SaveFilepath);
  SaveFilenameExt := TPath.GetExtension(SaveFilepath);

  for var i := 0 to 10 do
    DoDownload(ContentURI, GetNumberedSaveFilepath(i), cache); //TODO: this does the downloads in sequence, should try with parallel ones too

  for var i := 0 to 10 do
    begin
    var filepath := GetNumberedSaveFilepath(i);
    CheckEquals(ExpectedFileExists, TFile.Exists(filepath), 'File ' + filepath + ' was not downloaded');
    {}TFile.Delete(filepath);
    end;
end;

{$endregion}

procedure TestTFileDownloader.TestSingleDownload;
begin
  DoTestSingleDownload(DOWNLOAD_URI_STR, SAVE_FILENAME, FContentCache);
end;

procedure TestTFileDownloader.TestSingleDownloadWithoutCache;
begin
  DoTestSingleDownload(DOWNLOAD_URI_STR_NOCACHE, SAVE_FILENAME_NOCACHE, nil);
end;

procedure TestTFileDownloader.TestMultipleDownload;
begin
  DoTestMultipleDownloads(DOWNLOAD_URI_STR, SAVE_FILENAME, FContentCache);
end;

procedure TestTFileDownloader.TestMultipleDownloadWithoutCache;
begin
  DoTestMultipleDownloads(DOWNLOAD_URI_STR_NOCACHE, SAVE_FILENAME_NOCACHE, nil);
end;

procedure TestTFileDownloader.TestWrongUrlDownload;
begin
  try
    DoTestSingleDownload(DOWNLOAD_URI_WRONG, SAVE_FILENAME_WRONGURI, FContentCache, false);
  except
    on e: ENetUriException do
      {ShowException(e, ExceptAddr)}; //silently catching exception for wrong url
  end;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTFileDownloader.Suite);
end.

