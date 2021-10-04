unit Test.Zoomicon.Downloader; //Delphi DUnit Test Case

interface
  uses
    TestFramework, Zoomicon.Downloader.Models, System.Net.URLClient,
    Zoomicon.Downloader.Classes, System.Classes;

  const
    DOWNLOAD_URI = 'https://raw.githubusercontent.com/zoomicon/READCOM_Gallery/master/Gallery/README.md';
    DOWNLOAD_TIMEOUT: Cardinal = 10000; //10 sec (can also use INFINITE)
    SAVE_FILENAME = 'Test.Downloader.README.md';

  type
    // Test methods for class TFileDownloader

    TestTFileDownloader = class(TTestCase)
    strict private
    public
      procedure SetUp; override;
      procedure TearDown; override;
      procedure DoDownload(const TheContentURI: TURI; const TheSaveFilepath: String);
    published
      procedure TestSingleDownload;
      procedure TestMultipleDownload;
    end;

implementation
  uses
    System.SysUtils,
    System.IOUtils;

{$region 'SetUp / TearDown'}

procedure TestTFileDownloader.SetUp;
begin
end;

procedure TestTFileDownloader.TearDown;
begin
end;

{$endregion}

{$region 'Helpers'}

procedure TestTFileDownloader.DoDownload(const TheContentURI: TURI; const TheSaveFilepath: String);
begin
  if FileExists(TheSaveFilepath) then
    TFile.Delete(TheSaveFilepath); //remove download file if existing so that we can check if it was created later

  var FileDownloader := TFileDownloader.Create(TheContentURI, TheSaveFilepath);
  FileDownloader.Start;
  FileDownloader.WaitForDownload(DOWNLOAD_TIMEOUT); //Note: this can freeze the main thread
end;

{$endregion}

procedure TestTFileDownloader.TestSingleDownload;
begin
  var ContentURI := TURI.Create(DOWNLOAD_URI);
  var SaveFilepath := TPath.Combine(ExtractFileDir(ParamStr(0)), SAVE_FILENAME);

  DoDownload(ContentURI, SaveFilepath);
  CheckTrue(TFile.Exists(SaveFilepath), 'File ' + SaveFilepath + ' was not downloaded');
end;

procedure TestTFileDownloader.TestMultipleDownload;
  var SaveFileDirectory, SaveFilenameWithoutExt, SaveFilenameExt: String;

  function GetNumberedSaveFilepath(const i: Integer): String;
  begin
    result := TPath.Combine(TPath.Combine(SaveFileDirectory, SaveFilenameWithoutExt) + '_' + IntToStr(i), SaveFilenameExt);
  end;

begin
  var ContentURI := TURI.Create(DOWNLOAD_URI);
  var SaveFilepath := TPath.Combine(ExtractFileDir(ParamStr(0)), SAVE_FILENAME);

  SaveFileDirectory := TPath.GetDirectoryName(SaveFilepath);
  SaveFilenameWithoutExt := TPath.GetFileNameWithoutExtension(SaveFilepath);
  SaveFilenameExt := TPath.GetExtension(SaveFilepath);

  for var i := 0 to 10 do
    DoDownload(ContentURI, GetNumberedSaveFilepath(i));

  for var i := 0 to 10 do
    begin
    var filename := GetNumberedSaveFilepath(i);
    CheckTrue(TFile.Exists(filename), 'File ' + filename + ' was not downloaded');
    end;
end;


initialization
  // Register any test cases with the test runner
  RegisterTest(TestTFileDownloader.Suite);
end.

