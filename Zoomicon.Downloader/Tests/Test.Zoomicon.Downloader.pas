unit Test.Zoomicon.Downloader; //Delphi DUnit Test Case

interface
  uses
    TestFramework, Zoomicon.Downloader.Models, System.Net.URLClient,
    Zoomicon.Downloader.Classes, System.Classes;

  const
    DOWNLOAD_URI = 'https://raw.githubusercontent.com/zoomicon/READCOM_Gallery/master/Gallery/README.md';
    SAVE_FILENAME = 'Test.Downloader.README.md';

  type
    // Test methods for class TFileDownloader

    TestTFileDownloader = class(TTestCase)
    strict private
      FFileDownloader: TFileDownloader;
      FContentURI: TURI;
      FSaveFilepath: String;
    public
      procedure SetUp; override;
      procedure TearDown; override;
    published
      procedure TestStart;
    end;

implementation
  uses
    System.SysUtils,
    System.IOUtils;

procedure TestTFileDownloader.SetUp;
begin
  FContentURI := TURI.Create(DOWNLOAD_URI);
  FSaveFilepath := TPath.Combine(ExtractFileDir(ParamStr(0)), SAVE_FILENAME);
end;

procedure TestTFileDownloader.TearDown;
begin
  FreeAndNil(FFileDownloader);
end;

procedure TestTFileDownloader.TestStart;
begin
  if FileExists(FSaveFilePath) then
    TFile.Delete(FSaveFilepath); //remove download file if existing so that we can check if it was created later

  FFileDownloader := TFileDownloader.Create(FContentURI, FSaveFilepath);
  //FFileDownloader.Start; //TODO: maybe set TFileDownloader to not autostart? (and have the factory method do start instead?)
  TThread.Sleep(2000); //TODO: use TEvent in DownloaderThread and WaitFor (add a WaitFor method in IDownloader)

  CheckTrue(TFile.Exists(FSaveFilepath), 'File wasn''t downloaded');
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTFileDownloader.Suite);
end.

