program Zoomicon.Downloader.Tests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}



uses
  DUnitTestRunner,
  Test.Zoomicon.Downloader in 'Test.Zoomicon.Downloader.pas',
  Zoomicon.Cache.Classes in '..\..\Zoomicon.Cache\Zoomicon.Cache.Classes.pas',
  Zoomicon.Cache.Models in '..\..\Zoomicon.Cache\Zoomicon.Cache.Models.pas',
  Zoomicon.Downloader.Classes in '..\Zoomicon.Downloader.Classes.pas',
  Zoomicon.Downloader.Models in '..\Zoomicon.Downloader.Models.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

