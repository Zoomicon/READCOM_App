//Description: READ-COM App startup
//Author: George Birbilis (http://zoomicon.com)

unit READCOM.App.Main;

interface
  uses
    System.Math, //for Min
    READCOM.App.Models; //for DEFAULT_THUMB_WIDTH, DEFAULT_THUMB_HEIGHT

  procedure Main;
  procedure ShowHelp;

  var
    StorySource: String;
    SaveThumbnail: Boolean; //=false
    ThumbnailMaxSize: Integer; //initialized at "ParseCommandLine"

implementation
  uses
    FMX.Dialogs, //for ShowMessage
    FMX.Forms, //for Application
    System.SysUtils, //for FreeAndNil
    Zoomicon.Helpers.FMX.Forms.ApplicationHelper, //for Application.ExeName
    READCOM.Views.Main,
    READCOM.App.Debugging,
    READCOM.App.Globals, //for TGlobals
    READCOM.App.Messages,
    READCOM.App.URLs; //for OpenURLinBrowser

procedure ParseCommandLine; //TODO: use https://github.com/gabr42/GpDelphiUnits/blob/master/src/GpCommandLineParser.pas or https://github.com/VSoftTechnologies/VSoft.CommandLineParser instead to parse command-line
const PARAM_THUMB = '-thumb';
begin
  if (ParamCount <> 0) then
  begin
    var param1 := ParamStr(1);

    if (param1.StartsWith(PARAM_THUMB)) and (ParamCount > 1) then //optional -thumb switch: save screenshot (after having loaded any given story or last state or default document) and close again
    begin
      SaveThumbnail := true;

      //Bounding box for thumbnail fitting is a square, using default as min of max thumb width and height if not provided
      ThumbnailMaxSize := Min(DEFAULT_THUMB_WIDTH, DEFAULT_THUMB_HEIGHT);
      var LThumbEqLength := PARAM_THUMB.Length + 1;
      if param1.Length > LThumbEqLength then
        ThumbnailMaxSize := param1.Substring(LThumbEqLength).ToInteger;

      StorySource := ParamStr(2);
    end
    else
      StorySource := ParamStr(1);
  end
end;

procedure ShowHelp;
begin
  OpenURLinBrowser(URL_HELP);
end;

procedure Main;
begin
  CheckSafeMode;

  Randomize; //initializes the built-in random number generator with a random value (obtained from the system clock)

  //ApplicationHandleException := //...
  //ApplicationShowException := //...

  Application.Initialize; //FMX app template has this before creation of form(s) - probably the order plays some role

  Application.CreateForm(TGlobals, Globals); //create before MainForm, it's a DataModule it uses
  Application.CreateForm(TMainForm, MainForm);

  InitObjectDebugger(MainForm);

  try
    ParseCommandLine;
    Application.Run; //TODO: should nest in try/catch with raise and in case the error is related to DirectX not supported (need to check which exceptions are thrown, say DXCanvas or something not being possible to create), Show message that user can use SHIFT key on start, then terminate the app - SEE THOUGH IF MAIN FORM'S INITIAL METHODS THAT TRY TO LOAD DEFAULT STORY ETC. ARE THE ONE'S THAT CATCH AND SHOW THE ERROR AND MAKE SURE THERE IT'S RAISED AGAIN
  finally
    FreeObjectDebugger; //must free here and not in "finalization" section of READCOM.App.Debugging to not see leaks upon exiting
  end;
end;

end.
