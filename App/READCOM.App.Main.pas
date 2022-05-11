//Description: READ-COM App startup
//Author: George Birbilis (http://zoomicon.com)

unit READCOM.App.Main;

interface

  procedure Main;

  var
    StorySource: String;

implementation
  uses
    Test,
    FMX.Dialogs, //for ShowMessage
    FMX.Forms, //for Application
    System.SysUtils, //for FreeAndNil
    Zoomicon.Helpers.FMX.Forms.ApplicationHelper, //for Application.ExeName
    READCOM.Views.Main,
    READCOM.App.Debugging,
    READCOM.App.Globals;

procedure ParseCommandLine;
begin
  if (ParamCount <> 0) then
    StorySource := ParamStr(1); //TODO: add optional -thumb switch to save screenshot (after having loaded any given story or last state or default document) and close again
end;

procedure Main;
begin
  CheckSafeMode;

  Randomize; //initializes the built-in random number generator with a random value (obtained from the system clock)

  //ApplicationHandleException := //...
  //ApplicationShowException := //...

  Application.Initialize; //FMX app template has this before creation of form(s) - probably the order plays some role

  {$IF NOT DEFINED(ANDROID)}
  Application.CreateForm(TGlobals, Globals);
  //Globals := TGlobals.Create(Application);
  {$ENDIF} //TODO: see what can be done about this, the MainForm is shown greyed out if this is used, in either format and irrespectively of whether it's done before or after creation of MainForm

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
