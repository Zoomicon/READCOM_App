unit READCOM.App.Main;

interface

  procedure Main;

implementation
  uses
    FMX.Dialogs, //for ShowMessage
    FMX.Forms, //for Application
    System.SysUtils, //for FreeAndNil
    READCOM.Views.Main,
    READCOM.App.Debugging,
    READCOM.App.Globals;

procedure Main;
begin
  CheckSafeMode;

  Randomize; //initializes the built-in random number generator with a random value (obtained from the system clock)

  //ApplicationHandleException := //...
  //ApplicationShowException := //...

  Application.Initialize;

  Application.CreateForm(TGlobals, Globals);
  Application.CreateForm(TMainForm, MainForm);

  InitObjectDebugger(MainForm);

  try
    Application.Run; //TODO: should nest in try/catch with raise and in case the error is related to DirectX not supported (need to check which exceptions are thrown, say DXCanvas or something not being possible to create), Show message that user can use SHIFT key on start, then terminate the app - SEE THOUGH IF MAIN FORM'S INITIAL METHODS THAT TRY TO LOAD DEFAULT STORY ETC. ARE THE ONE'S THAT CATCH AND SHOW THE ERROR AND MAKE SURE THERE IT'S RAISED AGAIN
  finally
    FreeObjectDebugger; //must free here and not in "finalization" section of READCOM.App.Debugging to not see leaks upon exiting
  end;
end;

end.
