unit READCOM.App.Main;

interface

  procedure Main;

implementation
  uses
    {$IFDEF DEBUG}
    {$IF Defined(MSWINDOWS)}
    CodeSiteLogging,
    Zoomicon.Helpers.FMX.Forms.ApplicationHelper, //for Application.ExeName
    Windows, //for GetKeyState
    {$ENDIF }
    FormMessage,
    ObjectDebuggerFMXForm,
    {$ENDIF}
    FMX.Dialogs, //for ShowMessage
    FMX.Types, //for GlobalUseDX
    FMX.Forms, //for Application
    System.SysUtils, //for FreeAndNil
    READCOM.Views.Main,
    READCOM.App.Globals;

{$IFDEF DEBUG}{$IF Defined(MSWINDOWS)}

procedure EnableCodeSite;
begin
  CodeSite.Enabled := CodeSite.Installed;
  if CodeSite.Enabled then
  begin
    if CodeSite.Enabled then
    begin
      var Destination := TCodeSiteDestination.Create(Application);
      with Destination do
        begin
        with LogFile do
          begin
          Active := True;
          FileName := ChangeFileExt(ExtractFileName(Application.ExeName), '.csl');
          FilePath := '$(MyDocs)\My CodeSite Files\Logs\';
          end;
        Viewer.Active := True; // also show Live Viewer
        end;
      CodeSite.Destination := Destination;
      CodeSite.Clear
    end;
  end;
end;

{$ENDIF}{$ENDIF}

procedure Main;
begin

  {$IF Defined(MSWINDOWS)}
  if (GetKeyState(VK_SHIFT) < 0) then
  begin
    //GlobalUseDX10 := False; //must do before Application.Initialize //use DX9 instead of DX10
    GlobalUseDX := False; //must do before Application.Initialize //use GDI, no h/w acceleration
  end;
  {$ENDIF}

  {$IFDEF DEBUG}
  {$IF Defined(MSWINDOWS)}EnableCodeSite;{$ENDIF} //TODO: is CodeSite indeed only for Windows? Couldn't compile for other platforms
  ReportMemoryLeaksOnShutdown := True;
  {$ELSE}
  //CodeSite.Enabled := False; //we've removed CodeSite with compiler defines in production version so we can't use that here
  {$ENDIF}

  Randomize; //initializes the built-in random number generator with a random value (obtained from the system clock)

  //ApplicationHandleException := //...
  //ApplicationShowException := //...

  Application.Initialize;

  Application.CreateForm(TGlobals, Globals);
  Application.CreateForm(TMainForm, MainForm);

  {$IFDEF DEBUG}
  ObjectDebuggerFMXForm1 := TObjectDebuggerFMXForm.Create(MainForm); //don't Show the object inspector, MainForm shows/hides it at F11 keypress
  MessageForm := TMessageForm.Create(ObjectDebuggerFMXForm1);
  {$ENDIF}

  try
    Application.Run; //TODO: should nest in try/catch with raise and in case the error is related to DirectX not supported (need to check which exceptions are thrown, say DXCanvas or something not being possible to create), Show message that user can use SHIFT key on start, then terminate the app - SEE THOUGH IF MAIN FORM'S INITIAL METHODS THAT TRY TO LOAD DEFAULT STORY ETC. ARE THE ONE'S THAT CATCH AND SHOW THE ERROR AND MAKE SURE THERE IT'S RAISED AGAIN
  finally
    {$IFDEF DEBUG}
    FreeAndNil(ObjectDebuggerFMXForm1); //the object debugger anyway seems to be leaking objects (if different objects are selected)
    {$ENDIF}
  end;
end;

end.
