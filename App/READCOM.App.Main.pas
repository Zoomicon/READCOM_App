unit READCOM.App.Main;

interface

  procedure Main;

implementation
  uses
    {$IFDEF DEBUG}
    FormMessage,
    ObjectDebuggerFMXForm,
    {$ENDIF}
    FMX.Forms, //for Application
    System.SysUtils, //for FreeAndNil
    READCOM.Views.Main,
    READCOM.App.Globals;

{$IFDEF DEBUG}{$IFDEF WINDOWS}

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
  //GlobalUseDX10 := False; //must do before Application.Initialize //use DX9 instead of DX10 //TODO: remove, compatibility testing (should have some flags in app's settings, see if there's some Delphi lib to have such persistent options for changing/keeping compatibility settings)
  //GlobalUseDX := False; //must do before Application.Initialize //use GDI, no h/w acceleration //TODO: remove, compatibility testing (should have some flags in app's settings, see if there's some Delphi lib to have such persistent options for changing/keeping compatibility settings)

  {$IFDEF DEBUG}
  {$IFDEF WINDOWS}EnableCodeSite;{$ENDIF}
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
    Application.Run;
  finally
    {$IFDEF DEBUG}
    FreeAndNil(ObjectDebuggerFMXForm1); //the object debugger anyway seems to be leaking objects (if different objects are selected)
    {$ENDIF}
  end;
end;

end.