unit READCOM.App.Debugging;

interface
  uses
    System.Classes, //for TComponent
    System.SysUtils; //for Exception, FreeAndNil, Format

  procedure CheckSafeMode;

  procedure InitObjectDebugger(const AOwner: TComponent);
  procedure FreeObjectDebugger;
  procedure ToggleObjectDebuggerVisibility;

  procedure Log(const Format: String; const Args: array of const); overload;
  procedure Log(const Msg: String); overload;
  procedure Log(const E: Exception); overload;

implementation
  uses
  {$IF Defined(MSWINDOWS)}Windows,{$ENDIF} //for GetKeyState
  {$IFDEF DEBUG}
    {$IF Defined(MSWINDOWS)}
    CodeSiteLogging,
    Zoomicon.Helpers.FMX.Forms.ApplicationHelper, //for Application.ExeName
    {$ENDIF }
  FormMessage,
  ObjectDebuggerFMXForm,
  {$ENDIF}
  FMX.Forms, //for Application
  FMX.Types; //for GlobalUseDX

{$region 'Graphics SafeMode'}

procedure CheckSafeMode;
begin
  {$IF Defined(MSWINDOWS)}
  if (GetKeyState(VK_SHIFT) < 0) then
  begin
    //GlobalUseDX10 := False; //must do before Application.Initialize //use DX9 instead of DX10
    GlobalUseDX := False; //must do before Application.Initialize //use GDI, no h/w acceleration
  end;
  {$ENDIF}
end;

{$endregion}

{$region 'Object inspector'}

procedure InitObjectDebugger(const AOwner: TComponent);
begin
  {$IFDEF DEBUG}
  ObjectDebuggerFMXForm1 := TObjectDebuggerFMXForm.Create(AOwner); //don't Show the object inspector, MainForm shows/hides it at F11 keypress
  MessageForm := TMessageForm.Create(ObjectDebuggerFMXForm1);
  {$ENDIF}
end;

procedure FreeObjectDebugger;
begin
  {$IFDEF DEBUG}
  FreeAndNil(ObjectDebuggerFMXForm1); //the object debugger anyway seems to be leaking objects (if different objects are selected)
  {$ENDIF}
end;

procedure ToggleObjectDebuggerVisibility;
begin
  {$IFDEF DEBUG}
  with ObjectDebuggerFMXForm1 do
    Visible := not Visible;
  {$ENDIF}
end;

{$endregion}

{$region 'Logging'}

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

procedure Log(const Format: String; const Args: array of const);
begin
  Log(System.SysUtils.Format(Format, Args));
end;

procedure Log(const Msg: String);
begin
  {$IFDEF DEBUG}{$IF defined(MSWINDOWS)}
  try
    CodeSite.Send(Msg);
  except
    //NOP (seems CodeSite.Send(String) fails with very long strings as is the case wich serialized object trees that contain lots of TImages)
  end;
  {$ENDIF}{$ENDIF}
end;

procedure Log(const E: Exception);
begin
  {$IFDEF DEBUG}{$IF defined(MSWINDOWS)}
  try
    CodeSite.SendException(E);
  except
    //NOP (just in case it fails)
  end;
  {$ENDIF}{$ENDIF}
end;

{$endregion}

initialization

  {$IFDEF DEBUG}
  {$IF Defined(MSWINDOWS)}EnableCodeSite;{$ENDIF} //TODO: is CodeSite indeed only for Windows? Couldn't compile for other platforms
  ReportMemoryLeaksOnShutdown := True;
  {$ELSE}
  //CodeSite.Enabled := False; //we've removed CodeSite with compiler defines in production version so we can't use that here
  {$ENDIF}

end.
