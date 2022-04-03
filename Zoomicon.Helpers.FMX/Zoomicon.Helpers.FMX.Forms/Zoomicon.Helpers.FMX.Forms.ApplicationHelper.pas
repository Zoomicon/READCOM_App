unit Zoomicon.Helpers.FMX.Forms.ApplicationHelper;

interface
uses
  FMX.Forms; //for TApplication

type
  TApplicationHelper = class helper for TApplication
  public
    function ExeName: String;
    class function Confirm(Prompt: String): Boolean;
  end;

implementation
  uses
    {$IFDEF ANDROID}
    System.SysUtils, //TODO: is this needed?
    Androidapi.Helpers, //for TAndroidHelper, JStringToString
    Androidapi.Jni.JavaTypes, //to avoid "H2443 Inline function 'JStringToString' has not been expanded"
    {$ENDIF}
    System.UITypes, //for TMsgDlgType
    FMX.DialogService, //for TDialogService
    FMX.Dialogs; //for mbYesNo

{$region 'TApplicationHelper'}

//from https://gist.github.com/freeonterminate/2f7b2e29e40fa30ed3c4
function TApplicationHelper.ExeName: String;
begin
  Result := ParamStr(0);

  {$IFDEF ANDROID}
  if (Result.IsEmpty) then
    Result := JStringToString(TAndroidHelper.Context.getPackageCodePath);
  {$ENDIF}
end;

//based on https://stackoverflow.com/questions/42852945/delphi-correctly-displaying-a-message-dialog-in-firemonkey-and-returning-the-m
class function TApplicationHelper.Confirm(Prompt: String): Boolean;
begin
  var LResult := False;
  with TDialogService do
  begin
    PreferredMode := TPreferredMode.Platform;
    MessageDialog(Prompt, TMsgDlgType.mtConfirmation, mbYesNo, TMsgDlgBtn.mbNo, 0,
      procedure(const AResult: TModalResult)
      begin
        case AResult of
          mrYes: LResult := True;
          mrNo:  LResult := False;
        end;
      end
    );
  end;

  result := LResult;
end;

{$endregion}

end.

