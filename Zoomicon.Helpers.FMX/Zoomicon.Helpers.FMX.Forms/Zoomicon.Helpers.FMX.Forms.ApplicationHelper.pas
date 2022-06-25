unit Zoomicon.Helpers.FMX.Forms.ApplicationHelper;

interface
uses
  System.SysUtils, //for TProc, LongRec
  FMX.Forms; //for TApplication

type
  TApplicationHelper = class helper for TApplication
  public
    function ExeName: String; //TODO: make Property?
    function AppVersion: String; //TODO: make Property?
    class procedure Confirm(const Prompt: String; const SetConfirmationResult: TProc<Boolean>);
  end;

implementation
  uses
    {$IF DEFINED(MSWINDOWS)}
    Windows,
    {$ELSEIF DEFINED(MACOS)}
    Macapi.CoreFoundation,
    {$ELSEIF DEFINED(IOS)}
    iOSapi.Foundation, Macapi.ObjectiveC,
    {$ELSEIF DEFINED(ANDROID)}
    FMX.Helpers.Android,
    Androidapi.Helpers, //for TAndroidHelper, JStringToString
    Androidapi.JNI.GraphicsContentViewText, //to avoid "H2443 Inline function 'SharedActivityContext' has not been expanded"
    Androidapi.JNI.JavaTypes, //to avoid "H2443 Inline function 'JStringToString' has not been expanded"
    {$ENDIF}
    System.UITypes, //for TMsgDlgType
    FMX.DialogService, //for TDialogService
    FMX.Dialogs; //for mbYesNo

{$REGION 'TApplicationHelper'}

{$region 'ExeName'}

function TApplicationHelper.ExeName: String; //from https://gist.github.com/freeonterminate/2f7b2e29e40fa30ed3c4
begin
  Result := ParamStr(0);

  {$IF DEFINED(ANDROID)}
  if (Result.IsEmpty) then
    Result := JStringToString(TAndroidHelper.Context.getPackageCodePath);
  {$ENDIF}
end;

{$endregion}

{$region 'Confirm'}

class procedure TApplicationHelper.Confirm(const Prompt: String; const SetConfirmationResult: TProc<Boolean>); //based on https://stackoverflow.com/questions/42852945/delphi-correctly-displaying-a-message-dialog-in-firemonkey-and-returning-the-m
begin
  with TDialogService do
  begin
    PreferredMode := TPreferredMode.Platform;
    MessageDialog(Prompt, TMsgDlgType.mtConfirmation, mbYesNo, TMsgDlgBtn.mbNo, 0,
      procedure(const AResult: TModalResult)
      begin
        //Note: assuming this is executed on the main/UI thread later on, so we just call the "SetResult" callback procedure passing it the dialog result value
        case AResult of
          mrYes: SetConfirmationResult(true);
          mrNo:  SetConfirmationResult(false);
        end;
      end
    );
  end;
end;

{$endregion}

{$region 'Application Version'} //see https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Version_Info

{$IF DEFINED(MSWINDOWS)} //based on https://delphihaven.wordpress.com/2012/12/08/retrieving-the-applications-version-string/

function GetAppVersionStr2: string;
var
  Rec: LongRec;
begin
  Rec := LongRec(GetFileVersion(Application.ExeName));
  Result := Format('%d.%d', [Rec.Hi, Rec.Lo]);
end;

function GetAppVersionStr3: string;
var
  Exe: String;
  Size, Handle: DWORD;
  Buffer: TBytes;
  FixedPtr: PVSFixedFileInfo;
begin
  Exe := Application.ExeName;
  Size := GetFileVersionInfoSize(PChar(Exe), Handle);
  if (Size = 0) then
    RaiseLastOSError;
  SetLength(Buffer, Size);
  if not GetFileVersionInfo(PChar(Exe), Handle, Size, Buffer) then
    RaiseLastOSError;
  if not VerQueryValue(Buffer, '\', Pointer(FixedPtr), Size) then
    RaiseLastOSError;
  Result := Format('%d.%d.%d',
    [LongRec(FixedPtr.dwFileVersionMS).Hi,   //major
     LongRec(FixedPtr.dwFileVersionMS).Lo,   //minor
     LongRec(FixedPtr.dwFileVersionLS).Hi]); //release
end;

function GetAppVersionStr4: string;
var
  Exe: String;
  Size, Handle: DWORD;
  Buffer: TBytes;
  FixedPtr: PVSFixedFileInfo;
begin
  Exe := Application.ExeName;
  Size := GetFileVersionInfoSize(PChar(Exe), Handle);
  if (Size = 0) then
    RaiseLastOSError;
  SetLength(Buffer, Size);
  if not GetFileVersionInfo(PChar(Exe), Handle, Size, Buffer) then
    RaiseLastOSError;
  if not VerQueryValue(Buffer, '\', Pointer(FixedPtr), Size) then
    RaiseLastOSError;
  Result := Format('%d.%d.%d.%d',
    [LongRec(FixedPtr.dwFileVersionMS).Hi,   //major
     LongRec(FixedPtr.dwFileVersionMS).Lo,   //minor
     LongRec(FixedPtr.dwFileVersionLS).Hi,   //release
     LongRec(FixedPtr.dwFileVersionLS).Lo]); //build
end;

{$ELSEIF DEFINED(MACOS)} //based on https://delphihaven.wordpress.com/2012/12/08/retrieving-the-applications-version-string/

function GetAppVersionStr3: string;
var
  CFStr: CFStringRef;
  Range: CFRange;
begin
  CFStr := CFBundleGetValueForInfoDictionaryKey(CFBundleGetMainBundle, kCFBundleVersionKey);
  Range.location := 0;
  Range.length := CFStringGetLength(CFStr);
  SetLength(Result, Range.length);
  CFStringGetCharacters(CFStr, Range, PChar(Result));
end;

{$ELSEIF DEFINED(IOS)} //based on https://codeverge.com/embarcadero.delphi.firemonkey/getting-application-version-and/1050163

function GetAppVersionStr4: string;
var
   AppKey: Pointer;
   AppBundle: NSBundle;
   BuildStr : NSString;
begin
   AppKey := (NSSTR('CFBundleVersion') as ILocalObject).GetObjectID; //Note: iOS CFBundleVersion is the full version, CFBundleShortVersionString would be the major.minor. This is a little different than xCode projects tend to be where the build number is just the build number it seems
   AppBundle := TNSBundle.Wrap(TNSBundle.OCClass.mainBundle);
   BuildStr := TNSString.Wrap(AppBundle.infoDictionary.objectForKey(AppKey));
   Result := UTF8ToString(BuildStr.UTF8String);
end;

{$ELSEIF DEFINED(ANDROID)} //based on https://codeverge.com/embarcadero.delphi.firemonkey/getting-application-version-and/1050163

function GetAppVersionStr2: string;
begin
  with TAndroidHelper.Context do
    Result := JStringToString(getPackageManager.getPackageInfo(getPackageName, 0).versionName); //Note: can throw JPackageManager_NameNotFoundException, but Delphi should always include a version name
end;

function GetAppVersionStr3: string;
begin
  result := GetAppVersionStr2 + '.0';
end;

{$ELSEIF DEFINED(LINUX)}

//see https://stackoverflow.com/questions/44600597/delphi-linux64-how-to-retrieve-the-version-information-set-by-project-options-v and https://wiki.freepascal.org/Show_Application_Title,_Version,_and_Company#Linux

{$ENDIF}

function TApplicationHelper.AppVersion: String;
begin
  Result := GetAppVersionStr3; //TODO: this will fail on non-windows platforms that don't define this, should have extra GetAppVersionStr3 functions for those that don't have such to add .0 or trim .xx from end as needed if less or more digits are provided by those platforms (ELSE HAVE GetMajorVersion, GetMinorVersion etc. that return strings or '0' if they aren't implemented and concat here the major+minor+release ones to get 3-digit version)
end;

{$endregion}

{$ENDREGION}

end.

