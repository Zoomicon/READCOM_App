//Description: READ-COM StoryItem Factory
//Author: George Birbilis (http://zoomicon.com)

unit READCOM.App.URLs;

interface

  function IsURI(const Value: String): Boolean;

  /// <summary>Open URL on default browser</summary>
  /// <param name="url">Absolute address of the website to open in the web browser</param>
  procedure OpenURLinBrowser(const url: string);

implementation
  {$region 'Used units'}
  uses
   FMX.Forms,
   System.SysUtils,
   System.Types,
   System.UITypes,
   System.Variants,
   System.Net.URLClient, //for TURI
  {$IF DEFINED(IOS)}
   macapi.helpers, iOSapi.Foundation, FMX.Helpers.iOS;
  {$ELSEIF DEFINED(ANDROID)}
   Androidapi.Jni.App, //to avoid "H2443 Inline function 'TAndroidHelper.GetJActivity' has not been expanded"
   Androidapi.Jni.NET, //to avoid "H2443 Inline function 'StrToJURI' has not been expanded"
   Androidapi.JNI.GraphicsContentViewText,
   Androidapi.Helpers;
  {$ELSEIF DEFINED(MACOS)}
   Posix.Stdlib;
  {$ELSEIF DEFINED(MSWINDOWS)}
   Winapi.ShellAPI, Winapi.Windows;
  {$ENDIF}
  {$endregion}

function IsURI(const Value: String): Boolean;
begin
  result := Value.StartsWith('http://', true) or Value.StartsWith('https://', true);
end;

{$region 'OpenURLinBrowser'}

procedure OpenURLinBrowser(const url: string); //Based on: https://github.com/DeveloppeurPascal/librairies/blob/master/u_urlOpen.pas that is MIT Licensed (https://github.com/DeveloppeurPascal/librairies/blob/master/LICENSE)
{$IF DEFINED(ANDROID)}
var
 Intent: JIntent;
{$ENDIF}
begin
{$IF DEFINED(ANDROID)}
 Intent := TJIntent.Create;
 Intent.setAction(TJIntent.JavaClass.ACTION_VIEW);
 Intent.setData(StrToJURI(URL));
 // SharedActivity.startActivity(Intent);
 tandroidhelper.Activity.startActivity(Intent);
{$ELSEIF DEFINED(MSWINDOWS)}
 ShellExecute(0, 'OPEN', PWideChar(URL), nil, nil, SW_SHOWNORMAL);
{$ELSEIF DEFINED(IOS)}
 SharedApplication.OpenURL(StrToNSUrl(Url));
{$ELSEIF DEFINED(MACOS)}
  _system(PAnsiChar('open ' + AnsiString(URL)));
{$ENDIF}
end;

{$endregion}

end.
