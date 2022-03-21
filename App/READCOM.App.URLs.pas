//Based on: https://github.com/DeveloppeurPascal/librairies/blob/master/u_urlOpen.pas that is MIT Licensed (https://github.com/DeveloppeurPascal/librairies/blob/master/LICENSE)

unit READCOM.App.URLs;

interface
  uses
    System.Classes, //for TMemoryStream
    Zoomicon.Cache.Models; //for IFileCache

  const
    DOWNLOAD_TIMEOUT: Cardinal = 10000; //10 sec (can also use INFINITE)

  /// <summary>Open URL on default browser</summary>
  /// <param name="url">Absolute address of the website to open in the web browser</param>
  procedure OpenURLinBrowser(const url: string);

  function DownloadFileWithFallbackCache(const url: string): TMemoryStream;

  var
    FileCache: IFileCache;

implementation
  uses
   FMX.Forms,
   System.SysUtils,
   System.Types,
   System.UITypes,
   System.Variants,
   System.Net.URLClient, //for TURI
   Zoomicon.Cache.Classes, //for TFileCache
   Zoomicon.Downloader.Classes, //for TFileDownloader
   READCOM.App.Globals, //for FileCache
  {$IF Defined(IOS)}
   macapi.helpers, iOSapi.Foundation, FMX.Helpers.iOS;
  {$ELSEIF Defined(ANDROID)}
   Androidapi.Jni.App, //to avoid "H2443 Inline function 'TAndroidHelper.GetJActivity' has not been expanded"
   Androidapi.Jni.NET, //to avoid "H2443 Inline function 'StrToJURI' has not been expanded"
   Androidapi.JNI.GraphicsContentViewText,
   Androidapi.Helpers;
  {$ELSEIF Defined(MACOS)}
   Posix.Stdlib;
  {$ELSEIF Defined(MSWINDOWS)}
   Winapi.ShellAPI, Winapi.Windows;
  {$ENDIF}

procedure OpenURLinBrowser(const url: string);
{$IF Defined(ANDROID)}
var
 Intent: JIntent;
{$ENDIF}
begin
{$IF Defined(ANDROID)}
 Intent := TJIntent.Create;
 Intent.setAction(TJIntent.JavaClass.ACTION_VIEW);
 Intent.setData(StrToJURI(URL));
 // SharedActivity.startActivity(Intent);
 tandroidhelper.Activity.startActivity(Intent);
{$ELSEIF Defined(MSWINDOWS)}
 ShellExecute(0, 'OPEN', PWideChar(URL), nil, nil, SW_SHOWNORMAL);
{$ELSEIF Defined(IOS)}
 SharedApplication.OpenURL(StrToNSUrl(Url));
{$ELSEIF Defined(MACOS)}
  _system(PAnsiChar('open ' + AnsiString(URL)));
{$ENDIF}
end;

function DownloadFileWithFallbackCache(const url: string): TMemoryStream;
begin
  result := TMemoryStream.Create; //caller should free this
  var FileDownloader := TDownloader.Create(Application.MainForm, TURI.Create(url), result, FileCache, true); //AutoStart
  FileDownloader.OnlyFallbackCache := true; //would use this if we only wanted to fallback to cache in case of download errors / offline case
  FileDownloader.WaitForDownload(DOWNLOAD_TIMEOUT); //Note: this can freeze the main thread
  FreeAndNil(FileDownloader);
end;

initialization
  FileCache := TFileCache.Create;

end.
