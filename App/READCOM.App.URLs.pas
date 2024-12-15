//Description: URL utilities
//Author: George Birbilis (http://zoomicon.com)

unit READCOM.App.URLs;

interface
  uses
    {$IF DEFINED(IOS)}
    Macapi.ObjectiveC,
    iOSapi.Foundation,
    iOSapi.UIKit,
    {$ENDIF}
    FMX.Forms, //for TApplication
    Zoomicon.Helpers.FMX.Forms.ApplicationHelper;

  function IsURI(const Value: String): Boolean;

  type
    TUIApplicationOpenURLCompletionHandler = procedure(success: Boolean) of object;

    TApplicationOpenURLHelper = class helper(TApplicationHelper) for TApplication
    public
      function OpenURL(URL: string): Boolean;
    end;

  {$IF DEFINED(IOS)}
  //from: https://github.com/DelphiWorlds/HowTo/blob/main/Demos/OpenURL/Unit1.pas
  type

    UIApplicationEx = interface(UIApplication)
      ['{F977B9BE-C037-45FC-BBFA-3069C5BEB3BB}']
      procedure openURL(url: NSURL; options: NSDictionary; completionHandler: TUIApplicationOpenURLCompletionHandler); cdecl; overload;
    end;

    TUIApplicationEx = class(TOCGenericImport<UIApplicationClass, UIApplicationEx>)
    end;

  function SharedApplicationEx: UIApplicationEx;
  {$ENDIF}

implementation
  {$region 'Used units'}
  uses
   System.SysUtils,
   System.Types,
   System.UITypes,
   System.Variants,
  {$IF DEFINED(MSWINDOWS)}
   Winapi.ShellAPI,
   Winapi.Windows;
  {$ELSEIF DEFINED(ANDROID)}
   Androidapi.Jni.App, //to avoid DCC Hint "H2443 Inline function 'TAndroidHelper.GetJActivity' has not been expanded"
   Androidapi.Jni.NET, //to avoid DCC Hint "H2443 Inline function 'StrToJURI' has not been expanded"
   Androidapi.JNI.GraphicsContentViewText,
   Androidapi.Helpers;
  {$ELSEIF DEFINED(IOS)}
   Macapi.helpers;
   //FMX.Helpers.iOS;
  {$ELSEIF DEFINED(MACOS)} //need to have this after DEFINED(IOS) check
   Posix.Stdlib;
  {$ENDIF}
  {$endregion}

  function IsURI(const Value: String): Boolean;
  begin
    Result := Value.StartsWith('http://', true) or Value.StartsWith('https://', true); //TODO: a bit naive implementation
  end;

  {$IF DEFINED(IOS)}
  function SharedApplicationEx: UIApplicationEx;
  begin
    Result := TUIApplicationEx.Wrap(TUIApplication.OCClass.sharedApplication) as UIApplicationEx;
  end;
  {$ENDIF}

  function TApplicationOpenURLHelper.OpenURL(URL: string): Boolean;
  begin
    try
      {$IF DEFINED(MSWINDOWS)}
      ShellExecute(0, 'OPEN', PWideChar(URL), nil, nil, SW_SHOWNORMAL);

      {$ELSEIF DEFINED(ANDROID)}
      var Intent := TJIntent.Create;
      Intent.setAction(TJIntent.JavaClass.ACTION_VIEW);
      Intent.setData(StrToJURI(URL));
      TAndroidHelper.Activity.startActivity(Intent); //SharedActivity.startActivity(Intent);

      {$ELSEIF DEFINED(IOS)}
      //SharedApplication.openURL(StrToNSUrl(URL)); //doesn't work in recent iOS, deprecated and need to use open ( https://developer.apple.com/documentation/uikit/uiapplication/openurl(_:) points to https://developer.apple.com/documentation/uikit/uiapplication/open(_:options:completionhandler:) )
      SharedApplicationEx.openURL(StrToNSUrl(URL), nil, nil);

      {$ELSEIF DEFINED(MACOS)} //need to have this after DEFINED(IOS) check
      _system(PAnsiChar('open ' + AnsiString(URL)));

      {$ENDIF}

      Result := True; //TODO: iOS version has optional completionHandler, maybe others too, maybe use that
    except
      Result := False;
    end;
  end;

end.
