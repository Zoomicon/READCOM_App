unit READCOM.App.Globals;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls,
  System.ImageList, FMX.ImgList, FMX.SVGIconImageList;

resourcestring
  URL_HELP = 'https://github.com/Zoomicon/READCOM_App/wiki';
  URL_READCOM = 'https://www.read-com-eu.uma.es';
  STR_APP_TITLE = 'READ-COM: Reading Communities';
  STR_COMPATIBILITY_MODE = '(Compatibility mode)';
  STR_APP_VERSION = '0.3.6'; //TODO: at https://docwiki.embarcadero.com/Libraries/Sydney/en/FMX.Forms.TApplication_Properties there is no way to get application version (as defined using Options.../Version Info in the IDE for the project of the app). For Windows and OS-X there's implementation here https://delphihaven.wordpress.com/2012/12/08/retrieving-the-applications-version-string/ and for iOS and Android here https://codeverge.com/embarcadero.delphi.firemonkey/getting-application-version-and/1050163 - One could make an application helper collecting all that instead of hard-coding version info here too

type
  TGlobals = class(TDataModule)
    Lang: TLang;
    LightTheme: TStyleBook;
    DarkTheme: TStyleBook;
    SVGIconImageList: TSVGIconImageList;
  end;

var
  Globals: TGlobals;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

end.
