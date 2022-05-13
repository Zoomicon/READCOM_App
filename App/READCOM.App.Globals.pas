unit READCOM.App.Globals;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls,
  System.ImageList, FMX.ImgList, FMX.SVGIconImageList;

resourcestring
  URL_HELP = 'https://github.com/Zoomicon/READCOM_App/wiki';
  URL_READCOM = 'https://www.read-com-eu.uma.es';
  STR_APP_TITLE = 'READ-COM: Reading Communities';
  STR_COMPATIBILITY_MODE = '[Compatibility mode]';

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
