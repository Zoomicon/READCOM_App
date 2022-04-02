unit READCOM.App.Globals;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls,
  System.ImageList, FMX.ImgList, FMX.SVGIconImageList;

resourcestring
  URL_HELP = 'https://github.com/Zoomicon/READCOM_App/wiki';
  URL_READCOM = 'https://www.read-com-eu.uma.es';
  STR_APP_TITLE = 'READ-COM: Reading Communities';
  STR_APP_VERSION = '0.3.1';

type
  TGlobals = class(TDataModule)
    DefaultStyleBook: TStyleBook;
    Lang: TLang;
    SVGIconImageList: TSVGIconImageList;
  end;

var
  Globals: TGlobals;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

end.
