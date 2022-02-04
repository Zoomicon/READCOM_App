unit READCOM.App.Globals;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls,
  System.ImageList, FMX.ImgList, FMX.SVGIconImageList;

const
  URL_HELP = 'https://github.com/Zoomicon/READCOM_App/wiki';

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
