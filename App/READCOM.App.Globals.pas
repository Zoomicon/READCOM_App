unit READCOM.App.Globals;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls,
  System.ImageList, FMX.ImgList, FMX.SVGIconImageList;

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
