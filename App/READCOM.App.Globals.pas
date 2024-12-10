unit READCOM.App.Globals;

interface

uses
  System.SysUtils,
  System.Classes,
  System.ImageList,
  FMX.Types,
  FMX.Controls,
  FMX.ImgList,
  FMX.SVGIconImageList;

type

  TGlobals = class(TDataModule)
    Lang: TLang;
    SVGIconImageList: TSVGIconImageList;
  end;

var
  Globals: TGlobals;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

end.
