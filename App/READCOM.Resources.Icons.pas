unit READCOM.Resources.Icons;

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

  TIcons = class(TDataModule)
    SVGIconImageList: TSVGIconImageList;
  end;

var
  Icons: TIcons;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

end.
