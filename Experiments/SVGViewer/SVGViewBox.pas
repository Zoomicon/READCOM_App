unit SVGViewBox;

interface

uses
  System.SysUtils, System.Variants, System.Classes,
  Fmx.Graphics, Fmx.Objects, Fmx.Controls, Fmx.ExtCtrls,
  SVGInterfaces;

type
  TSVGPaintBox = class(TPaintBox)
  protected
    FGrayScale: Boolean;
    FSVG: ISVG;
    procedure Paint; override;
  public
    procedure InitViewer(const ATitle: string; const SVGFactory : ISVGFactory);
    procedure DrawFile(const AFileName: string);
  end;

implementation

uses
  System.Types;

{$region 'TSVGPaintBox'}

procedure TSVGPaintBox.DrawFile(const AFileName: string);
begin
  try
    FSVG.LoadFromFile(AFileName);
  except
    On E: ESVGException do ;
    else raise;
  end;
  //Invalidate;
end;

procedure TSVGPaintBox.InitViewer(const ATitle: string; const SVGFactory: ISVGFactory);
begin
  SetGlobalSvgFactory(SVGFactory);
  FSVG := GlobalSvgFactory.NewSvg;
end;

procedure TSVGPaintBox.Paint;
begin
  FSVG.Grayscale := FGrayScale;
  //FSVG.PaintTo(Canvas{.Handle}, RectF(0, 0, Width, Height), True);
end;

{$endregion}

end.
