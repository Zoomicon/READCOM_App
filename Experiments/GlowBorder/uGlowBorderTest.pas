unit uGlowBorderTest;

interface
uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Objects,
  FMX.Effects, //for TGlowEffect
  FMX.Ani; //for TFloatAnimation

  type
    TForm1 = class(TForm)
      Rectangle1: TRectangle;
      GlowEffect1: TGlowEffect;
      FloatAnimation1: TFloatAnimation;
    private
    public
    end;

  var
    Form1: TForm1;

implementation

{$R *.fmx}

  end.
