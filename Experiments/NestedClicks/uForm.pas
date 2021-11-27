unit uForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  uClickableFrame;

type
  TForm2 = class(TForm)
    ClickableFrame1: TClickableFrame;
    ClickableFrame2: TClickableFrame;
    ClickableFrame3: TClickableFrame;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

end.
