unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Controls.Presentation, FMX.StdCtrls, Zoomicon.Puzzler.Classes;

type

  TForm2 = class(TForm)
    Button2: TButton;
    Button1: TButton;
    Link1: TLink;
    Arrow1: TArrow;
    procedure Button1Click(Sender: TObject);
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.Button1Click(Sender: TObject);
begin
  Link1.Target := Button1;
  Arrow1.SourcePoint := PointF(0, 200);
  Arrow1.TargetPoint := PointF(100, 200);
end;

end.
