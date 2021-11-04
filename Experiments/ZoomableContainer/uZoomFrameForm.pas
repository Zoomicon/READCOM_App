unit uZoomFrameForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Controls.Presentation, FMX.StdCtrls, Zoomicon.Zooming.Classes,
  Zoomicon.Zooming.ZoomFrame;

type
  TForm1 = class(TForm)
    ZoomFrame: TZoomFrame;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  ZoomFrame.ZoomTo(Sender As TControl);
end;

end.
