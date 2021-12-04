unit uZoomFrameForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Objects,
  Zoomicon.Zooming.FMX.ZoomFrame;

type
  TZoomFrameForm = class(TForm)
    ZoomFrame: TZoomFrame;
    procedure btnZoomClick(Sender: TObject);
  end;

var
  ZoomFrameForm: TZoomFrameForm;

implementation

{$R *.fmx}

procedure TZoomFrameForm.btnZoomClick(Sender: TObject);
begin
  ZoomFrame.ZoomTo(Sender As TControl);
end;

end.
