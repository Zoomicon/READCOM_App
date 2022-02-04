unit uZoomedLayoutForm;

//TODO: this doesn't seem to show content (button)

interface

uses
  Zoomicon.Zooming.FMX, //for TZoomedLayout
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  TZoomedLayoutForm = class(TForm)
    ScrollBox1: TScrollBox;
    ZoomedLayout1: TZoomedLayout;
    Button1: TButton;
  private
    {Private declarations}
  public
    {Public declarations}
  end;

var
  ZoomedLayoutForm: TZoomedLayoutForm;

implementation

{$R *.fmx}

end.
