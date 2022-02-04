unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  uClickableFrame, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts;

type
  TMainForm = class(TForm)
    ClickableFrame1: TClickableFrame;
    ClickableFrame2: TClickableFrame;
    ClickableFrame3: TClickableFrame;
    btnEnableChildren: TButton;
    Layout: TLayout;
    procedure btnEnableChildrenClick(Sender: TObject);
  private
    {Private declarations}
  public
    {Public declarations}
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.btnEnableChildrenClick(Sender: TObject);
begin
  for var Control in Layout.Controls do
    Control.Enabled := true;
end;

end.
