unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls,
  uContainerFrame;

type
  TMainForm = class(TForm)
    BackgroundRectangle: TRectangle;
    cbEditMode: TCheckBox;
    btnAdd: TButton;
    ContainerFrame: TContainerFrame;
    cbProportional: TCheckBox;
    procedure btnAddClick(Sender: TObject);
    procedure cbEditModeChange(Sender: TObject);
    procedure cbProportionalChange(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

{$REGION 'Events'}

procedure TMainForm.btnAddClick(Sender: TObject);
begin
  with ContainerFrame do
    begin
      Proportional := cbProportional.IsChecked;
      EditMode := cbEditMode.IsChecked;
    end;
  ContainerFrame.AddContentFrame;
end;

procedure TMainForm.cbProportionalChange(Sender: TObject);
begin
  ContainerFrame.Proportional := cbProportional.IsChecked;
end;

procedure TMainForm.cbEditModechange(Sender: TObject);
begin
  ContainerFrame.EditMode := cbEditMode.IsChecked;
end;

{$ENDREGION}

end.
