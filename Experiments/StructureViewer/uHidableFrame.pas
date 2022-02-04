unit uHidableFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects;

type
  THidableFrame = class(TFrame)
    LabelName: TLabel;
    Rectangle: TRectangle;
    cbVisible: TCheckBox;
    btnShowChildren: TButton;
    UpdateTimer: TTimer;
    procedure cbVisibleChange(Sender: TObject);
    procedure btnShowChildrenClick(Sender: TObject);
    procedure UpdateTimerTimer(Sender: TObject);
  end;

implementation

{$R *.fmx}

{THidableFrame}

procedure THidableFrame.cbVisibleChange(Sender: TObject);
begin
  Visible := cbVisible.IsChecked;
end;

procedure THidableFrame.UpdateTimerTimer(Sender: TObject);
begin
  LabelName.Text := Name;
  cbVisible.IsChecked := Visible;
end;

procedure THidableFrame.btnShowChildrenClick(Sender: TObject);
begin
  for var Control in Rectangle.Controls do
    Control.Visible := true;
end;

end.
