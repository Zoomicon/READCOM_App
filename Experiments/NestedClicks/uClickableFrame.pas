unit uClickableFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects;

type
  TClickableFrame = class(TFrame)
    LabelName: TLabel;
    Rectangle: TRectangle;
    cbHitTest: TCheckBox;
    procedure FramePaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure cbHitTestChange(Sender: TObject);
  protected
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
  end;

implementation

{$R *.fmx}



{ TFrame3 }

procedure TClickableFrame.cbHitTestChange(Sender: TObject);
begin
  HitTest := cbHitTest.IsChecked;
end;

procedure TClickableFrame.FramePaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
  LabelName.Text := Name;
  cbHitTest.IsChecked := HitTest;
end;

procedure TClickableFrame.MouseClick(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;

  ShowMessageFmt('Clicked Frame %s', [Name]);
end;

end.
