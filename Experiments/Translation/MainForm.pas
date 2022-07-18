unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Memo.Types, FMX.StdCtrls, FMX.Edit, FMX.Layouts,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo;

type
  TForm2 = class(TForm)
    MemoFrom: TMemo;
    MemoTo: TMemo;
    btnTranslate: TButton;
    LayoutControls: TLayout;
    edFrom: TEdit;
    lblFrom: TLabel;
    lblTo: TLabel;
    edTo: TEdit;
    memoAPIkey: TMemo;
    lblAPIkey: TLabel;
    procedure btnTranslateClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation
  uses uTranslation;

{$R *.fmx}

procedure TForm2.btnTranslateClick(Sender: TObject);
begin
  MemoTo.Text :=
  Translate(memoAPIkey.Text, memoFrom.Text, edFrom.Text, edTo.Text) +
  '---------------' +
  Translate2(memoAPIkey.Text, memoFrom.Text, edFrom.Text, edTo.Text);
end;

end.
