unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Objects, FMX.Controls, FMX.Controls.Presentation, FMX.StdCtrls, uStory,
  FMX.Types, uZoomableFrame, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Layouts;

type
  TMainForm = class(TForm)
    ZoomableFrame: TZoomableFrame;
    TrackBar: TTrackBar;
    Story1: TStory;
    StyleBook: TStyleBook;
    procedure TrackBarChange(Sender: TObject);
    procedure ZoomableFrameScrollBoxDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure LoadMessage(msg:String);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}
{$R *.iPad.fmx IOS}

procedure TMainForm.TrackBarChange(Sender: TObject);
begin
    ZoomableFrame.RatioScale := TrackBar.Value;
end;

procedure TMainForm.ZoomableFrameScrollBoxDblClick(Sender: TObject);
begin
  LoadMessage('test');
end;

procedure TMainForm.LoadMessage(msg:String);
var
  panel:Tcalloutpanel;
  memo:TLabel;
begin
  panel:=TCalloutPanel.Create(self);
  panel.Parent:=self;
  panel.Align:=TAlignLayout.Client;
  panel.Margins.Left:=5;
  panel.Margins.Right:=5;
  panel.Margins.Top:=5;
  panel.Margins.Bottom:=5;
  panel.CalloutPosition:=TCalloutPosition.Left;
  panel.CalloutOffset:=10;
  memo:=TLabel.Create(panel);
  memo.Parent:=panel;
  memo.Align:=TAlignLayout.Contents;
  memo.Margins.Left:=15;
  memo.Margins.Right:=15;
  memo.Margins.Top:=5;
  memo.Margins.Bottom:=5;
  memo.HitTest:=false;
  memo.Text:=msg;
end;

end.
