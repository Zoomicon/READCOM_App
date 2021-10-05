unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Objects, FMX.Controls, FMX.Controls.Presentation, FMX.StdCtrls, uStory,
  FMX.Types, uZoomableFrame, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Layouts,
  READCOM.App.Messages.Models, //for IMessageMenu
  iPub.Rtl.Messaging; //for SubscribeAttribute, TipMessagingThread

type
  TMainForm = class(TForm)
    ZoomableFrame: TZoomableFrame;
    TrackBar: TTrackBar;
    Story1: TStory;
    procedure TrackBarChange(Sender: TObject);
    procedure ZoomableFrameScrollBoxDblClick(Sender: TObject);

  public
    procedure LoadMessage(msg:String);

    [Subscribe(TipMessagingThread.Main)]
    procedure OnMenu(const AMessage: IMessageMenu);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}
{$R *.iPad.fmx IOS}

procedure TMainForm.OnMenu(const AMessage: IMessageMenu);
begin
  ShowMessage('Menu');
end;

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
