unit READCOM.Views.About;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation,
  FMX.Objects, FMX.SVGIconImage, FMX.ImgList,
  READCOM.App.Globals; //for SVGIconImageList

type
  TAboutFrame = class(TFrame)
    PanelTitleVersion: TPanel;
    lblTitle: TLabel;
    PanelVersion: TPanel;
    lblVersion: TLabel;
    lblVersionValue: TLabel;
    PanelVersionPadding: TPanel;
    MemoInfo: TMemo;
    rectBackground: TRectangle;
    btnClose: TSpeedButton;
    GlyphLogo: TGlyph;
    procedure btnCloseClick(Sender: TObject);
  private
    {Private declarations}
  public
    {Public declarations}
  end;

implementation

{$R *.fmx}

procedure TAboutFrame.btnCloseClick(Sender: TObject);
begin
  Parent := nil;
  Visible := false;
end;

end.
