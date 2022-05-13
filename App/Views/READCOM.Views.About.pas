unit READCOM.Views.About;

interface

uses
  READCOM.App.Globals, //for Globals.SVGIconImageList
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation,
  FMX.Objects, FMX.SVGIconImage, FMX.ImgList, FMX.Layouts;

type
  TAboutFrame = class(TFrame)
    PanelTitleVersion: TPanel;
    lblTitle: TLabel;
    lblVersion: TLabel;
    lblVersionValue: TLabel;
    MemoInfo: TMemo;
    rectBackground: TRectangle;
    btnClose: TSpeedButton;
    GlyphLogo: TGlyph;
    FlowLayout1: TFlowLayout;
    FlowLayout2: TFlowLayout;
    FlowLayoutBreak1: TFlowLayoutBreak;
    lblBlankRow: TLabel;
    FlowLayoutBreak2: TFlowLayoutBreak;
    procedure btnCloseClick(Sender: TObject);
    procedure GlyphLogoTap(Sender: TObject; const Point: TPointF);
  private
    {Private declarations}
  public
    {Public declarations}
    constructor Create(AOwner: TComponent); override;
  end;

implementation
  uses
    Zoomicon.Helpers.FMX.Forms.ApplicationHelper, //for AppVersion
    READCOM.App.URLs; //for OpenURLinBrowser

{$R *.fmx}

{$region 'Initialization'}

constructor TAboutFrame.Create(AOwner: TComponent);
begin
  inherited;

  lblTitle.Text := STR_APP_TITLE;
  lblVersionValue.Text := Application.AppVersion;
  GlyphLogo.Cursor := crHandPoint;
end;

{$endregion}

{$region 'Events'}

procedure TAboutFrame.GlyphLogoTap(Sender: TObject; const Point: TPointF); //TODO: use some custom TGlyph descendent that surfaces MouseClick event and Cursor properties instead so that we can handle Click event too
begin
  OpenURLinBrowser(URL_READCOM);
end;

procedure TAboutFrame.btnCloseClick(Sender: TObject);
begin
  Parent := nil;
  Visible := false;
end;

{$endregion}

end.