//Description: READ-COM About dialog
//Author: George Birbilis (http://zoomicon.com)

unit READCOM.Views.About;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation,
  FMX.Objects, FMX.SVGIconImage, FMX.ImgList, FMX.Layouts, System.Actions,
  FMX.ActnList,
  //
  READCOM.App.Globals, //for Globals.SVGIconImageList
  READCOM.Views.Modal, //for TModalFrame
  Zoomicon.Media.FMX.ClickableGlyph; //for TClickableGlyph

type
  TAboutFrame = class(TModalFrame)
    ActionList: TActionList;
    actionHelp: TAction;
    rectBorder: TRectangle;
    MemoInfo: TMemo;
    btnHelp: TSpeedButton;
    GlyphLogo: TClickableGlyph;
    PanelTitleVersion: TPanel;
    TitleAndVersionLayout: TFlowLayout;
    lblTitle: TLabel;
    NewLine1: TFlowLayoutBreak;
    lblBlankRow: TLabel;
    NewLine2: TFlowLayoutBreak;
    VersionLayout: TFlowLayout;
    lblVersion: TLabel;
    lblVersionValue: TLabel;
    btnClose: TSpeedButton;
    actionClose: TAction;
    procedure GlyphLogoClick(Sender: TObject);
    procedure actionCloseExecute(Sender: TObject);
    procedure actionHelpExecute(Sender: TObject);
    procedure GlyphLogoTap(Sender: TObject; const Point: TPointF);

  protected
    class var
      Frame: TFrame;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation
  uses
    Zoomicon.Helpers.FMX.Forms.ApplicationHelper, //for AppVersion
    READCOM.App.Main,
    READCOM.App.Messages,
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

procedure TAboutFrame.GlyphLogoClick(Sender: TObject);
begin
  inherited;
  OpenURLinBrowser(URL_READCOM);
end;

//Click doesn't seem to work even though TClickableGlyph surfaces it (nor the Cursor property), so using Tap to support clicking at least on touch screens
procedure TAboutFrame.GlyphLogoTap(Sender: TObject; const Point: TPointF);
begin
  inherited;
  OpenURLinBrowser(URL_READCOM);
end;

procedure TAboutFrame.actionCloseExecute(Sender: TObject);
begin
  inherited;
  Close;
end;

procedure TAboutFrame.actionHelpExecute(Sender: TObject);
begin
  inherited;
  ShowHelp;
end;

{$endregion}

end.
