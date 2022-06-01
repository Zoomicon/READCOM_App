//Description: READ-COM About dialog
//Author: George Birbilis (http://zoomicon.com)

unit READCOM.Views.About;

interface

uses
  READCOM.App.Globals, //for Globals.SVGIconImageList
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation,
  FMX.Objects, FMX.SVGIconImage, FMX.ImgList, FMX.Layouts, System.Actions,
  FMX.ActnList;

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
    TitleAndVersionLayout: TFlowLayout;
    VersionLayout: TFlowLayout;
    NewLine1: TFlowLayoutBreak;
    lblBlankRow: TLabel;
    NewLine2: TFlowLayoutBreak;
    ActionList: TActionList;
    actionHelp: TAction;
    btnHelp: TSpeedButton;
    rectBorder: TRectangle;
    procedure btnCloseClick(Sender: TObject);
    procedure GlyphLogoTap(Sender: TObject; const Point: TPointF);
    procedure actionHelpExecute(Sender: TObject);

  protected
    class var
      Frame: TFrame;
  public
    constructor Create(AOwner: TComponent); override;
    class procedure ShowModal(const TheParent: TFmxObject; const VisibleFlag: Boolean = true); //TODO: abstract design into a reusable ModalFrame class (see TWaitFrame too)

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

class procedure TAboutFrame.ShowModal(const TheParent: TFmxObject; const VisibleFlag: Boolean = true);
begin
  if not Assigned(Frame) then
  begin
    Frame := Create(Application); //use Application as the Owner since we reuse the same WaitFrame instance
    //Frame.Align := TAlignLayout.Content; //already set at frame designer (could be a property [Content, Center, Scale etc.])
  end;

  with Frame do
  begin
    Parent := TheParent;

    //Visible := VisibleFlag;
    if not VisibleFlag then FreeAndNil(Frame); //destroy instead of hiding to save memory
  end;
end;

{$region 'Events'}

procedure TAboutFrame.GlyphLogoTap(Sender: TObject; const Point: TPointF); //TODO: use some custom TGlyph descendent that surfaces MouseClick event and Cursor properties instead so that we can handle Click event too
begin
  OpenURLinBrowser(URL_READCOM);
end;

procedure TAboutFrame.actionHelpExecute(Sender: TObject);
begin
  ShowHelp;
end;

procedure TAboutFrame.btnCloseClick(Sender: TObject);
begin
  TAboutFrame.ShowModal(Parent, false);
end;

{$endregion}

end.
