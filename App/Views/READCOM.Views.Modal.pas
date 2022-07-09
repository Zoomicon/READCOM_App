//Description: READ-COM Modal prompt
//Author: George Birbilis (http://zoomicon.com)

unit READCOM.Views.Modal;

interface

uses
  READCOM.App.Globals, //for Globals.SVGIconImageList
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation,
  FMX.Objects, FMX.SVGIconImage, FMX.ImgList, FMX.Layouts,
  FMX.ActnList;

type
  TModalFrame = class(TFrame)
    rectBackground: TRectangle;
    GlyphLogo: TGlyph;
    procedure rectBackgroundClick(Sender: TObject);

  protected
    const
      DEFAULT_CANCLOSE = true;

    class var
      Frame: TFrame;

  protected
    function GetCanClose: Boolean;
    procedure SetCanClose(const Value: Boolean);

  public
    constructor Create(AOwner: TComponent); override;
    class procedure ShowModal(const TheParent: TFmxObject; const VisibleFlag: Boolean = true); //TODO: abstract design into a reusable ModalFrame class (see TAboutFrame too)

    procedure Close;

  published
    property CanClose: Boolean read GetCanClose write SetCanClose default DEFAULT_CANCLOSE;

  end;

implementation
  uses
    READCOM.App.Main;

{$R *.fmx}

{$region 'Initialization'}

constructor TModalFrame.Create(AOwner: TComponent);
begin
  inherited;
  CanClose := DEFAULT_CANCLOSE;
end;

{$endregion}

{$region 'Properties'}

{$region 'CanClose'}

function TModalFrame.GetCanClose: Boolean;
begin
  result := (rectBackground.Cursor = crHandPoint);
end;

procedure TModalFrame.SetCanClose(const Value: Boolean);
begin
  if Value then
    rectBackground.Cursor := crHandPoint
  else
    rectBackground.Cursor := crDefault;
end;

{$endregion}

{$endregion}

{$region 'Methods'}

class procedure TModalFrame.ShowModal(const TheParent: TFmxObject; const VisibleFlag: Boolean = true);
begin
  if not Assigned(Frame) then
  begin
    Frame := Create(Application); //use Application as the Owner since we reuse the same ModalFrame instance
    //Frame.Align := TAlignLayout.Content; //already set at frame designer (could be a property [Content, Center, Scale etc.])
  end;

  with TModalFrame(Frame) do
  begin
    Parent := TheParent;
    if not VisibleFlag then Close;
  end;
end;

procedure TModalFrame.Close;
begin
  //Visible := false;
  FreeAndNil(Frame); //destroy instead of hiding to save memory
end;

{$endregion}

{$region 'Events'}

procedure TModalFrame.rectBackgroundClick(Sender: TObject);
begin
  if CanClose then
    Close;
end;

{$endregion}

end.
