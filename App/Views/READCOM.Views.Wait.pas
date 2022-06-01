//Description: READ-COM Wait dialog
//Author: George Birbilis (http://zoomicon.com)

unit READCOM.Views.Wait;

interface

uses
  READCOM.App.Globals, //for Globals.SVGIconImageList
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation,
  FMX.Objects, FMX.SVGIconImage, FMX.ImgList, FMX.Layouts,
  FMX.ActnList;

type
  TWaitFrame = class(TFrame)
    rectBackground: TRectangle;
    GlyphLogo: TGlyph;

  protected
    class var
      Frame: TFrame;

  public
    constructor Create(AOwner: TComponent); override;
    class procedure ShowModal(const TheParent: TFmxObject; const VisibleFlag: Boolean = true); //TODO: abstract design into a reusable ModalFrame class (see TAboutFrame too)

  end;

implementation
  uses
    READCOM.App.Main;

{$R *.fmx}

{$region 'Initialization'}

constructor TWaitFrame.Create(AOwner: TComponent);
begin
  inherited;
  GlyphLogo.Cursor := crHourGlass;
end;

{$endregion}

class procedure TWaitFrame.ShowModal(const TheParent: TFmxObject; const VisibleFlag: Boolean = true);
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

end.
