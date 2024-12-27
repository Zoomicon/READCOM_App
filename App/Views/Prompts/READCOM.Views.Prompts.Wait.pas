//Description: READ-COM Wait dialog
//Author: George Birbilis (http://zoomicon.com)

unit READCOM.Views.Prompts.Wait;

interface
  {$region 'Used units'}
  uses
    System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
    //
    FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
    FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation,
    FMX.Objects, FMX.SVGIconImage, FMX.ImgList, FMX.Layouts,
    FMX.ActnList,
    //
    Zoomicon.Media.FMX.ModalFrame; //for TModalFrame
  {$endregion}

type
  TWaitFrame = class(TModalFrame)
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.fmx}

{$region 'Initialization'}

constructor TWaitFrame.Create(AOwner: TComponent);
begin
  inherited;

  CanClose := false; //this will do "rectBackground.Cursor := crDefault", so do this before setting to crHourGlass

  //Note: the following will practically also function as "CanClose := false", since "GetCanClose" checks if "rectBackground.Cursor = crHandPoint"
  rectBackground.Cursor := crHourGlass; //don't set the Glyph's Cursor, it has HitTest=false as default (and don't want to change it since we are handling Click event at the rectBackground since TGlyph doesn't expose the Mouse events at Delphi 11.1's FMX)
end;

{$endregion}

end.
