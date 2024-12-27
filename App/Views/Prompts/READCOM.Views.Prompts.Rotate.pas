//Description: READ-COM Rotate dialog
//Author: George Birbilis (http://zoomicon.com)

unit READCOM.Views.Prompts.Rotate;

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
  TRotateFrame = class(TModalFrame)
  end;

implementation

{$R *.fmx}

end.
