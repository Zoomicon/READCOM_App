unit ActionsTestFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.ImageList, FMX.ImgList, FMX.SVGIconImageList, System.Actions,
  FMX.ActnList, FMX.Controls.Presentation, FMX.Objects;

type
  TFrame1 = class(TFrame)
    SpeedButton1: TSpeedButton;
    ActionList1: TActionList;
    ActionEdit: TAction;
    SVGIconImageList1: TSVGIconImageList;
    Rectangle1: TRectangle;
    Label1: TLabel;
  private
    {Private declarations}
  public
    {Public declarations}
  end;

implementation

{$R *.fmx}

end.
