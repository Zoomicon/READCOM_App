unit ActionsTest;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.ImageList, FMX.ImgList, FMX.SVGIconImageList, System.Actions,
  FMX.ActnList, FMX.Controls.Presentation, FMX.StdCtrls, ActionsTestFrame;

type
  TForm2 = class(TForm)
    Frame11: TFrame1;
    ActionList1: TActionList;
    ActionEdit: TAction;
    SpeedButton1: TSpeedButton;
    SVGIconImageList1: TSVGIconImageList;
    Label1: TLabel;
  private
    {Private declarations}
  public
    {Public declarations}
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

end.
