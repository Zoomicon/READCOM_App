unit READCOM.Views.About;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation,
  FMX.Objects, FMX.SVGIconImage;

type
  TAboutFrame = class(TFrame)
    svgLogo: TSVGIconImage;
    Panel1: TPanel;
    lblTitle: TLabel;
    Panel2: TPanel;
    lblVersion: TLabel;
    lblVersionValue: TLabel;
    Panel3: TPanel;
    Memo1: TMemo;
    rectBackground: TRectangle;
    btnClose: TSpeedButton;
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
