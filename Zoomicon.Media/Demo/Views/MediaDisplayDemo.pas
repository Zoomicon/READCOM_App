unit MediaDisplayDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  Zoomicon.Media.FMX.MediaDisplay, FMX.Memo.Types, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo;

type
  TMediaDisplayDemo1 = class(TForm)
    MediaDisplay1: TMediaDisplay;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MediaDisplayDemo1: TMediaDisplayDemo1;

implementation

{$R *.fmx}

end.
