unit uContentFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects;

type
  TContentFrame = class(TFrame)
    rectBackground: TRectangle;
  private
    {Private declarations}
  public
    {Public declarations}
  end;

implementation

{$R *.fmx}

end.
