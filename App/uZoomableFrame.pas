unit uZoomableFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Controls.Presentation;

type
  IZoomable = interface
    ['{C6A56119-CA7F-4748-B63C-DD7D6722B7BA}']
    function GetRatioScale: Single;
    procedure SetRatioScale(const ARatioScale: Single);
    property RatioScale: Single read GetRatioScale write SetRatioScale;
  end;

  TZoomableFrame = class(TFrame, IZoomable)
    ScrollBox: TScrollBox;
    Layout: TLayout;
  protected
    FRatioScale: Single;
  public
    function GetRatioScale: Single;
    procedure SetRatioScale(const ARatioScale: Single);
  published
    property RatioScale: Single read GetRatioScale write SetRatioScale;
  end;

implementation

{$R *.fmx}

function TZoomableFrame.GetRatioScale: Single;
begin
  result := FRatioScale;
end;

procedure TZoomableFrame.SetRatioScale(const ARatioScale: Single);
begin
  Layout.Scale := TPosition.Create(TPointF.Create(ARatioScale, ARatioScale));
  FRatioScale := ARatioScale;
end;

end.
