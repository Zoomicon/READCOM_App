unit Zoomicon.Zooming.ZoomAndPan;

interface

uses
  Zoomicon.Zooming.Models, //for IZoomable
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Controls.Presentation;

type
  TZoomableFrame = class(TFrame, IZoomable)
  //-- Methods
  public
    procedure ZoomTo(Rect: TRectF);
    procedure ScrollToCenter(Rect: TRectF);
  end;

implementation

uses
  System.Math;

{$R *.fmx}

{ TZoomableFrame }

procedure TZoomableFrame.ScrollToCenter(Rect: TRectF);
begin

end;

procedure TZoomableFrame.ZoomTo(Rect: TRectF);
begin
  var scaleX := Width / Rect.Width;
  var scaleY := Height / Rect.Height;
  var point := TPointF.Create(Scale.Point.X, Scale.Point.Y);
  point.Scale(Min(scaleX, scaleY)); //TODO: does this change the scale? or do we need to set it again?
  Scale.Point := point;

  Position.Point := TPointF.Create(Rect.Left + Rect.Width/2 - Width/2, Rect.Top + Rect.Height/2 - Height/2); //TODO: should check clip bounds maybe to find the viewport width/height the parent has constrained us into
  //...or maybe check the type of the parent?

  //need something like IScrollInfo of .NET
  //https://www.codeproject.com/Articles/85603/A-WPF-custom-control-for-zooming-and-panning
  //https://www.codeproject.com/Articles/85603/A-WPF-custom-control-for-zooming-and-panning#Part2
  //https://www.codeproject.com/Articles/167453/A-Silverlight-custom-control-for-zooming-and-panni
  //https://docs.microsoft.com/en-us/dotnet/api/system.windows.controls.primitives.iscrollinfo?redirectedfrom=MSDN&view=windowsdesktop-5.0
  //https://docs.microsoft.com/en-us/dotnet/api/system.windows.controls.scrollviewer?redirectedfrom=MSDN&view=windowsdesktop-5.0

end;

end.
