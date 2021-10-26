unit Zoomicon.Zooming.Classes;

interface

uses
  Zoomicon.Zooming.Models, //for IZoomable
  FMX.Layouts, //for TCustomScrollBox
  System.Types; //for TSizeF

type
  TCustomScrollBoxHelper = class helper for TCustomScrollBox
  protected
    function GetViewportSize: TSizeF;
  public
    /// <summary>Size of view port of the ScrollBox's content.</summary>
    property ViewportSize: TSizeF read GetViewportSize;
  end;

  TScaledLayoutHelper = class helper for TScaledLayout
    procedure ZoomTo(const Rect: TRectF);
  end;

  TZoomableHelper = class(TScaledLayout, IZoomable)
  end;

implementation

{ TCustomScrollBoxHelper }

function TCustomScrollBoxHelper.GetViewportSize: TSizeF;
begin          TScrollContent
  result := {ContentLayout.Size.}Size; //TODO: check if correct
end;

{ TScaledLayoutHelper }

procedure TScaledLayoutHelper.ZoomTo(const Rect: TRectF);
begin

end;

end.
