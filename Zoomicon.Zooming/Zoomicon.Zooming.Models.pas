unit Zoomicon.Zooming.Models;

interface

uses
  System.Types;

type
  IZoomable = interface
    ['{C6A56119-CA7F-4748-B63C-DD7D6722B7BA}']

    procedure ZoomTo(Rect: TRectF);
    procedure ScrollToCenter(Rect: TRectF);
  end;

implementation

end.
