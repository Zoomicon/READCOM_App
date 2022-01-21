//Description: Zooming Models
//Author: George Birbilis (http://zoomicon.com)

unit Zoomicon.Zooming.Models;

interface

uses
  System.Types,
  FMX.Controls; //for TControl

type
  TZoomChangedEvent = procedure (NewZoom: TPointF) of object;

  IZoomable = interface
    ['{C6A56119-CA7F-4748-B63C-DD7D6722B7BA}']

    function GetZoom: TPointF;
    procedure SetZoom(const ValueX, ValueY: Single); overload;
    procedure SetZoom(const Value: TPointF); overload;
    procedure SetZoom(const Value: Single); overload;
    procedure ZoomTo(const Control: TControl = nil; const KeepRatio: Boolean = true); //ZoomTo(nil) zooms to all content
    //
    property Zoom: TPointF read GetZoom write SetZoom;
    //property OnZoomChanged: TZoomChangedEvent; //should be able to define an event at an interface and let the mapping (say to a field) details to the implementing class
  end;

implementation

end.
