unit Zoomicon.Zooming.Classes;

//TODO: NOT WORKING (for now use TZoomFrame instead)

interface

uses
  Zoomicon.Zooming.Models, //for IZoomable
  FMX.Controls, //for TControl
  FMX.Layouts, //for TCustomScrollBox
  FMX.Types, //for TAlignLayout
  System.Classes, //for TComponent
  System.Types; //for TSizeF

type
  TZoomedLayout = class(TScaledLayout, IZoomable)
  protected
    FStoredWheelDelta: extended;
    FOnZoomChanged: TZoomChangedEvent;
    FInnerLayout: TScaledLayout;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    //
    procedure SetInnerLayout(const Value: TScaledLayout);
    procedure ParentChanged; override;
    procedure HandleViewportMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
    procedure HandleViewportResize(Sender: TObject);

  public
    constructor Create(AOwner: TComponent); override;

    //IZoomable
    function GetZoom: TPointF;
    procedure SetZoom(const ValueX, ValueY: Single); overload;
    procedure SetZoom(const Value: TPointF); overload;
    procedure SetZoom(const Value: Single); overload;
    procedure ZoomTo(const Control: TControl; const KeepRatio: Boolean = true);

  published
    property InnerLayout: TScaledLayout read FInnerLayout write SetInnerLayout;
    property Zoom: TPointF read GetZoom write SetZoom;
    property OnZoomChanged: TZoomChangedEvent read FOnZoomChanged write FOnZoomChanged;
  end;

procedure Register;

implementation
  uses
    Zoomicon.FMX.Utils,
    Math; //for Sign

{ TZoomedLayout }

constructor TZoomedLayout.Create(AOwner: TComponent);
begin
  inherited;

  if (csDesigning in ComponentState) then
    Align := TAlignLayout.Client //at design mode fill client area of parent
  else
    Align := TAlignLayout.Center;

  SetAcceptsControls(true);
  SetInnerLayout(TScaledLayout.Create(Self));
end;

procedure TZoomedLayout.SetInnerLayout(const Value: TScaledLayout);
begin
  with Value do
    begin
    Align := TAlignLayout.Fit;
    //Stored := False; //don't store automatically, stored via property
    Locked := True;
    HitTest := False;
    end;
  Value.Parent := Self;
end;

procedure TZoomedLayout.ParentChanged; //if can't find scrollbox then try AncestorParentChanged instead
begin
  inherited;
  var Viewport: TControl := GetScrollBoxParent(Self);
  if (Viewport = nil) then
    Viewport := ParentControl;

  if (Viewport <> nil) then
    begin
    Viewport.OnMouseWheel := HandleViewportMouseWheel;
    Viewport.OnResize := HandleViewportResize; //TODO: should we handle OnResized (undocumented) instead? (see https://stackoverflow.com/questions/52255438/what-the-difference-between-onresize-and-onresized#comment91617044_52255438)
    end;
end;

//

function TZoomedLayout.GetZoom: TPointF;
begin
  result := ScalingFactor;
end;

procedure TZoomedLayout.SetZoom(const Value: Single);
begin
  SetZoom(Value, Value);
end;

procedure TZoomedLayout.SetZoom(const ValueX, ValueY: Single);
begin
  SetZoom(TPointF.Create(ValueX, ValueY));
end;

procedure TZoomedLayout.SetZoom(const Value: TPointF);
begin
  var ValueX := Value.X;
  var ValueY := Value.Y;

  if (ValueX = 0) or (ValueY = 0) then
    exit; //FMX has bug where Scale won't work anymore if set to 0

  BeginUpdate;

  Size.Size := TSizeF.Create(OriginalWidth * Abs(ValueX), OriginalHeight * Abs(ValueY)); //don't use Scale to resize (won't work well here), ScaledLayout scales its contents automatically
  Scale.Point := TPointF.Create(Sign(ValueX), Sign(ValueY));

  //ScrollBox.InvalidateContentSize;

  if Assigned(FOnZoomChanged) then
    FOnZoomChanged(Value);

  EndUpdate;
end;

//

procedure TZoomedLayout.ZoomTo(const Control: TControl; const KeepRatio: Boolean = true); //TODO: adjust for scrollbar sizes
begin
  //BeginUpdate; //Not needed
  var Rect := Control.BoundsRect;

  var scalingFactor := FInnerLayout.ScalingFactor;

  if KeepRatio then
    begin
    var zoomFactor := Min(OriginalWidth/(Rect.Width*scalingFactor.X), OriginalHeight/(Rect.Height*scalingFactor.Y)); //using Max here would mean you fill the area but get some cliping
    SetZoom(zoomFactor);
    end
  else
    begin
    var zoomFactor := TPointF.Create(OriginalWidth/(Rect.Width*scalingFactor.X), OriginalHeight/(Rect.Height*scalingFactor.Y));
    SetZoom(zoomFactor);
    end;

  //EndUpdate; //make sure we do this here if needed, not at the end, else scrolling calculations won't work

  var CenterPointNewCoords := ParentControl.AbsoluteToLocal(Control.ParentControl.LocalToAbsolute(Rect.CenterPoint*scalingFactor)); //must adjust the CenterPoint by the ScalingFactor here

  var ScrollBoxHost := GetScrollBoxParent(Self);
  if (ScrollBoxHost <> nil) then
    begin
    var scrollToPos := CenterPointNewCoords - TPointF.Create(ScrollBoxHost.Width/2, ScrollBoxHost.Height/2);
    ScrollBoxHost.ViewportPosition := scrollToPos; //don't use ScrollTo method, it is deprecated and just calls ScrollBy (which expects [DX, DY], not a position to scroll to)
    end
  else
    begin
    var offsetPos := CenterPointNewCoords - TPointF.Create(ParentControl.Width/2, ParentControl.Height/2);
    Position.Point := offsetPos;
    end;
end;

//

procedure TZoomedLayout.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); //TODO: adjust to zoom at mouse point
begin
  if ssCtrl in Shift then //using CTRL+MouseWheel to zoom in/out
    begin
    FStoredWheelDelta := FStoredWheelDelta + WheelDelta; // accumulate wheeldelta's
    SetZoom(1 + FStoredWheelDelta / 120 / 5); //increase 5 to zoom slower
    Handled := true;
    end
  else //TODO: handle Shift or Alt too for horizontal scrolling? (see if FMX has similar shortcut already)
    inherited; //let ancestor handle event
end;

procedure TZoomedLayout.HandleViewportMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); //TODO: adjust to zoom at mouse point
begin
  if ssCtrl in Shift then
    begin
    FStoredWheelDelta := FStoredWheelDelta + WheelDelta; // accumulate wheeldelta's
    SetZoom(1 + FStoredWheelDelta / 120 / 5); //increase 5 to zoom slower
    Handled := true;
    end
  else
    Handled := false;
end;

//

procedure TZoomedLayout.HandleViewportResize(Sender: TObject);
begin
  var oldZoom := Zoom;

  var Viewport := Sender as TControl;
  var ViewportSize := Viewport.Size.Size;
  if not ViewportSize.IsZero then
    begin
    BeginUpdate;
    Size.Size := ViewportSize; //TODO: adjust for scrollbar sizes

    OriginalWidth := Width;
    OriginalHeight := Height;

    Zoom := oldZoom;
    //ScrollBox.InvalidateContentSize;
    EndUpdate;
    end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure RegisterClasses;
begin
  RegisterFmxClasses([TZoomedLayout]); //register for persistence (in case they're used standalone)
end;

procedure Register;
begin
  GroupDescendentsWith(TZoomedLayout, TControl);
  RegisterClasses;
  RegisterComponents('Zoomicon', [TZoomedLayout]);
end;

initialization
  RegisterClasses; //don't call Register here, it's called by the IDE automatically on a package installation (fails at runtime)

end.
