unit Zoomicon.Zooming.ZoomFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts,
  Zoomicon.Zooming.Models;

const
  DEFAULT_ZOOM_CONTROLS_VISIBLE = false;

type
  TZoomFrame = class(TFrame, IZoomable)
    ScrollBox: TScrollBox;
    Zoomer: TScaledLayout;
    ScaledLayout: TScaledLayout;
    ZoomControls: TGridPanelLayout;
    trackZoomX: TTrackBar;
    trackZoomY: TTrackBar;
    switchSyncAxes: TSwitch;
    procedure ScrollBoxResize(Sender: TObject);
    procedure UpdateZoomFromTrackbars;
    procedure trackZoomXTracking(Sender: TObject);
    procedure trackZoomYTracking(Sender: TObject);
    procedure ScrollBoxMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);

  protected
    FStoredWheelDelta: extended;
    FOnZoomChanged: TZoomChangedEvent;

    {Proportional}
    function IsProportional: Boolean;
    procedure SetProportional(const Value: Boolean);

    {ZoomControlsVisible}
    function GetZoomControlsVisible: Boolean;
    procedure SetZoomControlsVisible(const Value: Boolean);

  public
    constructor Create(AOwner: TComponent); override;
    procedure Loaded; override;

    //IZoomable
    function GetZoom: TPointF;
    procedure ZoomTo(const Control: TControl; const KeepRatio: Boolean = true);
    procedure SetZoom(const ValueX, ValueY: Single); overload;
    procedure SetZoom(const Value: TPointF); overload;
    procedure SetZoom(const Value: Single); overload;

  published
    property Proportional: Boolean read IsProportional write SetProportional;
    property Zoom: TPointF read GetZoom write SetZoom;
    property OnZoomChanged: TZoomChangedEvent read FOnZoomChanged write FOnZoomChanged;
    property ZoomControlsVisible: Boolean read GetZoomControlsVisible write SetZoomControlsVisible default DEFAULT_ZOOM_CONTROLS_VISIBLE;
  end;

procedure Register;

implementation
  uses
    Zoomicon.FMX.Utils, //for TScaledLayoutHelper
    Math; //for Sign

{$R *.fmx}

{ TZoomFrame }

constructor TZoomFrame.Create(AOwner: TComponent);
begin
  inherited;
  ScrollBox.Stored := true;
  ZoomControls.Stored := false; //not storing, will load from FMX and the apply wrapper properties //no need to set this for its children controls too
  ZoomControlsVisible := DEFAULT_ZOOM_CONTROLS_VISIBLE;
end;

procedure TZoomFrame.Loaded;
begin
  BeginUpdate;
  Zoomer.Align := TAlignLayout.Center; //at design mode we have it set to TAlignLayout.Client
  UpdateZoomFromTrackbars;
  EndUpdate;
end;

{$region 'IZoomable'}

function TZoomFrame.GetZoom: TPointF;
begin
  result := Zoomer.ScalingFactor;
end;

procedure TZoomFrame.SetZoom(const Value: Single);
begin
  SetZoom(Value, Value);
end;

procedure TZoomFrame.SetZoom(const Value: TPointF);
begin
  SetZoom(Value.X, Value.Y);
end;

procedure TZoomFrame.SetZoom(const ValueX, ValueY: Single);
begin
  if (ValueX <> 0) and (ValueY <>0) then //FMX has bug where Scale won't work anymore if set to 0
    begin
    BeginUpdate;

    with Zoomer do
    begin
      Size.Size := TSizeF.Create(OriginalWidth * Abs(ValueX), OriginalHeight * Abs(ValueY)); //don't use Scale to resize (won't work well here), ScaledLayout scales its contents automatically
      Scale.Point := TPointF.Create(Sign(ValueX), Sign(ValueY));
    end;
    //ScrollBox.InvalidateContentSize;

    //update track bars
    trackZoomX.BeginUpdate; trackZoomX.ValueRange.Value := ValueX; trackZoomX.EndUpdate;
    trackZoomY.BeginUpdate; trackZoomY.ValueRange.Value := ValueY; trackZoomY.EndUpdate;

    EndUpdate;
    end;
end;

procedure TZoomFrame.ZoomTo(const Control: TControl; const KeepRatio: Boolean = true); //TODO: adjust for scrollbar sizes
begin
  //BeginUpdate; //Not needed
  var Rect := Control.BoundsRect;

  var scalingFactor := ScaledLayout.ScalingFactor;

  if KeepRatio then
    begin
    var zoomFactor := Min(Zoomer.OriginalWidth/(Rect.Width*scalingFactor.X), Zoomer.OriginalHeight/(Rect.Height*scalingFactor.Y)); //using Max here would mean you fill the area but get some cliping
    SetZoom(zoomFactor);
    end
  else
    begin
    var zoomFactor := TPointF.Create(Zoomer.OriginalWidth/(Rect.Width*scalingFactor.X), Zoomer.OriginalHeight/(Rect.Height*scalingFactor.Y));
    SetZoom(zoomFactor);
    end;

  //EndUpdate; //make sure we do this here if needed, not at the end, else scrolling calculations won't work

  var ZoomerParent := Zoomer.ParentControl;
  var CenterPointNewCoords := ZoomerParent.AbsoluteToLocal(Control.ParentControl.LocalToAbsolute(Rect.CenterPoint*scalingFactor)); //must adjust the CenterPoint by the ScalingFactor here

  var ScrollBoxHost := GetScrollBoxParent(Zoomer);
  if (ScrollBoxHost <> nil) then
    begin
    var scrollToPos := CenterPointNewCoords - TPointF.Create(ScrollBoxHost.Width/2, ScrollBoxHost.Height/2);
    ScrollBoxHost.ViewportPosition := scrollToPos; //don't use ScrollTo method, it is deprecated and just calls ScrollBy (which expects [DX, DY], not a position to scroll to)
    end
  else
    begin //TODO: see if this case (when not hosted in ScrollBox) works
    var offsetPos := CenterPointNewCoords - TPointF.Create(ZoomerParent.Width/2, ZoomerParent.Height/2);
    Zoomer.Position.Point := offsetPos;
    end;
end;

{$endregion}

{$region 'MouseWheel'}

procedure TZoomFrame.ScrollBoxMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); //TODO: adjust to zoom at mouse point
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

{$endregion}

{$region 'Resize'}

procedure TZoomFrame.ScrollBoxResize(Sender: TObject);
begin
  var oldZoom := Zoomer.ScalingFactor;

  var Viewport := Sender as TControl;
  var ViewportSize := Viewport.Size.Size;
  if not ViewportSize.IsZero then
    begin
    BeginUpdate;
    Zoomer.Size.Size := ViewportSize;

    with Zoomer do
    begin
      OriginalWidth := Width;
      OriginalHeight := Height;
    end;

    SetZoom(oldZoom); //UpdateZoomFromTrackbars;
    //ScrollBox.InvalidateContentSize;
    EndUpdate;
    end;
end;

{$endregion}

{$region 'Trackbars'}

procedure TZoomFrame.UpdateZoomFromTrackbars;
begin
  SetZoom(trackZoomX.Value, trackZoomY.Value);
end;

procedure TZoomFrame.trackZoomXTracking(Sender: TObject);
begin
  if trackZoomX.IsUpdating then exit;

  BeginUpdate;
  if switchSyncAxes.IsChecked then
    trackZoomY.Value := trackZoomX.Value;

  UpdateZoomFromTrackbars;
  EndUpdate;
end;

procedure TZoomFrame.trackZoomYTracking(Sender: TObject);
begin
  if trackZoomY.IsUpdating then exit;

  BeginUpdate;
  if switchSyncAxes.IsChecked then
    trackZoomX.Value := trackZoomY.Value;

  UpdateZoomFromTrackbars;
  EndUpdate;
end;

{$endregion}

{$region 'Proportional'}

function TZoomFrame.IsProportional: Boolean;
begin
  result := switchSyncAxes.IsChecked;
end;

procedure TZoomFrame.SetProportional(const Value: Boolean);
begin
  switchSyncAxes.IsChecked := Value;
end;

{$endregion}

{$region 'ZoomControlsVisible'}

function TZoomFrame.GetZoomControlsVisible: Boolean;
begin
  result := ZoomControls.Visible;
end;

procedure TZoomFrame.SetZoomControlsVisible(const Value: Boolean);
begin
  ZoomControls.Visible := Value;
end;

{$endregion}

procedure RegisterClasses;
begin
  RegisterFmxClasses([TZoomFrame]); //register for persistence (in case they're used standalone)
end;

procedure Register;
begin
  GroupDescendentsWith(TZoomFrame, TControl);
  RegisterClasses;
  RegisterComponents('Zoomicon', [TZoomFrame]);
end;

initialization
  RegisterClasses; //don't call Register here, it's called by the IDE automatically on a package installation (fails at runtime)

end.
