unit uMainForm;

interface

uses
  FMX.Controls,
  FMX.Forms,
  FMX.Layouts,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.Controls.Presentation,
  FMX.Types,
  System.Classes,
  System.Types,
  Zoomicon.Zooming.Models; //for IZoomable

const
  DEFAULT_ZOOM_CONTROLS_VISIBLE = true;

type
  TMainForm = class(TForm, IZoomable)
    ScrollBox: TScrollBox;
    trackZoomY: TTrackBar;
    ScaledLayout: TScaledLayout;
    btnZoom1: TButton;
    FillRect2: TRectangle;
    FillRect1: TRectangle;
    Zoomer: TScaledLayout;
    trackZoomX: TTrackBar;
    switchSyncAxes: TSwitch;
    ZoomControls: TGridPanelLayout;
    btnZoom2: TButton;
    btnZoom3: TButton;
    btnZoom4: TButton;
    btnZoom5: TButton;
    btnZoom5a: TButton;
    procedure btnZoomClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ScrollBoxResize(Sender: TObject);
    procedure trackZoomXTracking(Sender: TObject);
    procedure trackZoomYTracking(Sender: TObject);
    procedure ScrollBoxMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);

  protected
    FStoredWheelDelta: extended;
    FOnZoomChanged: TZoomChangedEvent;
    procedure UpdateZoomFromTrackbars;

    {Proportional}
    function IsProportional: Boolean;
    procedure SetProportional(const Value: Boolean);

    {ZoomControlsVisible}
    function GetZoomControlsVisible: Boolean;
    procedure SetZoomControlsVisible(const Value: Boolean);

  public
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

var
  MainForm: TMainForm;

implementation
  uses
    Zoomicon.FMX.Utils, //for TScaledLayoutHelper
    Math; //for Sign

{$R *.fmx}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  BeginUpdate;
  ZoomControlsVisible := DEFAULT_ZOOM_CONTROLS_VISIBLE;
  Zoomer.Align := TAlignLayout.Center; //at design mode we have it set to TAlignLayout.Client
  UpdateZoomFromTrackbars;
  EndUpdate;
end;

{$region 'IZoomable'}

function TMainForm.GetZoom: TPointF;
begin
  result := Zoomer.ScalingFactor;
end;

procedure TMainForm.SetZoom(const Value: Single);
begin
  SetZoom(Value, Value);
end;

procedure TMainForm.SetZoom(const Value: TPointF);
begin
  SetZoom(Value.X, Value.Y);
end;

procedure TMainForm.SetZoom(const ValueX, ValueY: Single);
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

procedure TMainForm.ZoomTo(const Control: TControl; const KeepRatio: Boolean = true); //TODO: adjust for scrollbar sizes
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

procedure TMainForm.ScrollBoxMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); //TODO: adjust to zoom at mouse point
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

procedure TMainForm.ScrollBoxResize(Sender: TObject);
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

procedure TMainForm.UpdateZoomFromTrackbars;
begin
  SetZoom(trackZoomX.Value, trackZoomY.Value);
end;

procedure TMainForm.trackZoomXTracking(Sender: TObject);
begin
  if trackZoomX.IsUpdating then exit;

  BeginUpdate;
  if switchSyncAxes.IsChecked then
    trackZoomY.Value := trackZoomX.Value;

  UpdateZoomFromTrackbars;
  EndUpdate;
end;

procedure TMainForm.trackZoomYTracking(Sender: TObject);
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

function TMainForm.IsProportional: Boolean;
begin
  result := switchSyncAxes.IsChecked;
end;

procedure TMainForm.SetProportional(const Value: Boolean);
begin
  switchSyncAxes.IsChecked := Value;
end;

{$endregion}

{$region 'ZoomControlsVisible'}

function TMainForm.GetZoomControlsVisible: Boolean;
begin
  result := ZoomControls.Visible;
end;

procedure TMainForm.SetZoomControlsVisible(const Value: Boolean);
begin
  ZoomControls.Visible := Value;
end;

{$endregion}

/////////////////////////

procedure TMainForm.btnZoomClick(Sender: TObject);
begin
  var Control := Sender As TButton;
  //ShowMessageFmt('Clicked: %s - button scale: (%f,%f)', [Control.Text, Control.Scale.X, Control.Scale.Y]);
  //with ScrollBox.ViewportPosition do ShowMessageFmt('ViewPortPosition before: (%f, %f)', [x, y]);
  ZoomTo(Control, switchSyncAxes.IsChecked);
  //with ScrollBox.ViewportPosition do ShowMessageFmt('ViewPortPosition after: (%f, %f)', [x, y]);
end;

end.