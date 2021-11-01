unit MainForm;

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
  System.Types;

type
  TForm2 = class(TForm)
    ScrollBox: TScrollBox;
    trackZoomY: TTrackBar;
    ScaledLayout: TScaledLayout;
    btnZoom1: TButton;
    FillRect2: TRectangle;
    FillRect1: TRectangle;
    Zoomer: TScaledLayout;
    trackZoomX: TTrackBar;
    switchSyncAxes: TSwitch;
    GridPanelLayout1: TGridPanelLayout;
    btnZoom2: TButton;
    btnZoom3: TButton;
    btnZoom4: TButton;
    btnZoom5: TButton;
    Button1: TButton;
    procedure trackZoomYTracking(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ScrollBoxResize(Sender: TObject);
    procedure btnZoomClick(Sender: TObject);
    procedure UpdateZoomFromTrackbars;
    procedure trackZoomXTracking(Sender: TObject);
    procedure ScrollBoxMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);

  public
    procedure ZoomTo(const Control: TControl; const KeepRatio: Boolean = true);
    procedure SetZoom(const ValueX, ValueY: Single); overload;
    procedure SetZoom(const Value: TPointF); overload;
    procedure SetZoom(const Value: Single); overload;
  end;

var
  Form2: TForm2;

implementation
  uses
    Zoomicon.FMX.Utils,
    Math;

{$R *.fmx}

procedure TForm2.FormCreate(Sender: TObject);
begin
  BeginUpdate;
  Zoomer.Align := TAlignLayout.Center; //at design mode we have it set to TAlignLayout.Client
  UpdateZoomFromTrackbars;
  EndUpdate;
end;

procedure TForm2.SetZoom(const Value: Single);
begin
  SetZoom(Value, Value);
end;

procedure TForm2.SetZoom(const Value: TPointF);
begin
  SetZoom(Value.X, Value.Y);
end;

procedure TForm2.SetZoom(const ValueX, ValueY: Single);
begin
  if (ValueX <> 0) and (ValueY <>0) then //FMX has bug where Scale won't work anymore if set to 0
    begin
    BeginUpdate;

    //update track bars
    trackZoomX.BeginUpdate; trackZoomX.ValueRange.Value := ValueX; trackZoomX.EndUpdate;
    trackZoomY.BeginUpdate; trackZoomY.ValueRange.Value := ValueY; trackZoomY.EndUpdate;

    with Zoomer do
    begin
      Size.Size := TSizeF.Create(OriginalWidth * Abs(ValueX), OriginalHeight * Abs(ValueY)); //don't use Scale to resize (won't work well here), ScaledLayout scales its contents automatically
      Scale.Point := TPointF.Create(Sign(ValueX), Sign(ValueY));
    end;
    //ScrollBox.InvalidateContentSize;

    EndUpdate;
    end;
end;

procedure TForm2.ZoomTo(const Control: TControl; const KeepRatio: Boolean = true); //TODO: adjust for scrollbar sizes
begin
  //BeginUpdate; //Not needed
  var Rect := Control.BoundsRect;

  var scalingFactor := GetScalingFactor(ScaledLayout);

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

  var CenterPointNewCoords := ScrollBox.Content.AbsoluteToLocal(Control.ParentControl.LocalToAbsolute(Rect.CenterPoint*scalingFactor)); //must adjust the CenterPoint by the ScalingFactor here

  var scrollToPos := CenterPointNewCoords - TPointF.Create(ScrollBox.Size.Width/2, ScrollBox.Size.Height/2);
  ScrollBox.ViewportPosition := scrollToPos; //don't use ScrollTo method, it is deprecated and just calls ScrollBy (which expects [DX, DY], not a position to scroll to)
end;

procedure TForm2.UpdateZoomFromTrackbars;
begin
  SetZoom(trackZoomX.Value, trackZoomY.Value);
end;

var
  StoredWheelDelta: extended;

procedure TForm2.ScrollBoxMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); //TODO: adjust to zoom at mouse point
begin
  if ssCtrl in Shift then
    begin
    StoredWheelDelta := StoredWheelDelta + WheelDelta; // accumulate wheeldelta's
    SetZoom(1 + StoredWheelDelta / 120 / 5); //increase 5 to zoom slower
    Handled := true;
    end
  else
    Handled := false;
end;

procedure TForm2.ScrollBoxResize(Sender: TObject);
begin
  var scrollBoxSize := ScrollBox.Size.Size;
  if not scrollBoxSize.IsZero then
    begin
    BeginUpdate;
    Zoomer.Size.Size := scrollBoxSize;
    with Zoomer do
    begin
      OriginalWidth := Width;
      OriginalHeight := Height;
    end;
    UpdateZoomFromTrackbars;
    //ScrollBox.InvalidateContentSize;
    EndUpdate;
    end;
end;

procedure TForm2.trackZoomXTracking(Sender: TObject);
begin
  if trackZoomX.IsUpdating then exit;

  BeginUpdate;
  if switchSyncAxes.IsChecked then
    trackZoomY.Value := trackZoomX.Value;

  UpdateZoomFromTrackbars;
  EndUpdate;
end;

procedure TForm2.trackZoomYTracking(Sender: TObject);
begin
  if trackZoomY.IsUpdating then exit;

  BeginUpdate;
  if switchSyncAxes.IsChecked then
    trackZoomX.Value := trackZoomY.Value;

  UpdateZoomFromTrackbars;
  EndUpdate;
end;

/////////////////////////

procedure TForm2.btnZoomClick(Sender: TObject);
begin
  var Control := Sender As TButton;
  //ShowMessageFmt('Clicked: %s - button scale: (%f,%f)', [Control.Text, Control.Scale.X, Control.Scale.Y]);
  //with ScrollBox.ViewportPosition do ShowMessageFmt('ViewPortPosition before: (%f, %f)', [x, y]);
  ZoomTo(Control, switchSyncAxes.IsChecked);
  //with ScrollBox.ViewportPosition do ShowMessageFmt('ViewPortPosition after: (%f, %f)', [x, y]);
end;

end.
