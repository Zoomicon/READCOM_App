unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects;

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
    procedure trackZoomYTracking(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ScrollBoxResize(Sender: TObject);
    procedure btnZoomClick(Sender: TObject);
    procedure UpdateZoomFromTrackbars;
    procedure trackZoomXTracking(Sender: TObject);

  public
    procedure ZoomTo(const Rect: TRectF);
    procedure SetZoom(const Value: Single); overload;
    procedure SetZoom(const ValueX, ValueY: Single); overload;
  end;

var
  Form2: TForm2;

implementation
  uses Math;

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
      Size.Size := TSizeF.Create(OriginalWidth * abs(ValueX), OriginalHeight * abs(ValueY)); //don't use Scale to resize (won't work well here), ScaledLayout scales its contents automatically
      Scale.Point := TPointF.Create(sign(ValueX), sign(ValueY));
    end;
    //ScrollBox.InvalidateContentSize;

    EndUpdate;
    end;
end;

procedure TForm2.UpdateZoomFromTrackbars;
begin
  SetZoom(trackZoomX.Value, trackZoomY.Value);
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

procedure TForm2.ZoomTo(const Rect: TRectF); //TODO: doesn't work correctly (zooms to wrong place / center only)
begin
  BeginUpdate;
  SetZoom(Min(ScaledLayout.Width/Rect.Width, ScaledLayout.Height/Rect.Height)); //using Max here would mean you fill the area but get some cliping
  ScrollBox.ViewportPosition := Zoomer.ScreenToLocal(ScaledLayout.LocalToScreen(Rect.Location));
  EndUpdate;
end;

procedure TForm2.btnZoomClick(Sender: TObject);
begin
  var Control := Sender As TButton;
  ShowMessageFmt('Clicked: %s', [Control.Text]);
  ZoomTo(Control.BoundsRect);
end;

end.
