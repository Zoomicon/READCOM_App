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
    Button1: TButton;
    procedure trackZoomYTracking(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ScrollBoxResize(Sender: TObject);
    procedure btnZoomClick(Sender: TObject);
    procedure UpdateZoomFromTrackbars;
    procedure trackZoomXTracking(Sender: TObject);

  public
    procedure ZoomTo(const Control: TControl);
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
      Size.Size := TSizeF.Create(OriginalWidth * Abs(ValueX), OriginalHeight * Abs(ValueY)); //don't use Scale to resize (won't work well here), ScaledLayout scales its contents automatically
      Scale.Point := TPointF.Create(Sign(ValueX), Sign(ValueY));
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

procedure TForm2.ZoomTo(const Control: TControl); //TODO: doesn't work correctly (when window has been resized it fails to position viewport correctly [wrong scrolling])
begin
  //BeginUpdate; //MUST NOT DO, WON'T DO PANNING CALCULATION CORRECTLY
  var rect := Control.BoundsRect;

  var zoomFactor := Min(ScaledLayout.Width/Rect.Width, ScaledLayout.Height/Rect.Height); //TODO: check if OriginalWidth/OriginalHeight always stays same
  SetZoom(zoomFactor); //using Max here would mean you fill the area but get some cliping

  //ScrollBox.InvalidateContentSize;

  //var ScrollPoint := ScrollBox.Content.AbsoluteToLocal(Control.ParentControl.LocalToAbsolute(Rect.Location));
  //var ScrollPoint := ScrollBox.Content.Position + Zoomer.Position.Point + (ScaledLayout.Position.Point + Rect.Location/zoomFactor;
  //ScrollBox.ScrollTo(-ScrollPoint.X + (ScrollBox.Width - Rect.Width)/2, -ScrollPoint.Y + (ScrollBox.Height - Rect.Height)/2);

  var AdjRect := ScrollBox.Content.AbsoluteToLocal(Control.ParentControl.LocalToAbsolute(Rect));
  ScrollBox.ViewportPosition := AdjRect.Location - TPointF.Create((ScrollBox.Size.Width - AdjRect.Size.Width)/2, (ScrollBox.Size.Height - AdjRect.Size.Height)/2);

  //EndUpdate;
end;

procedure TForm2.btnZoomClick(Sender: TObject);
begin
  var Control := Sender As TButton;
  //ShowMessageFmt('Clicked: %s - button scale: (%f,%f)', [Control.Text, Control.Scale.X, Control.Scale.Y]);
  //with ScrollBox.ViewportPosition do ShowMessageFmt('ViewPortPosition before: (%f, %f)', [x, y]);
  ZoomTo(Control);
  //with ScrollBox.ViewportPosition do ShowMessageFmt('ViewPortPosition after: (%f, %f)', [x, y]);
end;

end.
