unit Zoomicon.Zooming.FMX.ZoomFrame;

interface

uses
  Zoomicon.Zooming.Models, //for IZoomable
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts;

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
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;

    //IZoomable
    function GetZoom: TPointF;
    procedure ZoomTo(const Control: TControl = nil; const KeepRatio: Boolean = true); //ZoomTo(nil) zooms to all content
    procedure SetZoom(const ValueX, ValueY: Single); overload;
    procedure SetZoom(const Value: TPointF); overload;
    procedure SetZoom(const Value: Single); overload;

    procedure SetZoomerSize(const ASize: TSizeF); //TODO: use some other better way for clients to tell us that their contents resized?

  published
    property Proportional: Boolean read IsProportional write SetProportional;
    property Zoom: TPointF read GetZoom write SetZoom;
    property OnZoomChanged: TZoomChangedEvent read FOnZoomChanged write FOnZoomChanged;
    property ZoomControlsVisible: Boolean read GetZoomControlsVisible write SetZoomControlsVisible default DEFAULT_ZOOM_CONTROLS_VISIBLE;
  end;

procedure Register;

implementation
  uses
    Zoomicon.Helpers.FMX.Layouts.ScrollBoxHelpers, //for GetScrollBoxParent
    Zoomicon.Helpers.FMX.Layouts.ScaledLayoutHelpers, //for TScaledLayout.ScalingFactor
    Math; //for Sign

{$R *.fmx}

{TZoomFrame}

constructor TZoomFrame.Create(AOwner: TComponent);
begin
  inherited;

  ScrollBox.SetSubComponent(true);
  ScrollBox.Stored := false;

  ZoomControls.SetSubComponent(true);
  ZoomControls.Stored := false; //not storing, will load from FMX and the apply wrapper properties //no need to set this for its children controls too

  ZoomControlsVisible := DEFAULT_ZOOM_CONTROLS_VISIBLE;
end;

procedure TZoomFrame.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  inherited;
  if (ScaledLayout.ChildrenCount <> 0) then //Children can be nil, so check ChildrenCount first
    for var Control in ScaledLayout.Children do
      if not ((csDesigning in ComponentState) and (Control.ClassName = 'TGrabHandle.TGrabHandleRectangle')) then //this is to not store Delphi IDE designer's selection grab handles
        Proc(Control); //Store all children of ScaledLayout as if they were ours
end;

//TODO: seems to have issue in design mode, not loading all children back into the ScaledLayout (they seem to move into ZoomLayout)
procedure TZoomFrame.Loaded;
begin
  BeginUpdate;

  //reparent children after loading (since at GetChildren we stored children of ScaledLayout as ours), except for the ScrollBox and the ZoomControls
  if (ControlsCount <> 0) then //extra optimization (we didn't need to check for <> 0, since we always have two children, the ScrollBox and the ZoomControls ones) //don't use >2 as extra optimization, may be called multiple times when we have inherited Frames
    For var Control in Controls do
      if (Control <> ScrollBox) and (Control <> ZoomControls) then
        begin
        //ScaledLayout.InsertComponent(Control); //this won't show the child in structure view in the IDE
        Control.Parent := ScaledLayout; //this seems to only keep one child when loading form in the IDE, others eventually move to the frame
        end;

  Zoomer.Align := TAlignLayout.Center; //at design mode we have it set to TAlignLayout.Client
  UpdateZoomFromTrackbars;

  EndUpdate;

  inherited;
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
      Scale.Point := PointF(Sign(ValueX), Sign(ValueY));
    end;
    //ScrollBox.InvalidateContentSize;

    //update track bars
    trackZoomX.BeginUpdate; trackZoomX.ValueRange.Value := ValueX; trackZoomX.EndUpdate;
    trackZoomY.BeginUpdate; trackZoomY.ValueRange.Value := ValueY; trackZoomY.EndUpdate;

    EndUpdate;
    end;
end;

//TODO: take in mind scrollbar size
procedure TZoomFrame.ZoomTo(const Control: TControl = nil; const KeepRatio: Boolean = true); //ZoomTo(nil) zooms to all content
begin
  {$region 'Zoom'}
  //BeginUpdate; //Not needed

  var ZoomerAbsRect := Zoomer.AbsoluteRect;
  var ControlAbsRect: TRectF;
  if Assigned(Control) then
    ControlAbsRect := Control.AbsoluteRect
  else
    ControlAbsRect := ScaledLayout.AbsoluteRect;

  if KeepRatio then
    begin
      var ZoomFactor := Min(ZoomerAbsRect.Width / ControlAbsRect.Width, ZoomerAbsRect.Height / ControlAbsRect.Height);
      SetZoom(ZoomFactor);
    end
  else
    begin
      var ZoomFactor := PointF(ZoomerAbsRect.Width / ControlAbsRect.Width, ZoomerAbsRect.Height / ControlAbsRect.Height);
      SetZoom(ZoomFactor);
    end;

  //RecalcAbsolute; //TForm doesn't seem to have such method (would probably be needed if we wrapped everything in a single BeginUpdate/EndUpdate, haven't made that work ok though)
  //EndUpdate; //make sure we do this here if needed, not at the end, else scrolling calculations won't work

  {$endregion}

  {$region 'Pan (center)'}

  //NEEDED TO RECALCULATE AFTER ZOOMING IN ORDER TO FIND THE CORRECT CENTER
  if Assigned(Control) then
    ControlAbsRect := Control.AbsoluteRect
  else
    ControlAbsRect := ScaledLayout.AbsoluteRect;

  var ZoomerParent := Zoomer.ParentControl;
  var CenterPointNewCoords := ZoomerParent.AbsoluteToLocal(ControlAbsRect.CenterPoint);

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

  {$endregion}
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
  var Viewport := Sender as TControl;
  var ViewportSize := Viewport.Size.Size;
  if not ViewportSize.IsZero then
    SetZoomerSize(ViewportSize);
end;

procedure TZoomFrame.SetZoomerSize(const ASize: TSizeF); //TODO: add to rest of code (and to related experiment)
begin
  var oldZoom := Zoomer.ScalingFactor;

  BeginUpdate;

  Zoomer.Size.Size := ASize;
  with Zoomer do
  begin
    OriginalWidth := Width;
    OriginalHeight := Height;
  end;

  SetZoom(oldZoom); //UpdateZoomFromTrackbars;
  //ScrollBox.InvalidateContentSize; //TODO: probably needed when we explicitly call this method

  EndUpdate;
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
