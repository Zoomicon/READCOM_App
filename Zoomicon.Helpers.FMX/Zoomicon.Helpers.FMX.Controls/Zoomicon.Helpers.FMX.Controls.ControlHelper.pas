//Description: Control Helpers for FMX
//Author: George Birbilis (http://zoomicon.com)

unit Zoomicon.Helpers.FMX.Controls.ControlHelper;

interface
  uses
    System.Types,
    FMX.Controls,
    FMX.Graphics, //for TBitmap
    FMX.Layouts,
    FMX.Objects,
    FMX.Types;

  type

    TControlHelper = class helper for TControl //used to expose Scale of TControl which is unfortunately "strict private" so one can't write zooming code that needs to check the Scale of TControls
    protected
      function GetScale: TPosition;
      procedure SetScale(const Value: TPosition);

      function GetOrientation: TOrientation;

      function ObjectAtPoint(const AScreenPoint: TPointF; const RecursionDepth: Integer = 0; const IncludeDisabled: Boolean = false; const IncludeSelf: Boolean = true; const IncludeSubComponents: Boolean = true): IControl; overload;

    (* REMOVED: Delphi 11.1 has TagObject property - if this is used it causes Illegal Access Error when doing an AutoDrag operation in TTreeView (MouseMove calls MakeScreenshot and fails when trying to access Scene property)
      function GetTagObject: TObject;
      procedure SetTagObject(const Value: TObject);
    *)

      function IsSubComponent: Boolean;
      procedure SetSubComponent(const Value: Boolean); //even though there is TComponent.SetSubComponent, Delphi 11 compiler seems to not find it for the property setter below

    public
      function MakeThumbnail(const MaxWidth, MaxHeight: Integer): TBitmap;

      function ObjectAtPoint(const AScreenPoint: TPointF; const Recursive: Boolean; const IncludeDisabled: Boolean = false; const IncludeSelf: Boolean = true; const IncludeSubComponents: Boolean = true): IControl; overload; inline;
      function ObjectAtLocalPoint(const ALocalPoint: TPointF; const Recursive: Boolean = true; const IncludeDisabled: Boolean = false; const IncludeSelf: Boolean = true; const IncludeSubComponents: Boolean = true): IControl; inline;

      procedure FlipHorizontally;
      procedure FlipVertically;

      /// <summary>Converts a rect from the coordinate system of a given <c>AControl</c> to that of the control.</summary>
      function ConvertLocalRectFrom(const AControl: TControl; const AControlLocalRect: TRectF): TRectF;
      /// <summary>Converts a rect from the control's coordinate system to that of the specified control <c>AControl</c>.</summary>
      function ConvertLocalRectTo(const AControl: TControl; const ALocalRect: TRectF): TRectF;

      procedure SelectNext(const CurControl: TControl; const GoFoward: Boolean = true);

      property SubComponent: Boolean read IsSubComponent write SetSubComponent stored false;

    published
      property Scale: TPosition read GetScale write SetScale;
      property Orientation: TOrientation read GetOrientation;
    (* REMOVED: Delphi 11.1 has TagObject property - if this is used it causes Illegal Access Error when doing an AutoDrag operation in TTreeView (MouseMove calls MakeScreenshot and fails when trying to access Scene property)
      property TagObject: TObject read GetTagObject write SetTagObject stored false;
    *)

    end;

implementation
  uses
    //System.Rtti,
    System.Classes, //for csSubComponent
    System.Math; //for Min

{$REGION 'TControlHelper'}

{$region 'Scale'}

function TControlHelper.GetScale: TPosition;
begin
  {
  var context := TRttiContext.Create;
  var typeObj:= context.GetType(Self.ClassInfo);
  var prop := typeObj.GetProperty('Scale');
  result := prop.GetValue(Self).AsType<TPosition>; //Scale property is Readable, no need to check first
}
  result := (Self as IRotatedControl).GetScale; //TControl implements IRotatedControl so we can bypass the fact that the "Scale" property is not accessible
end;

procedure TControlHelper.SetScale(const Value: TPosition);
begin
  {
  var context := TRttiContext.Create;
  var typeObj:= context.GetType(Self.ClassInfo);
  var prop := typeObj.GetProperty('Scale');
  var scale := prop.GetValue(Self).AsType<TPosition>; //Scale property is Readable, no need to check first
  scale.Assign(Value); //do not replace the Scale object via prop.SetValue (else we'll leak objects in memory), just assign value to it (that's what the "strict private" TControl.SetScale does)
}
  (Self as IRotatedControl).SetScale(Value); //TControl implements IRotatedControl so we can bypass the fact that the "Scale" property is not accessible
end;

{$endregion}

{$region 'Orientation'}

function TControlHelper.GetOrientation: TOrientation;
begin
  if (Width >= Height) then
    result := TOrientation.Horizontal
  else
    result := TOrientation.Vertical;
end;

{$endregion}

{$region 'MakeThumbnail'}

function TControlHelper.MakeThumbnail(const MaxWidth, MaxHeight: Integer): TBitmap; //based on TControl.MakeScreenshot

  function GetMaxBitmapRect: TRectF;
  var
    MaxDimensionSize: Integer;
  begin
    MaxDimensionSize := TCanvasManager.DefaultCanvas.GetAttribute(TCanvasAttribute.MaxBitmapSize);
    Result := TRectF.Create(0, 0, Min(MaxWidth, MaxDimensionSize), Min(MaxHeight, MaxDimensionSize)); //cap the rect based on (MaxWidth, MaxHeight)
  end;

var
  SceneScale: Single;
  BitmapRect: TRectF;
begin
  if Scene <> nil then
    SceneScale := Scene.GetSceneScale
  else
    SceneScale := 1;

  // TBitmap has limitation of size. It's a max texture size. If we takes screenshot of control, which exceeds this
  // limitation, we get "Bitmap size to big" exception. So we normalize size for avoiding it.
  BitmapRect := TRectF.Create(0, 0, Width * SceneScale, Height * SceneScale);
  BitmapRect := BitmapRect.PlaceInto(GetMaxBitmapRect);

  Result := TBitmap.Create(Round(BitmapRect.Width), Round(BitmapRect.Height));
  Result.BitmapScale := SceneScale;
  Result.Clear(0);

  if (Result.Width = 0) or (Result.Height = 0) then //extra optimization for items with one or both dimensions set to 0 (avoid calling into any drawing code)
    exit;

  if Result.Canvas.BeginScene then
  try
    PaintTo(Result.Canvas, TRectF.Create(0, 0, Result.Width / SceneScale, Result.Height / SceneScale));
  finally
    Result.Canvas.EndScene;
  end;
end;

{$endregion}

{$region 'ObjectAt'}

function TControlHelper.ObjectAtPoint(const AScreenPoint: TPointF; const RecursionDepth: Integer = 0; const IncludeDisabled: Boolean = false; const IncludeSelf: Boolean = true; const IncludeSubComponents: Boolean = true): IControl; //based on TControl.ObjectAtPoint

  function ShouldTestMouseHits: Boolean; //locally hiding the ShouldTestMouseHits method to override that behaviour below
  begin
    Result := Visible and (IncludeDisabled or AbsoluteEnabled or (csDesigning in ComponentState));
  end;

begin
  if not ShouldTestMouseHits then
    Exit(nil);

  var LP := AScreenPoint;
  if FScene <> nil then
    LP := FScene.ScreenToLocal(LP);
  if (ClipChildren or SmallSizeControl) and not PointInObject(LP.X, LP.Y) then
    Exit(nil);

  if (RecursionDepth > 0) and (ControlsCount > 0) then
    for var I := GetLastVisibleObjectIndex - 1 downto GetFirstVisibleObjectIndex do
    begin
      var Control := Controls[I];
      if not Control.GetVisible then
        Continue;

      var NewObj := Control.ObjectAtPoint(AScreenPoint, RecursionDepth - 1, IncludeDisabled, true, IncludeSubComponents); //not propagating the IncludeSelf, need the default true value for children else it would find none
      if Assigned(NewObj) and (IncludeSubComponents or not TControl(NewObj).SubComponent) then
        Exit(NewObj);
    end;

  Result := nil;

  if IncludeSelf and PointInObject(LP.X, LP.Y) and CheckHitTest(HitTest) then //TODO: allow to have option to ignore hit test //not checking if the item is a SubComponent itself, checked it at its parent before doing recursion (so that we won't end up checking it for the top item)
    Result := Self;
end;

function TControlHelper.ObjectAtPoint(const AScreenPoint: TPointF; const Recursive: Boolean; const IncludeDisabled: Boolean = false; const IncludeSelf: Boolean = true; const IncludeSubComponents: Boolean = true): IControl;
begin
  if Recursive and (not IncludeDisabled) and IncludeSelf then
    result := ObjectAtPoint(AScreenPoint) //the default Delphi implementation (unbounded recursion)
  else
    result := ObjectAtPoint(AScreenPoint, 1, IncludeDisabled, IncludeSelf, IncludeSubComponents);
end;

function TControlHelper.ObjectAtLocalPoint(const ALocalPoint: TPointF; const Recursive: Boolean = true; const IncludeDisabled: Boolean = false; const IncludeSelf: Boolean = true; const IncludeSubComponents: Boolean = true): IControl;
begin
  result := ObjectAtPoint(LocalToScreen(ALocalPoint), Recursive, IncludeDisabled, IncludeSelf, IncludeSubComponents);
end;

{$endregion}

{$region 'Flip'}

procedure TControlHelper.FlipHorizontally;
begin
  BeginUpdate;

  //make sure after flipping we end up having the same bounds (scaling is done from top-left corner)
  if (Scale.X > 0) then
    Position.X := Position.X + Width
  else
    Position.X := Position.X - Width;

  Scale.X := -Scale.X;

  EndUpdate;
end;

procedure TControlHelper.FlipVertically;
begin
  BeginUpdate;

  //make sure after flipping we end up having the same bounds (scaling is done from top-left corner)
  if (Scale.Y > 0) then
    Position.Y := Position.Y + Height
  else
    Position.Y := Position.Y - Height;

  Scale.Y := -Scale.Y;

  EndUpdate;
end;

{$endregion}

{$region 'TagObject'}
(* //REMOVED: see comment at interface section
function TControlHelper.GetTagObject: TObject;
begin
  result := TObject(Tag);
end;

procedure TControlHelper.SetTagObject(const Value: TObject);
begin
  Tag := NativeInt(Value);
end;
*)
{$endregion}

{$region 'ConvertLocalRect'}

function TControlHelper.ConvertLocalRectFrom(const AControl: TControl; const AControlLocalRect: TRectF): TRectF;
begin
  result := TRectF.Create(ConvertLocalPointFrom(AControl, AControlLocalRect.TopLeft), ConvertLocalPointFrom(AControl, AControlLocalRect.BottomRight));
end;

function TControlHelper.ConvertLocalRectTo(const AControl: TControl; const ALocalRect: TRectF): TRectF;
begin
  result := TRectF.Create(ConvertLocalPointTo(AControl, ALocalRect.TopLeft), ConvertLocalPointTo(AControl, ALocalRect.BottomRight));
end;

{$endregion}

{$region 'SubComponent'}

function TControlHelper.IsSubComponent: Boolean;
begin
  result := csSubComponent in FComponentStyle;
end;

procedure TControlHelper.SetSubComponent(const Value: Boolean); //see comment at method definition on why this method is needed
begin
  inherited SetSubComponent(Value);
end;

{$endregion}

{$region 'SelectNext'}

//TODO: simplify
procedure TControlHelper.SelectNext(const CurControl: TControl; const GoFoward: Boolean = true); //based on https://codeverge.com/embarcadero.delphi.firemonkey/fmx-how-to-programmatically-mov/2031600
var
  tablist          : ITabList;
  next             : IControl;
  parent, control: TControl;
begin
  control := CurControl;
  parent  := control.ParentControl;
  repeat
    tablist := parent.GetTabList;
    next := tablist.FindNextTabStop(control, GoFoward, True);
    if (not Assigned(next)) or (TControl(next) = control) then
    begin
      control := parent;
      parent  := parent.ParentControl;
    end
    else
      Break;
  until parent = nil;

  if Assigned(next) then
    control := TControl(next)
  else
    control := nil;

  if Assigned(control) then
    control.SetFocus;
end;

{$endregion}

{$ENDREGION}

end.

