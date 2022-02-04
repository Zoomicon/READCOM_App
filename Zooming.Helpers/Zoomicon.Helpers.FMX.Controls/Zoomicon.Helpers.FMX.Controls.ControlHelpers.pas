//Description: Control Helpers for FMX
//Author: George Birbilis (http://zoomicon.com)

unit Zoomicon.Helpers.FMX.Controls.ControlHelpers;

interface
  uses
    System.Types,
    FMX.Controls,
    FMX.Layouts,
    FMX.Types,
    FMX.Objects;

  type

    TControlScaleHelper = class helper for TControl //used to expose Scale of TControl which is unfortunately "strict private" so one can't write zooming code that needs to check the Scale of TControls
    protected
      function GetScale: TPosition;
      procedure SetScale(const Value: TPosition);

    published
      property Scale: TPosition read GetScale write SetScale;
    end;

    TControlObjectAtHelper = class helper(TControlScaleHelper) for TControl //using class helper inheritance, else only last helper defined for same Class is seen by compiler
    protected
      function ObjectAtPoint(const AScreenPoint: TPointF; const RecursionDepth: Integer = 0; const IncludeDisabled: Boolean = false; const IncludeSelf: Boolean = true): IControl; overload;

    public
      function ObjectAtPoint(const AScreenPoint: TPointF; const Recursive: Boolean; const IncludeDisabled: Boolean = false; const IncludeSelf: Boolean = true): IControl; overload; inline;
      function ObjectAtLocalPoint(const ALocalPoint: TPointF; const Recursive: Boolean = true; const IncludeDisabled: Boolean = false; const IncludeSelf: Boolean = true): IControl; inline;
    end;

    TControlFlipHelper = class helper(TControlObjectAtHelper) for TControl
      procedure FlipHorizontally;
      procedure FlipVertically;
    end;

    TControlTagObjectHelper = class helper(TControlFlipHelper) for TControl
    protected
      function GetTagObject: TObject;
      procedure SetTagObject(const Value: TObject);

    public
      property TagObject: TObject read GetTagObject write SetTagObject stored false;
    end;

    TControlConvertLocalRectHelper = class helper(TControlTagObjectHelper) for TControl
    public
      /// <summary>Converts a rect from the coordinate system of a given <c>AControl</c> to that of the control.</summary>
      function ConvertLocalRectFrom(const AControl: TControl; const AControlLocalRect: TRectF): TRectF;
      /// <summary>Converts a rect from the control's coordinate system to that of the specified control <c>AControl</c>.</summary>
      function ConvertLocalRectTo(const AControl: TControl; const ALocalRect: TRectF): TRectF;
    end;

    TControlSubComponentHelper = class helper(TControlConvertLocalRectHelper) for TControl
    protected
      function IsSubComponent: Boolean;
      procedure SetSubComponent(const Value: Boolean); //even though there is TComponent.SetSubComponent, Delphi 11 compiler seems to not find it for the property setter below

    public
      property SubComponent: Boolean read IsSubComponent write SetSubComponent stored false;
    end;

    TControlFocusHelper = class helper(TControlSubComponentHelper) for TControl
    public
      procedure SelectNext(const CurControl: TControl; const GoFoward: Boolean = true);
    end;

implementation
  uses
    //System.Rtti,
    System.Classes; //for csSubComponent

{$REGION 'TControlScaleHelper'}

function TControlScaleHelper.GetScale: TPosition;
begin
  {
  var context := TRttiContext.Create;
  var typeObj:= context.GetType(Self.ClassInfo);
  var prop := typeObj.GetProperty('Scale');
  result := prop.GetValue(Self).AsType<TPosition>; //Scale property is Readable, no need to check first
}
  result := (Self as IRotatedControl).GetScale; //TControl implements IRotatedControl so we can bypass the fact that the "Scale" property is not accessible
end;

procedure TControlScaleHelper.SetScale(const Value: TPosition);
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

{$ENDREGION}

{$region 'TControlObjectAtHelper'}

function TControlObjectAtHelper.ObjectAtPoint(const AScreenPoint: TPointF; const RecursionDepth: Integer = 0; const IncludeDisabled: Boolean = false; const IncludeSelf: Boolean = true): IControl; //based on TControl.ObjectAtPoint

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

      var NewObj := Control.ObjectAtPoint(AScreenPoint, RecursionDepth - 1, IncludeDisabled); //not propagating the IncludeSelf, need the default true value for children else it would find none
      if Assigned(NewObj) then
        Exit(NewObj);
      end;

  Result := nil;

  if IncludeSelf and PointInObject(LP.X, LP.Y) and CheckHitTest(HitTest) then //TODO: allow to have option to ignore hit test
    Result := Self;
end;

function TControlObjectAtHelper.ObjectAtPoint(const AScreenPoint: TPointF; const Recursive: Boolean; const IncludeDisabled: Boolean = false; const IncludeSelf: Boolean = true): IControl;
begin
  if Recursive and (not IncludeDisabled) and IncludeSelf then
    result := ObjectAtPoint(AScreenPoint) //the default Delphi implementation (unbounded recursion)
  else
    result := ObjectAtPoint(AScreenPoint, 1, IncludeDisabled, IncludeSelf);
end;

function TControlObjectAtHelper.ObjectAtLocalPoint(const ALocalPoint: TPointF; const Recursive: Boolean = true; const IncludeDisabled: Boolean = false; const IncludeSelf: Boolean = true): IControl;
begin
  result := ObjectAtPoint(LocalToScreen(ALocalPoint), Recursive, IncludeDisabled, IncludeSelf);
end;

{$endregion}

{$region 'TControlFlipHelper'}

procedure TControlFlipHelper.FlipHorizontally;
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

procedure TControlFlipHelper.FlipVertically;
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

{$region 'TControlTagObjectHelper'}

function TControlTagObjectHelper.GetTagObject: TObject;
begin
  result := TObject(Tag);
end;

procedure TControlTagObjectHelper.SetTagObject(const Value: TObject);
begin
  Tag := NativeInt(Value);
end;

{$endregion}

{$region 'TControlConvertLocalRectHelper'}

function TControlConvertLocalRectHelper.ConvertLocalRectFrom(const AControl: TControl; const AControlLocalRect: TRectF): TRectF;
begin
  result := TRectF.Create(ConvertLocalPointFrom(AControl, AControlLocalRect.TopLeft), ConvertLocalPointFrom(AControl, AControlLocalRect.BottomRight));
end;

function TControlConvertLocalRectHelper.ConvertLocalRectTo(const AControl: TControl; const ALocalRect: TRectF): TRectF;
begin
  result := TRectF.Create(ConvertLocalPointTo(AControl, ALocalRect.TopLeft), ConvertLocalPointTo(AControl, ALocalRect.BottomRight));
end;

{$endregion}

{$REGION 'TControlSubComponentHelper'}

function TControlSubComponentHelper.IsSubComponent: Boolean;
begin
  result := csSubComponent in FComponentStyle;
end;

procedure TControlSubComponentHelper.SetSubComponent(const Value: Boolean); //see comment at method definition on why this method is needed
begin
  inherited SetSubComponent(Value);
end;

{$ENDREGION}

{$REGION 'TControlFocusHelper'}

//TODO: simplify
procedure TControlFocusHelper.SelectNext(const CurControl: TControl; const GoFoward: Boolean = true); //based on https://codeverge.com/embarcadero.delphi.firemonkey/fmx-how-to-programmatically-mov/2031600
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

{$ENDREGION}

end.

