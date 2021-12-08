unit Zoomicon.Helpers.FMX.Controls.ControlHelpers;

{-$DEFINE CONTROLSCALEHELPER} //Uncomment only if really needed - else it will make code that uses Scale at TControl descendents that expose that property slower since it uses RTTI

interface
  uses
    System.Types,
    FMX.Controls,
    FMX.Layouts,
    FMX.Types,
    FMX.Objects;

  type

    TControlObjectAtHelper = class helper for TControl
    protected
      function ObjectAtPoint(const AScreenPoint: TPointF; RecursionDepth: Integer = 0): IControl; overload;

    public
      function ObjectAtPoint(const AScreenPoint: TPointF; Recursive: Boolean): IControl; overload; inline;
      function ObjectAtLocalPoint(const ALocalPoint: TPointF; Recursive: Boolean = true): IControl; inline;
    end;

    TControlConvertLocalRectHelper = class helper(TControlObjectAtHelper) for TControl
    public
      /// <summary>Converts a rect from the coordinate system of a given <c>AControl</c> to that of the control.</summary>
      function ConvertLocalRectFrom(const AControl: TControl; const AControlLocalRect: TRectF): TRectF;
      /// <summary>Converts a rect from the control's coordinate system to that of the specified control <c>AControl</c>.</summary>
      function ConvertLocalRectTo(const AControl: TControl; const ALocalRect: TRectF): TRectF;
    end;

    TControlFocusHelper = class helper(TControlConvertLocalRectHelper) for TControl
    public
      procedure SelectNext(const CurControl: TControl; const GoFoward: Boolean = true);
    end;

    {$IFDEF CONTROLSCALEHELPER}
    TControlScaleHelper = class helper(TControlFocusHelper) for TControl //used to expose Scale of TControl which is unfortunately "strict private" so one can't write zooming code that needs to check the Scale of TControls //using class helper inheritance, else only last helper defined for same Class is seen by compiler
    protected
      function GetScale: TPosition;
      procedure SetScale(const Value: TPosition);
    published
      property Scale: TPosition read GetScale write SetScale;
    end;
    {$ENDIF}

implementation
  {$IFDEF CONTROLSCALEHELPER}
  uses System.Rtti; //uncomment only if you uncomment TControlScaleHelper
  {$ENDIF}

{$region 'TControlObjectAtHelper'}

function TControlObjectAtHelper.ObjectAtPoint(const AScreenPoint: TPointF; RecursionDepth: Integer = 0): IControl; //based on TControl.ObjectAtPoint
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

      var NewObj := Control.ObjectAtPoint(AScreenPoint, RecursionDepth - 1);
      if Assigned(NewObj) then
        Exit(NewObj);
      end;

  Result := nil;

  if PointInObject(LP.X, LP.Y) {and CheckHitTest(HitTest)} then //TODO: allow to have option to ignore hit test
    Result := Self;
end;

function TControlObjectAtHelper.ObjectAtPoint(const AScreenPoint: TPointF; Recursive: Boolean): IControl;
begin
  if Recursive then
    result := ObjectAtPoint(AScreenPoint)
  else
    result := ObjectAtPoint(AScreenPoint, 1);
end;

function TControlObjectAtHelper.ObjectAtLocalPoint(const ALocalPoint: TPointF; Recursive: Boolean = true): IControl;
begin
  result := ObjectAtPoint(LocalToScreen(ALocalPoint), Recursive);
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

{$REGION 'TControlScaleHelper'}
{$IFDEF CONTROLSCALEHELPER}

function TControlScaleHelper.GetScale: TPosition;
begin
  var context := TRttiContext.Create;
  var typeObj:= context.GetType(Self.ClassInfo);
  var prop := typeObj.GetProperty('Scale');
  result := prop.GetValue(Self).AsType<TPosition>; //Scale property is Readable, no need to check first
end;

procedure TControlScaleHelper.SetScale(const Value: TPosition);
begin
  var context := TRttiContext.Create;
  var typeObj:= context.GetType(Self.ClassInfo);
  var prop := typeObj.GetProperty('Scale');
  var scale := prop.GetValue(Self).AsType<TPosition>; //Scale property is Readable, no need to check first
  scale.Assign(Value); //do not replace the Scale object via prop.SetValue (else we'll leak objects in memory), just assign value to it (that's what the "strict private" TControl.SetScale does)
end;

{$ENDIF}
{$ENDREGION}

end.

