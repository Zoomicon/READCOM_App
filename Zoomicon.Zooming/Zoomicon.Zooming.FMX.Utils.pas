unit Zoomicon.Zooming.FMX.Utils;

{-$DEFINE CONTROLSCALEHELPER} //Uncomment only if really needed - else it will make code that uses Scale at TControl descendents that expose that property slower since it uses RTTI

interface
  uses
    System.Types,
    FMX.Controls,
    FMX.Layouts,
    FMX.Types,
    FMX.Objects;

  function GetScrollContentParent(const FmxObject: TFmxObject): TScrollContent;
  function GetScrollBoxParent(const FmxObject: TFmxObject): TCustomScrollBox;

  type

    TControlFocusHelper = class helper for TControl
    public
      procedure SelectNext(const CurControl: TControl; const GoFoward: Boolean = true);
    end;

    {$IFDEF CONTROLSCALEHELPER}
    TControlScaleHelper = class helper(TControlFocusHelper) for TControl //used to expose Scale of TControl which is unfortunately "strict private" so one can't write zooming code that needs to check the Scale of TControls
    protected
      function GetScale: TPosition;
      procedure SetScale(const Value: TPosition);
    published
      property Scale: TPosition read GetScale write SetScale;
    end;
    {$ENDIF}

    TCustomScrollBoxViewportHelper = class helper for TCustomScrollBox
    protected
      function GetViewportSize: TSizeF;
    public
      /// <summary>Size of view port of the ScrollBox's content.</summary>
      property ViewportSize: TSizeF read GetViewportSize;
    end;

    TScaledLayoutScalingFactorHelper = class helper for TScaledLayout
    protected
      function GetScalingFactor: TPointF;
    public
      property ScalingFactor: TPointF read GetScalingFactor;
    end;

implementation
  {$IFDEF CONTROLSCALEHELPER}
  uses System.Rtti; //uncomment only if you uncomment TControlScaleHelper
  {$ENDIF}

{$REGION 'Functions'}

function GetScrollContentParent(const FmxObject: TFmxObject): TScrollContent;
begin
  var parent := FmxObject.Parent;
  if parent is TScrollContent then
    result := (parent as TScrollContent)
  else
    result := nil;
end;

function GetScrollBoxParent(const FmxObject: TFmxObject): TCustomScrollBox;
begin
  var ScrollContent := GetScrollContentParent(FmxObject);
  if ScrollContent = nil then
    result := nil
  else
    result := ScrollContent.ScrollBox;
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

{$REGION 'TCustomScrollBoxViewportHelper'}

function TCustomScrollBoxViewportHelper.GetViewportSize: TSizeF;
begin
  result := Size.Size; //TODO: adjust for scrollbar sizes
end;

{$ENDREGION}

{$REGION 'TScaledLayoutScalingFactorHelper'}

function TScaledLayoutScalingFactorHelper.GetScalingFactor: TPointF;
begin
  result := PointF(Width/OriginalWidth, Height/OriginalHeight); //need to use OriginalWidth/OriginalHeight, not Width/Height (since we may have resized)
end;

{$ENDREGION}

end.

