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

    TControlFocusHelper = class helper for TControl
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

