unit Zoomicon.FMX.Utils;

interface
  uses
    System.Types,
    FMX.Layouts,
    FMX.Types,
    FMX.Objects;

  function GetScalingFactor(const ScaledLayout: TScaledLayout): TPointF;
  function GetScrollContentParent(const FmxObject: TFmxObject): TScrollContent;
  function GetScrollBoxParent(const FmxObject: TFmxObject): TCustomScrollBox;

implementation

function GetScalingFactor(const ScaledLayout: TScaledLayout): TPointF;
begin
  with ScaledLayout do
    result := TPointF.Create(Width/OriginalWidth, Height/OriginalHeight); //need to use OriginalWidth/OriginalHeight, not Width/Height (since we may have resized the form)
end;

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

end.
