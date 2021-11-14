unit Zoomicon.FMX.Utils;

interface
  uses
    System.Types,
    FMX.Layouts,
    FMX.Types,
    FMX.Objects;

  function GetScrollContentParent(const FmxObject: TFmxObject): TScrollContent;
  function GetScrollBoxParent(const FmxObject: TFmxObject): TCustomScrollBox;

  type
    TCustomScrollBoxHelper = class helper for TCustomScrollBox
    protected
      function GetViewportSize: TSizeF;
    public
      /// <summary>Size of view port of the ScrollBox's content.</summary>
      property ViewportSize: TSizeF read GetViewportSize;
    end;

    TScaledLayoutHelper = class helper for TScaledLayout
    protected
      function GetScalingFactor: TPointF;
    public
      property ScalingFactor: TPointF read GetScalingFactor;
    end;

implementation

{ TCustomScrollBoxHelper }

function TCustomScrollBoxHelper.GetViewportSize: TSizeF;
begin
  result := Size.Size; //TODO: adjust for scrollbar sizes
end;

{ TScaledLayoutHelper }

function TScaledLayoutHelper.GetScalingFactor: TPointF;
begin
  result := PointF(Width/OriginalWidth, Height/OriginalHeight); //need to use OriginalWidth/OriginalHeight, not Width/Height (since we may have resized)
end;

{ Misc }

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

