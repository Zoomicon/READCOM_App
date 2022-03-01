unit Zoomicon.Helpers.FMX.Layouts.ScrollBoxHelpers;

interface
  uses
    FMX.Layouts,
    FMX.Types,
    System.Types;

  type

    TScrollBoxViewportHelper = class helper for TCustomScrollBox
    protected
      function GetViewportSize: TSizeF;
    public
      /// <summary>Size of view port of the ScrollBox's content.</summary>
      property ViewportSize: TSizeF read GetViewportSize;
    end;

  function GetScrollContentParent(const FmxObject: TFmxObject): TScrollContent;
  function GetScrollBoxParent(const FmxObject: TFmxObject): TCustomScrollBox;

implementation

{$REGION 'TScrollBoxViewportHelper'}

function TScrollBoxViewportHelper.GetViewportSize: TSizeF;
begin
  result := Size.Size; //TODO: adjust for scrollbar sizes
end;

{$ENDREGION}

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

end.
