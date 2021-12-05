unit Zoomicon.Helpers.FMX.Layouts.ScaledLayoutHelpers;

interface
  uses
    FMX.Layouts,
    System.Types;

  type
    TScaledLayoutScalingFactorHelper = class helper for TScaledLayout
    protected
      function GetScalingFactor: TPointF;
    public
      property ScalingFactor: TPointF read GetScalingFactor;
    end;

implementation

{$REGION 'TScaledLayoutScalingFactorHelper'}

function TScaledLayoutScalingFactorHelper.GetScalingFactor: TPointF;
begin
  result := PointF(Width/OriginalWidth, Height/OriginalHeight); //need to use OriginalWidth/OriginalHeight, not Width/Height (since we may have resized)
end;

{$ENDREGION}

end.
