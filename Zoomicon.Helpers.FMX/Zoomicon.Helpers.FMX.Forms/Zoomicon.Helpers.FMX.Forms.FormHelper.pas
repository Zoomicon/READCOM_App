unit Zoomicon.Helpers.FMX.Forms.FormHelper;

interface
uses
  FMX.Controls, //for TOrientation
  FMX.Forms; //for TForm

type
  TFormHelper = class helper for TForm
  protected
    function GetOrientation: TOrientation;

  published
    property Orientation: TOrientation read GetOrientation;
  end;

implementation

{$REGION 'TFormHelper'}

{$region 'Orientation'}

function TFormHelper.GetOrientation: TOrientation;
begin
  if (Width >= Height) then
    result := TOrientation.Horizontal
  else
    result := TOrientation.Vertical;
end;

{$endregion}

{$ENDREGION}

end.

