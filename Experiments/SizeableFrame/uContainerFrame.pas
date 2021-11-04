unit uContainerFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Objects,
  uContentFrame;

type
  TContainerFrame = class(TFrame)
    BackgroundRectangle: TRectangle;
  protected
    fProportional: boolean;
    fEditMode: boolean;
    procedure SetProportional(value: boolean);
    procedure SetEditMode(value: boolean);
  published
    procedure AddContentFrame;
    property Proportional: boolean read fProportional write SetProportional;
    property EditMode: boolean read fEditMode write SetEditMode;
  end;

implementation

{$R *.fmx}

{$REGION 'Properties'}

{$REGION 'EditMode'}

procedure TContainerFrame.SetEditMode(value: boolean);
begin
  fEditMode := value;
  if (ControlsCount <> 0) then
    for var c in Controls do
      if (c is TSelection) then
         TSelection(c).HideSelection := not value;
end;

{$ENDREGION}

{$REGION 'Proportional'}

procedure TContainerFrame.SetProportional(value: boolean);
begin
  if value then Align := TAlignLayout.Fit else Align := TAlignLayout.Scale;

  fProportional := value;
  if (ControlsCount <> 0) then
    for var c in Controls do
      if (c is TSelection) then
         TSelection(c).Proportional := value;
end;

{$ENDREGION}

{$ENDREGION}

{$REGION 'Methods'}

procedure TContainerFrame.AddContentFrame;
var
  sel: TSelection;
begin
  //Create Selector
  sel:= TSelection.Create(self);
  sel.Name := 'sel' + FloatToStr(Random).Replace(',', '');
  sel.Align := TAlignLayout.Scale;
  sel.HideSelection := not EditMode;
  sel.Proportional := Proportional;
  sel.Proportional := true; //Preserve aspect ratio when resizing
  sel.GripSize := 8;

  begin //Create ContentFrame
    var c: TContentFrame;
    c := TContentFrame.Create(sel);
    c.Name := 'c' + FloatToStr(Random).Replace(',', '');
    c.Align := TAlignLayout.Contents;
    c.Parent := sel;
  end;

  sel.Parent := self;
end;

{$ENDREGION}

end.
