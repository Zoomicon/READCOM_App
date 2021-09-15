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
var
  i: Integer;
  c: TFmxObject;
begin
  fEditMode := value;
  for i := 0 to ChildrenCount-1 do
  begin
    c := Children[i];
    if (c is TSelection) then
       begin
       TSelection(c).HideSelection := not value;
       end;
  end;
end;

{$ENDREGION}

{$REGION 'Proportional'}

procedure TContainerFrame.SetProportional(value: boolean);
var
  i: Integer;
  c: TFmxObject;
begin
  if value then Align := TAlignLayout.Fit else Align := TAlignLayout.Scale;

  fProportional := value;
  for i := 0 to ChildrenCount-1 do
  begin
    c := Children[i];
    if (c is TSelection) then
       begin
       TSelection(c).Proportional := value;
       end;
  end;
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
