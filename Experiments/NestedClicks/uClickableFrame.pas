unit uClickableFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects, FMX.Layouts;

type
  TClickableFrame = class(TFrame)
    LabelName: TLabel;
    cbHitTest: TCheckBox;
    cbFixDblClick: TCheckBox;
    cbEnabled: TCheckBox;
    btnEnableChildren: TButton;
    UpdateTimer: TTimer;
    cbAbsoluteEnabled: TCheckBox;
    cbTabStop: TCheckBox;
    Layout: TLayout;
    Rectangle: TRectangle;
    procedure cbHitTestChange(Sender: TObject);
    procedure cbEnabledChange(Sender: TObject);
    procedure btnEnableChildrenClick(Sender: TObject);
    procedure UpdateTimerTimer(Sender: TObject);
    procedure cbTabStopChange(Sender: TObject);
  protected
    procedure DblClick; override;
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
  end;

implementation

{$R *.fmx}

{$region 'Utils'}

function ShiftToStr(Shift: TShiftState): String;
begin
  var list := TStringList.Create(#0, ',');

  if ssShift in Shift then list.Add('Shift');
  if ssAlt in Shift then list.Add('Alt');
  if ssCtrl in Shift then list.Add('Ctrl');
  if ssLeft in Shift then list.Add('Left');
  if ssRight in Shift then list.Add('Right');
  if ssMiddle in Shift then list.Add('Middle');
  if ssDouble in Shift then list.Add('Double');
  if ssTouch in Shift then list.Add('Touch');
  if ssPen in Shift then list.Add('Pen');
  if ssCommand in Shift then list.Add('Command');
  if ssHorizontal in Shift then list.Add('Horizontal');

  result := '[' + list.DelimitedText + ']';
end;

{$endregion}

{TClickableFrame}

procedure TClickableFrame.cbHitTestChange(Sender: TObject);
begin
  HitTest := cbHitTest.IsChecked;
end;

procedure TClickableFrame.cbTabStopChange(Sender: TObject);
begin
  TabStop := cbTabStop.IsChecked;
end;

procedure TClickableFrame.cbEnabledChange(Sender: TObject);
begin
  Enabled := cbEnabled.IsChecked;
end;

procedure TClickableFrame.UpdateTimerTimer(Sender: TObject);
begin
  LabelName.Text := Name;
  cbHitTest.IsChecked := HitTest;
  cbEnabled.IsChecked := Enabled;
  cbAbsoluteEnabled.IsChecked := AbsoluteEnabled;
  cbTabStop.IsChecked := TabStop;
end;

procedure TClickableFrame.btnEnableChildrenClick(Sender: TObject);
begin
  for var Control in Layout.Controls do
    if Control <> cbAbsoluteEnabled then
      Control.Enabled := true;
end;

procedure TClickableFrame.DblClick; //unfortunately there's no MouseDblClick that would also give us ShiftState (say double-clicking with Alt pressed etc.)
begin
  inherited;

  ShowMessageFmt('Double-clicked Frame %s', [Name]);
end;

procedure TClickableFrame.MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;

  //Delphi 11 bug: Shift state for MouseClick is always empty

  //if not (ssDouble in Shift) then //this doesn't help, Click always seems to be called before it can detect double-click
  if not cbFixDblClick.IsChecked then //workaround
    ShowMessageFmt('Clicked Frame %s at (%f, %f), Shift state=%s', [Name, X, Y, ShiftToStr(Shift)]); //Delphi 11 bug: need to comment this for DblClick to work
end;

end.
