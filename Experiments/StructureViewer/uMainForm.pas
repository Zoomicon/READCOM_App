unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.MultiView, SubjectStand,
  FrameStand, uHidableFrame, FMX.Layouts;

type
  TMainForm = class(TForm)
    MultiView: TMultiView;
    HidableFrame1: THidableFrame;
    btnMenu: TSpeedButton;
    ContentLayout: TLayout;
    btnShowChildren: TButton;
    MainLayout: TLayout;
    HidableFrame2: THidableFrame;
    HidableFrame3: THidableFrame;
    MultiViewFrameStand: TFrameStand;
    procedure btnShowChildrenClick(Sender: TObject);
    procedure MultiViewStartShowing(Sender: TObject);
  protected
    procedure StructureViewSelection(Sender: TObject; const Selection: TObject);
  end;

var
  MainForm: TMainForm;

implementation
  uses Zoomicon.Introspection.FMX.StructureView; //for TStructureView

{$R *.fmx}

procedure TMainForm.btnShowChildrenClick(Sender: TObject);
begin
  for var Control in ContentLayout.Controls do
    Control.Visible := true;
end;

procedure TMainForm.StructureViewSelection(Sender: TObject; const Selection: TObject);
begin
  ShowMessage(TControl(Selection).ClassName);
  MultiView.HideMaster;
end;

procedure TMainForm.MultiViewStartShowing(Sender: TObject);
begin
  with MultiViewFrameStand do
  begin
    CloseAllExcept(TStructureView);
    var info:= MultiViewFrameStand.GetFrameInfo<TStructureView>;
    with info.Frame do
    begin
      GUIRoot := ContentLayout;
      OnSelection := StructureViewSelection;
    end;
    info.Show;
  end;
end;

end.
