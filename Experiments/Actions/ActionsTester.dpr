program ActionsTester;

uses
  System.StartUpCopy,
  FMX.Forms,
  ActionsTest in 'ActionsTest.pas' {Form2},
  ActionsTestFrame in 'ActionsTestFrame.pas' {Frame1: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
