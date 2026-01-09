program GlowBorderTest;

uses
  System.StartUpCopy,
  FMX.Forms,
  uGlowBorderTest in 'uGlowBorderTest.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
