program ColorPicker;

uses
  System.StartUpCopy,
  FMX.Forms,
  uColorPicker in 'uColorPicker.pas' {Form4};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
