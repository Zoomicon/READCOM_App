unit READCOM.Resources.Themes;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls;

type

  TThemes = class(TDataModule)
    LightTheme: TStyleBook;
    DarkTheme: TStyleBook;
  end;

var
  Themes: TThemes;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

end.
