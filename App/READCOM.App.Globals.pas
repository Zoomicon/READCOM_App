unit READCOM.App.Globals;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls;

type
  TDataModule1 = class(TDataModule)
    DefaultStyleBook: TStyleBook;
    Lang: TLang;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DataModule1: TDataModule1;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

end.
