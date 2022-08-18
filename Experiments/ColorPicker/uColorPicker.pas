unit uColorPicker;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, System.Rtti,
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.Components,
  FMX.Colors, FMX.Layouts, FMX.ListBox;

type
  TForm4 = class(TForm)
    ComboColorBox1: TComboColorBox;
    ColorComboBox1: TColorComboBox;
    ColorListBox1: TColorListBox;
    ColorPanel1: TColorPanel;
    ColorBox1: TColorBox;
    ColorPicker1: TColorPicker;
    ColorQuad1: TColorQuad;
    BindingsList1: TBindingsList;
    LinkControlToPropertyColor: TLinkControlToProperty;
    LinkControlToPropertyColor2: TLinkControlToProperty;
    LinkControlToPropertyColor3: TLinkControlToProperty;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

end.
