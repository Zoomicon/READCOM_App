unit uInheritedFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ExtCtrls, uDropFrame;

type
  TInheritedFrame = class(TDropFrame)
  protected
    procedure DoDropTarget1DragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation); override;
    procedure DoDropTarget1Dropped(Sender: TObject; const Data: TDragObject; const Point: TPointF); override;
  end;

var
  InheritedFrame: TInheritedFrame;

implementation

{$R *.fmx}

{TInheritedFrame}

procedure TInheritedFrame.DoDropTarget1DragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  inherited;
end;

procedure TInheritedFrame.DoDropTarget1Dropped(Sender: TObject; const Data: TDragObject; const Point: TPointF);
begin
  inherited;
  ShowMessage('Inherited Frame!');
end;

end.
