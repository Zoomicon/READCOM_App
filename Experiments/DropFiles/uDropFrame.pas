unit uDropFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ExtCtrls;

type
  TDropFrame = class(TFrame)
    DropTarget1: TDropTarget;
    procedure DropTarget1DragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
    procedure DropTarget1Dropped(Sender: TObject; const Data: TDragObject; const Point: TPointF);
  protected
    procedure DoDropTarget1DragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation); virtual;
    procedure DoDropTarget1Dropped(Sender: TObject; const Data: TDragObject; const Point: TPointF); virtual;
  end;

implementation

{$R *.fmx}

procedure TDropFrame.DropTarget1DragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  DoDropTarget1DragOver(Sender, Data, Point, Operation);
end;

procedure TDropFrame.DropTarget1Dropped(Sender: TObject; const Data: TDragObject; const Point: TPointF);
begin
  DoDropTarget1Dropped(Sender, Data, Point);
end;

//---------------------------------

procedure TDropFrame.DoDropTarget1DragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  Operation := TDragOperation.Copy;
end;

procedure TDropFrame.DoDropTarget1Dropped(Sender: TObject; const Data: TDragObject;  const Point: TPointF);
begin
  if SizeOf(Data.Files) <> 0 then
    ShowMessage(Format('Received %s at %s', [Data.Files[0], Name]));
end;


end.
