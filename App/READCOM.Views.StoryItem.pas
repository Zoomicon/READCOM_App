unit READCOM.Views.StoryItem;

interface

uses
  READCOM.Messages.Models, //for IMessageEditModeChange
  Zoomicon.Manipulator,
  iPub.Rtl.Messaging, //for SubscribeAttrible, GMessaging
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.SVGIconImage, FMX.ExtCtrls;

type
  TStoryItem = class(TFrame) //abstract
    DropTarget: TDropTarget;
    Manipulator: TManipulator;
    procedure DropTargetDropped(Sender: TObject; const Data: TDragObject; const Point: TPointF);

  protected
    function GetDropFilter: String; virtual;
    procedure DoEditModeChange(const Value: Boolean);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFile(const Filepath: String); virtual; abstract;
    procedure LoadFiles(const Files: array of String); virtual;

    [Subscribe(TipMessagingThread.Main)]
    procedure OnEditModeChange(const AMessage: IMessageEditModeChange);
  end;

implementation

{$R *.fmx}

constructor TStoryItem.Create(AOwner: TComponent);
begin
  inherited;
  //DropTarget1.FilterIndex := 1; //this is the default value
  DropTarget.Filter := GetDropFilter;
  GMessaging.Subscribe(Self);
end;

destructor TStoryItem.Destroy;
begin
  GMessaging.Unsubscribe(Self);
  inherited;
end;

function TStoryItem.GetDropFilter: String;
begin
  result := '';
end;

procedure TStoryItem.LoadFiles(const Files: array of String);
begin
  for var f in Files do
    LoadFile(f);
end;

procedure TStoryItem.OnEditModeChange(const AMessage: IMessageEditModeChange);
begin
  DoEditModeChange(AMessage.Value);
end;

procedure TStoryItem.DoEditModeChange(const Value: Boolean);
begin
  Manipulator.EditMode := Value;
  TabStop := Value;
end;

procedure TStoryItem.DropTargetDropped(Sender: TObject; const Data: TDragObject; const Point: TPointF);
begin
  LoadFiles(Data.Files);
end;

end.
