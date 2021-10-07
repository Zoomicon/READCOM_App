unit READCOM.Views.StoryItem;

interface

uses
  READCOM.App.Models, //for ILoadable
  READCOM.Messages.Models, //for IMessageEditModeChange
  Zoomicon.Manipulator,
  iPub.Rtl.Messaging, //for SubscribeAttrible, GMessaging
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.SVGIconImage, FMX.ExtCtrls, FMX.Controls.Presentation;

const
  DEFAULT_AUTOSIZE = true;

type
  TStoryItem = class(TFrame, ILoadable)
    Manipulator: TManipulator;
    DropTarget: TDropTarget;
    Button1: TButton;
    procedure DropTargetDropped(Sender: TObject; const Data: TDragObject; const Point: TPointF);
    procedure DropTargetDragOver(Sender: TObject; const Data: TDragObject;
      const Point: TPointF; var Operation: TDragOperation);
    procedure FrameDblClick(Sender: TObject);

  protected
    FAutoSize: Boolean;
    procedure InitDropTarget;
    procedure DoEditModeChange(const Value: Boolean);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    {$region 'ILoadable'}
    function GetLoadFilesFilter: String; virtual;
    procedure Load(const Stream: TStream); overload; virtual; abstract;
    procedure Load(const Filepath: string); overload; virtual;
    procedure Load(const Filepaths: array of string); overload; virtual;
    {$endregion}

    {$region 'Messages'}
    [Subscribe(TipMessagingThread.Main)]
    procedure OnEditModeChange(const AMessage: IMessageEditModeChange);
    {$endregion}

  published
    property AutoSize: Boolean read FAutoSize write FAutoSize default DEFAULT_AUTOSIZE;
  end;

implementation

{$R *.fmx}

constructor TStoryItem.Create(AOwner: TComponent);
begin
  inherited;
  FAutoSize := DEFAULT_AUTOSIZE;
  InitDropTarget;
  GMessaging.Subscribe(Self);
end;

destructor TStoryItem.Destroy;
begin
  GMessaging.Unsubscribe(Self);
  inherited;
end;

{$region 'Edit mode'}

procedure TStoryItem.OnEditModeChange(const AMessage: IMessageEditModeChange);
begin
  DoEditModeChange(AMessage.Value);
end;

procedure TStoryItem.DoEditModeChange(const Value: Boolean);
begin
  Manipulator.EditMode := Value;
  TabStop := Value;
end;

{$endregion}

{$region 'ILoadable'}

function TStoryItem.GetLoadFilesFilter: String;
begin
  result := '';
end;

procedure TStoryItem.Load(const Filepath: String);
begin
  var InputFileStream := TFileStream.Create(Filepath,  fmOpenRead);
  try
    Load(InputFileStream);
  finally
    FreeAndNil(InputFileStream);
  end;
end;

procedure TStoryItem.Load(const Filepaths: array of String);
begin
  for var f in Filepaths do
    Load(f);
end;

{$endregion}

{$region 'Drop target'}

procedure TStoryItem.InitDropTarget;
begin
  DropTarget.FilterIndex := 1; //this is the default value
  DropTarget.Filter := GetLoadFilesFilter;
end;

procedure TStoryItem.DropTargetDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  Operation := TDragOperation.Copy;
end;

procedure TStoryItem.DropTargetDropped(Sender: TObject; const Data: TDragObject; const Point: TPointF);
begin
  Load(Data.Files);
end;

{$endregion}

procedure TStoryItem.FrameDblClick(Sender: TObject);
begin
 ShowMessage('Double click StoryItem');
end;

end.
