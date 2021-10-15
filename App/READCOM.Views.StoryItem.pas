unit READCOM.Views.StoryItem;

interface

uses
  READCOM.App.Models, //for IStoryItem
  READCOM.Messages.Models, //for IMessageEditModeChange
  Zoomicon.Manipulator,
  iPub.Rtl.Messaging, //for SubscribeAttrible, GMessaging
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.SVGIconImage, FMX.ExtCtrls, FMX.Controls.Presentation;

const
  DEFAULT_AUTOSIZE = true;

type
  TStoryItem = class(TFrame, IStoryItem, IStoreable)
    Manipulator: TManipulator;
    DropTarget: TDropTarget;
    procedure DropTargetDropped(Sender: TObject; const Data: TDragObject; const Point: TPointF);
    procedure DropTargetDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
    procedure FrameDblClick(Sender: TObject);

  protected
    FAutoSize: Boolean;
    procedure InitDropTarget;
    procedure DoEditModeChange(const Value: Boolean);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    //--- Methods ---
    procedure PlayRandomAudioStoryItem;

    { IStoreable }
    function GetLoadFilesFilter: String; virtual;
    procedure Load(const Stream: TStream); overload; virtual;
    procedure Load(const Filepath: string); overload; virtual;
    procedure Load(const Filepaths: array of string); overload; virtual;
    procedure Save(const Stream: TStream); overload; virtual;
    procedure Save(const Filepath: string); overload; virtual;
    procedure Save(const Directory: string; const FilenamePrefix: string); overload; virtual;

    { Id }
    function GetId: TGUID;
    procedure SetId(const Value: TGUID);

    { ParentStoryItem }
    function GetParentStoryItem: IStoryItem;
    procedure SetParentStoryItem(const Value: IStoryItem);

    { StoryItems }
    function GetStoryItems: TStoryItemCollection;
    procedure SetStoryItems(const Value: TStoryItemCollection);

    { AudioStoryItems }
    function GetAudioStoryItems: TAudioStoryItemCollection;

    { Hidden }
    function IsHidden: Boolean;
    procedure SetHidden(const Value: Boolean);

    { Target }
    function GetTarget: IStoryItem;
    procedure SetTarget(const Value: IStoryItem);

    { TargetId }
    function GetTargetId: TGUID;
    procedure SetTargetId(const Value: TGUID);

    //--- Messages ---
    [Subscribe(TipMessagingThread.Main)]
    procedure HandleParentNavigatedToChanged;

    {$region 'Messages'}
    procedure OnEditModeChange(const AMessage: IMessageEditModeChange);
    {$endregion}

  published
    //--- Properties ---
    property Id: TGUID read GetId write SetId;
    property ParentStoryItem: IStoryItem read GetParentStoryItem write SetParentStoryItem; //default nil //stored false //TODO: see if Delphi persistence can do loops
    property StoryItems: TStoryItemCollection read GetStoryItems write SetStoryItems; //default nil
    property AudioStoryItems: TAudioStoryItemCollection read GetAudioStoryItems; //default nil //stored false
    property Hidden: Boolean read IsHidden write SetHidden; //default false
    property Target: IStoryItem read GetTarget write SetTarget; //default nil //stored false
    property TargetId: TGUID read GetTargetId write SetTargetId; //default ''

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

procedure TStoryItem.PlayRandomAudioStoryItem;
begin
  var RandomAudioStoryItem := AudioStoryItems.PickRandom;
  if RandomAudioStoryItem <> nil then
    RandomAudioStoryItem.Play;
end;

{$REGION '--- PROPERTIES ---'}

{$region 'Hidden'}

function TStoryItem.IsHidden: Boolean;
begin

end;

procedure TStoryItem.SetHidden(const Value: Boolean);
begin

end;

{$endregion}

{$region 'Id'}

function TStoryItem.GetId: TGUID;
begin

end;

procedure TStoryItem.SetId(const Value: TGUID);
begin

end;

{$endregion}

{$region 'ParentStoryItem'}

function TStoryItem.GetParentStoryItem: IStoryItem;
begin

end;

procedure TStoryItem.SetParentStoryItem(const Value: IStoryItem);
begin

end;

{$endregion}

{$region 'StoryItems'}

function TStoryItem.GetStoryItems: TStoryItemCollection;
begin

end;

procedure TStoryItem.SetStoryItems(const Value: TStoryItemCollection);
begin

end;

{$endregion}

{$region 'AudioStoryItems'}

function TStoryItem.GetAudioStoryItems: TAudioStoryItemCollection;
begin

end;

{$endregion}

{$region 'Target'}

function TStoryItem.GetTarget: IStoryItem;
begin

end;

procedure TStoryItem.SetTarget(const Value: IStoryItem);
begin

end;

{$endregion}

{$region 'TargetId'}

function TStoryItem.GetTargetId: TGUID;
begin

end;

procedure TStoryItem.SetTargetId(const Value: TGUID);
begin

end;

{$endregion}

{$ENDREGION}

{$REGION '--- EVENTS ---'}

procedure TStoryItem.FrameDblClick(Sender: TObject);
begin
 ShowMessage('Double click StoryItem');
end;

procedure TStoryItem.HandleParentNavigatedToChanged;
begin

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

{$ENDREGION}

{$region 'IStoreable'}

{ Load }

function TStoryItem.GetLoadFilesFilter: String;
begin
  result := '';
end;

procedure TStoryItem.Load(const Stream: TStream);
begin
  //TODO
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

{ Save }

procedure TStoryItem.Save(const Stream: TStream);
begin
  //TODO
end;

procedure TStoryItem.Save(const Filepath: string);
begin
  var OutputFileStream := TFileStream.Create(Filepath,  fmCreate); //or fmShareDenyNone //TODO: may be needed for Android
  try
    Save(OutputFileStream);
  finally
    FreeAndNil(OutputFileStream);
  end;
end;

procedure TStoryItem.Save(const Directory: string; const FilenamePrefix: string);
begin
  //TODO
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

end.
