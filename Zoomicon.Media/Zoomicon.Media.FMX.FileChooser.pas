unit Zoomicon.Media.FMX.FileChooser;

//Note: since this uses TLocalFilesystemBindSourceAdapter, make sure you've installed the package first before opening the frame in the designer (and repeat the package installation if code is changed)

interface

uses
  Zoomicon.Media.FMX.DataBinding, //for TLocalFilesBindSourceAdapter
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView,
  System.Rtti, System.Bindings.Outputs,
  Fmx.Bind.Editors, Data.Bind.EngExt, Fmx.Bind.DBEngExt,
  Data.Bind.Components, Data.Bind.ObjectScope, FMX.Controls.Presentation,
  FMX.Edit, FMX.ComboEdit;

type
  TFileChooser = class(TFrame)
    listFolderContents: TListView;
    cbFolder: TComboEdit;
    AdapterFolders: TLocalFilesystemBindSourceAdapter;
    AdapterFiles: TLocalFilesystemBindSourceAdapter;
    AdapterBindSource1: TAdapterBindSource;
    BindingsList1: TBindingsList;
    AdapterBindSource2: TAdapterBindSource;
    LinkFillControlToField: TLinkFillControlToField;
    LinkFillControlToField1: TLinkFillControlToField;
    procedure cbFolderChange(Sender: TObject);

  protected
    {AllowFolderInput}
    function GetAllowFolderInput: Boolean;
    procedure SetAllowFolderInput(const Value: Boolean);
    {FolderPath}
    function GetFolderPath: String;
    procedure SetFolderPath(const Value: String);
    procedure DoFolderPathChange(const Value: String);

  public
    const
      DefaultAllowFolderInput = true;
      DefaultFolderPath = ''; //TODO: should maybe use something else like user's folder (or fallback to it if we can't set this at runtime - else can have enum based property on a known or custom folder to show)

    constructor Create(AOwner: TComponent); override;

  published
    property AllowFolderInput: Boolean read GetAllowFolderInput write SetAllowFolderInput default DefaultAllowFolderInput;
    property FolderPath: String read GetFolderPath write SetFolderPath; //default DefaultFolderPath; //Note: set in constructor, declared defaults not allowed for String types (always stored)
  end;

procedure Register;

implementation

{$R *.fmx}

{$REGION 'TFileChooser'}

{$region 'Initialization'}

constructor TFileChooser.Create(AOwner: TComponent);
begin
  inherited;

  cbFolder.Stored := false;
  listFolderContents.Stored := false;

  AllowFolderInput := DefaultAllowFolderInput;
  FolderPath := DefaultFolderPath;
end;

{$endregion}

{$region 'AllowFolderInput'}

function TFileChooser.GetAllowFolderInput: Boolean;
begin
  result := cbFolder.Model.InputSupport;
end;

procedure TFileChooser.SetAllowFolderInput(const Value: Boolean);
begin
  cbFolder.Model.InputSupport := Value;
end;

{$endregion}

{$region 'FolderPath'}

function TFileChooser.GetFolderPath: String;
begin
  result := AdapterFolders.FolderPath;
end;

procedure TFileChooser.SetFolderPath(const Value: String);
begin
  if (GetFolderPath <> Value) then
    DoFolderPathChange(Value);
end;

procedure TFileChooser.DoFolderPathChange(const Value: String);
begin
  AdapterFolders.FolderPath := Value;
  AdapterFiles.FolderPath := Value;

  with cbFolder do
  begin
    var TextChanged := (Text <> Value); //must set this before setting ItemIndex
    ItemIndex := Items.IndexOf(Text); //Note: this returns -1 for custom path (not from the dropdown list) //this seems to be required, setting Text directly without changing this causes errors
    if TextChanged or (ItemIndex = -1) then //must check this to avoid Stack Overflow errors
      Text := Value;
  end; //TODO: there still are some stack overflow errors in some special cases (seem to occur at specific folders, e.g. trying to gradually navigate the path: "C:\adobeTemp\ETR21F0.tmp\3\AdobeCommon_x64")
end;

{$endregion}

{$region 'cbFolder.OnChange'}

procedure TFileChooser.cbFolderChange(Sender: TObject); //Note: even if we were just setting AdapterFiles.Path via LiveBindings this would be needed, else would only set the value if user press ENTER on the ComboEdit, but not when they pick an item from the dropdown
begin
  with cbFolder do
    SetFolderPath(Model.Text); //don't use Items[SelectedIndex] since user may have typed some other (deeper or more shallow) path
end;

{$endregion}

{$ENDREGION}

procedure RegisterSerializationClasses;
begin
  RegisterFmxClasses([TFileChooser]);
end;

procedure Register;
begin
  GroupDescendentsWith(TFileChooser, TComponent);
  RegisterSerializationClasses;
  RegisterComponents('Zoomicon', [TFileChooser]);
end;

initialization
  RegisterSerializationClasses; //don't call Register here, it's called by the IDE automatically on a package installation (fails at runtime)

end.
