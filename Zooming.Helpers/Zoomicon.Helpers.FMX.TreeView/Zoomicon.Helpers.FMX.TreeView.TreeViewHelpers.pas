unit Zoomicon.Helpers.FMX.TreeView.TreeViewHelpers;

interface
  uses
    FMX.TreeView,
    FMX.Types,
    System.Types;

  type

    TTreeViewSearchHelper = class helper for TTreeView
    public
      function FindObject(const Value: TObject): TTreeViewItem;
    end;

    TTreeViewItemSearchHelper = class helper for TTreeViewItem
    public
      function FindObject(const Value: TObject): TTreeViewItem;
    end;

implementation

function TTreeViewSearchHelper.FindObject(const Value: TObject): TTreeViewItem;
begin
  for var i := 0 to (Count - 1) do //Items is a 0-indexed array
  begin
    var foundItem := Items[i].FindObject(Value); //note: that FindObject does depth-first search
    if Assigned(foundItem) then
      exit(foundItem); //found
  end;

  result := nil; //not found
end;

function TTreeViewItemSearchHelper.FindObject(const Value: TObject): TTreeViewItem;
begin
  if (TagObject = Value) then
    exit(Self)
  else
    for var i := 0 to (Count - 1) do //Items is a 0-indexed array
    begin
      var foundItem := Items[i].FindObject(Value); //does recursion (depth-first)
      if Assigned(foundItem) then //if not nil then has been found
        exit(foundItem); //unwind stack and return the first one found
    end;

  result := nil; //not found
end;

end.
