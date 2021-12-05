unit Zoomicon.Helpers.RTL.ClassListHelpers;

interface
  uses System.Contnrs; //for TClassList

  type

    TClassArray = array of TClass;

    TClassListCreateFromClassArrayHelper = class helper for TClassList
      constructor Create(const Items: TClassArray); overload; virtual;
    end;

    TClassListFindClassOfHelper = class helper(TClassListCreateFromClassArrayHelper) for TClassList //using class helper inheritance, else only last helper defined for same Class is seen by compiler
      function FindClassOf(const AObject: TObject; const AExact: Boolean = True; const AStartAt: Integer = 0): Integer;
    end;

implementation

{$REGION 'TClassListFindClassOfHelper'}

constructor TClassListCreateFromClassArrayHelper.Create(const Items: TClassArray);
begin
  inherited Create;
  for var AClass in Items do
    Add(AClass);
end;

function TClassListFindClassOfHelper.FindClassOf(const AObject: TObject; const AExact: Boolean = True; const AStartAt: Integer = 0): Integer;
begin
  for var i := AStartAt to Count - 1 do
    if (AExact and (Items[i] = AObject.ClassType)) or
       (not AExact and AObject.InheritsFrom(Items[i])) then
    begin
      Result := i;
      exit;
    end;
  Result := -1;
end;

{$ENDREGION}

end.
