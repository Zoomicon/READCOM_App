unit Zoomicon.Generics.Collections;

interface
  uses
    SysUtils, //for TPredicate, TProc
    System.Generics.Collections, //for TList
    System.Rtti; //for RttiContext

type

  TListEx<T> = class(TList<T>)
    { GetRandom }
    class function GetRandom(const List: TList<T>): T; overload;
    function GetRandom: T; overload; virtual;

    { GetAll }
    class function GetAll(const Enum: TEnumerable<T>; const Predicate: TPredicate<T> = nil): TListEx<T>; overload;
    function GetAll(const Predicate: TPredicate<T> = nil): TListEx<T>; overload;

    {ForEach}
    class procedure ForEach(const Enum: TEnumerable<T>; const Proc: TProc<T>; const Predicate: TPredicate<T> = nil); overload;
    procedure ForEach(const Proc: TProc<T>; const Predicate: TPredicate<T> = nil); overload;
  end;

  //----------------------------------------------------------------------------

  TInterfaceListEx<T: IInterface> = class(TListEx<T>)
    { GetAllOfInterface }
    class function GetAllOfInterface<AInterface: IInterface>(const Enum: TEnumerable<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): TInterfaceListEx<AInterface>; overload;
    function GetAllOfInterface<AInterface: IInterface>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): TInterfaceListEx<AInterface>; overload;
  end;

  //----------------------------------------------------------------------------

  TObjectListEx<T: class> = class(TListEx<T>)
    { GetAllOfInterface }
    class function GetAllOfInterface<AInterface: IInterface>(const Enum: TEnumerable<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): TInterfaceListEx<AInterface>; overload;
    function GetAllOfInterface<AInterface: IInterface>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): TInterfaceListEx<AInterface>; overload;

    { GetAllOfClass }
    class function GetAllOfClass<AClass: class>(const Enum: TEnumerable<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil): TObjectListEx<AClass>; overload;
    function GetAllOfClass<AClass: class>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil): TListEx<AClass>; overload;

    {ForEach}
    class procedure ForEachOfClass<AClass: class>(const Enum: TEnumerable<T>; const Proc: TProc<AClass>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil); overload;
    procedure ForEachOfClass<AClass: class>(const Proc: TProc<AClass>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil); overload;
  end;

implementation

{$REGION 'TListEx' ------------------------------------------------------------}

{$region 'GetRandom'}

class function TListEx<T>.GetRandom(const List: TList<T>): T;
begin
  with List do
    begin
    var c := Count;
    if c = 0 then
      result := Default(T) //returns Default value for type T
    else
      result := Items[Random(c)]; //TList uses 0-based index, and 0 <= Random(c) < c
    end;
end;

function TListEx<T>.GetRandom: T;
begin
  result := {TListEx<T>.}GetRandom(self);
end;

{$endregion}

{$region 'GetAll'}

class function TListEx<T>.GetAll(const Enum: TEnumerable<T>; const Predicate: TPredicate<T> = nil): TListEx<T>;
begin
  var ResultList := TListEx<T>.Create;
  for var item in Enum do
    if (not Assigned(Predicate)) or Predicate(item) then
      ResultList.Add(item);
  result := ResultList;
end;

function TListEx<T>.GetAll(const Predicate: TPredicate<T> = nil): TListEx<T>;
begin
  result := {TListEx<T>.}GetAll(self);
end;

{$endregion}

{$region 'ForEach'}

class procedure TListEx<T>.ForEach(const Enum: TEnumerable<T>; const Proc: TProc<T>; const Predicate: TPredicate<T> = nil);
begin
  if Assigned(Proc) then
    for var item in Enum do
      if (not Assigned(Predicate)) or Predicate(item) then
        Proc(item);
end;

procedure TListEx<T>.ForEach(const Proc: TProc<T>; const Predicate: TPredicate<T> = nil);
begin
  {TListEx<T>.}ForEach(self, Proc, Predicate);
end;

{$endregion}

{$ENDREGION ...................................................................}

{$REGION 'TInterfaceListEx' ------------------------------------------------------}

{$region 'GetAllOfInterface'}

class function TInterfaceListEx<T>.GetAllOfInterface<AInterface>(const Enum: TEnumerable<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): TInterfaceListEx<AInterface>;
var
  itemAsAInterface: AInterface;
begin
  var guid := TRttiInterfaceType(TRttiContext.Create.GetType(TypeInfo(AInterface))).GUID;
  var ListOfAInterface := TInterfaceListEx<AInterface>.Create;
  for var item in Enum do
    if Supports(item, guid, itemAsAInterface) and ((not Assigned(OuterPredicate)) or OuterPredicate(item))  then
      if (not Assigned(InnerPredicate)) or InnerPredicate(itemAsAInterface) then
        ListOfAInterface.Add(itemAsAInterface);
  result := ListOfAInterface;
end;

function TInterfaceListEx<T>.GetAllOfInterface<AInterface>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): TInterfaceListEx<AInterface>;
begin
  result := {TInterfaceListEx<T>.}GetAllOfInterface<AInterface>(Self, OuterPredicate, InnerPredicate);
end;

{$endregion}

{$ENDREGION ...................................................................}

{$REGION 'TObjectListEx' ------------------------------------------------------}

{$region 'GetAllOfInterface'}

class function TObjectListEx<T>.GetAllOfInterface<AInterface>(const Enum: TEnumerable<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): TInterfaceListEx<AInterface>;
var
  itemAsAInterface: AInterface;
begin
  var guid := TRttiInterfaceType(TRttiContext.Create.GetType(TypeInfo(AInterface))).GUID;
  var ListOfAInterface := TInterfaceListEx<AInterface>.Create;
  for var item in Enum do
    if Supports(item, guid, itemAsAInterface) and ((not Assigned(OuterPredicate)) or OuterPredicate(item)) then
      if (not Assigned(InnerPredicate)) or InnerPredicate(itemAsAInterface) then
        ListOfAInterface.Add(itemAsAInterface);
  result := ListOfAInterface;
end;

function TObjectListEx<T>.GetAllOfInterface<AInterface>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): TInterfaceListEx<AInterface>;
begin
  result := {TObjectListEx<T>.}GetAllOfInterface<AInterface>(Self, OuterPredicate, InnerPredicate);
end;

{$endregion}

{$region 'GetAllOfClass'}

class function TObjectListEx<T>.GetAllOfClass<AClass>(const Enum: TEnumerable<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil): TObjectListEx<AClass>;
begin
  var ListOfAClass := TObjectListEx<AClass>.Create;
  for var item in Enum do
    if (item is AClass) and ((not Assigned(OuterPredicate)) or OuterPredicate(item)) then
      begin
      var itemAsAClass := item as AClass;
      if (not Assigned(InnerPredicate)) or InnerPredicate(ItemAsAClass) then
        ListOfAClass.Add(itemAsAClass);
      end;
  result := ListOfAClass;
end;

function TObjectListEx<T>.GetAllOfClass<AClass>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil): TListEx<AClass>;
begin
  result := {TObjectListEx<T>.}GetAllOfClass<AClass>(Self, OuterPredicate, InnerPredicate);
end;

{$endregion}

{$region 'ForEach'}

class procedure TObjectListEx<T>.ForEachOfClass<AClass>(const Enum: TEnumerable<T>; const Proc: TProc<AClass>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil);
begin
  if Assigned(Proc) then
    for var item in Enum do
      if (item is AClass) and ((not Assigned(OuterPredicate)) or OuterPredicate(item)) then
        begin
        var itemAsAClass := item as AClass;
        if (not Assigned(InnerPredicate)) or InnerPredicate(ItemAsAClass) then
          Proc(itemAsAClass);
        end;
end;

procedure TObjectListEx<T>.ForEachOfClass<AClass>(const Proc: TProc<AClass>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil);
begin
  {TObjectListEx<T>.}ForEachOfClass<AClass>(Self, Proc, OuterPredicate, InnerPredicate);
end;

{$endregion}

{$ENDREGION ...................................................................}

end.
