//Description: Generic Collections
//Author: George Birbilis (http://zoomicon.com)

unit Zoomicon.Generics.Collections;

interface
  uses
    SysUtils, //for TPredicate, TProc
    System.Generics.Collections; //for TList

type

  {$region 'TListEx' ---------------------------------------------------------}

  (* //TODO: TList doesn't descend from some TInterfacedObject so we'd need to implement our own QueryInterface and ref counting to use interfaces. Could maybe else do at some descendent TInterfacedListEx that would implement IListEx and have extra methods to return such
  IListEx<T> = interface
    ['{F6DB7965-D05E-4303-8415-12A0E85097DB}']
    function GetRandom: T; overload;
    function GetAll(const Predicate: TPredicate<T> = nil): IListEx<T>; overload;
    function GetCount(const Predicate: TPredicate<T> = nil): Integer; overload;
    function GetFirst(const Predicate: TPredicate<T> = nil): T; overload;
    function GetLast(const Predicate: TPredicate<T> = nil): T; overload;
    procedure ForEach(const Proc: TProc<T>; const Predicate: TPredicate<T> = nil); overload;
    function All(const Predicate: TPredicate<T>): Boolean; overload;
    procedure Shuffle; overload;
    procedure AddOnce(const Item: T); overload;
  end;
  *)

  TListEx<T> = class(TList<T>{, IListEx<T>})
    {GetRandom}
    class function GetRandom(const List: TList<T>): T; overload;
    function GetRandom: T; overload;

    {GetAll}
    class function GetAll(const Enum: TEnumerable<T>; const Predicate: TPredicate<T> = nil): {IListEx}TListEx<T>; overload;
    function GetAll(const Predicate: TPredicate<T> = nil): TListEx<T>; overload;

    {GetCount}
    class function GetCount(const Enum: TEnumerable<T>; const Predicate: TPredicate<T> = nil): Integer; overload;
    function GetCount(const Predicate: TPredicate<T> = nil): Integer; overload;

    {GetFirst}
    class function GetFirst(const Enum: TEnumerable<T>; const Predicate: TPredicate<T> = nil): T; overload;
    function GetFirst(const Predicate: TPredicate<T> = nil): T; overload;

    {GetLast}
    class function GetLast(const List: TList<T>; const Predicate: TPredicate<T> = nil): T; overload;
    function GetLast(const Predicate: TPredicate<T> = nil): T; overload;

    {ForEach}
    class procedure ForEach(const Enum: TEnumerable<T>; const Proc: TProc<T>; const Predicate: TPredicate<T> = nil); overload;
    procedure ForEach(const Proc: TProc<T>; const Predicate: TPredicate<T> = nil); overload;

    {All}
    class function All(const Enum: TEnumerable<T>; const Predicate: TPredicate<T>): Boolean; overload;
    function All(const Predicate: TPredicate<T>): Boolean; overload;

    {Shuffle}
    class procedure Shuffle(const List: TList<T>); overload; //based on: http://www.bytechaser.com/en/functions/p6sv9tve9v/randomly-shuffle-contents-of-any-list-in-c-sharp.aspx
    procedure Shuffle; overload;

    {AddOnce}
    class procedure AddOnce(const List: TList<T>; const Item: T); overload;
    procedure AddOnce(const Item: T); overload;
  end;

  {$endregion ................................................................}

  {$region 'TIntefaceListEx' -------------------------------------------------}

  TInterfaceListEx<T: IInterface> = class(TListEx<T>)
    {GetAllInterface}
    class function GetAllInterface<AInterface: IInterface>(const Enum: TEnumerable<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): TInterfaceListEx<AInterface>; overload;
    function GetAllInterface<AInterface: IInterface>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): TInterfaceListEx<AInterface>; overload;

    {GetInterfaceCount}
    class function GetInterfaceCount<AInterface: IInterface>(const Enum: TEnumerable<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): Integer; overload;
    function GetInterfaceCount<AInterface: IInterface>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): Integer; overload;

    {GetFirstInterface}
    class function GetFirstInterface<AInterface: IInterface>(const Enum: TEnumerable<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): AInterface; overload;
    function GetFirstInterface<AInterface: IInterface>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): AInterface; overload;

    {GetLastInterface}
    class function GetLastInterface<AInterface: IInterface>(const List: TList<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): AInterface; overload;
    function GetLastInterface<AInterface: IInterface>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): AInterface; overload;

    {ForEachInterface}
    class procedure ForEachInterface<AInterface: IInterface>(const Enum: TEnumerable<T>; const Proc: TProc<AInterface>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil); overload;
    procedure ForEachInterface<AInterface: IInterface>(const Proc: TProc<AInterface>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil); overload;

    {AllInterfaces}
    class function AllInterfaces<AInterface: IInterface>(const Enum: TEnumerable<T>; const Predicate: TPredicate<AInterface>): Boolean; overload;
    function AllInterfaces<AInterface: IInterface>(const Predicate: TPredicate<AInterface>): Boolean; overload;
  end;

  {$endregion ................................................................}

  {$region 'TObjectListEx' ---------------------------------------------------}

  //Note: unfortunately we can't define a common ancestor for TInterfaceListEx and TObjectListEx to move all interface methods there. The reason is there's no constaint to say "either class or TInterface" (and Supports just has overloaded versions that take either a class or an interface)

  TObjectListEx<T: class> = class(TListEx<T>)
    {GetAllInterface}
    class function GetAllInterface<AInterface: IInterface>(const Enum: TEnumerable<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): TInterfaceListEx<AInterface>; overload;
    function GetAllInterface<AInterface: IInterface>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): TInterfaceListEx<AInterface>; overload;

    {GetInterfaceCount}
    class function GetInterfaceCount<AInterface: IInterface>(const Enum: TEnumerable<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): Integer; overload;
    function GetInterfaceCount<AInterface: IInterface>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): Integer; overload;

    {GetFirstInterface}
    class function GetFirstInterface<AInterface: IInterface>(const Enum: TEnumerable<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): AInterface; overload;
    function GetFirstInterface<AInterface: IInterface>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): AInterface; overload;

    {GetLastInterface}
    class function GetLastInterface<AInterface: IInterface>(const List: TList<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): AInterface; overload;
    function GetLastInterface<AInterface: IInterface>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): AInterface; overload;

    {ForEachInterface}
    class procedure ForEachInterface<AInterface: IInterface>(const Enum: TEnumerable<T>; const Proc: TProc<AInterface>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil); overload;
    procedure ForEachInterface<AInterface: IInterface>(const Proc: TProc<AInterface>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil); overload;

    {AllInterfaces}
    class function AllInterfaces<AInterface: IInterface>(const Enum: TEnumerable<T>; const Predicate: TPredicate<AInterface>): Boolean; overload;
    function AllInterfaces<AInterface: IInterface>(const Predicate: TPredicate<AInterface>): Boolean; overload;

    {GetAllClass}
    class function GetAllClass<AClass: class>(const Enum: TEnumerable<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil): TObjectListEx<AClass>; overload;
    function GetAllClass<AClass: class>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil): TListEx<AClass>; overload;

    {GetClassCount}
    class function GetClassCount<AClass: class>(const Enum: TEnumerable<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil): Integer; overload;
    function GetClassCount<AClass: class>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil): Integer; overload;

    {GetFirstClass}
    class function GetFirstClass<AClass: class>(const Enum: TEnumerable<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil): AClass; overload;
    function GetFirstClass<AClass: class>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil): AClass; overload;

    {GetLastClass}
    class function GetLastClass<AClass: class>(const List: TList<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil): AClass; overload;
    function GetLastClass<AClass: class>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil): AClass; overload;

    {ForEachClass}
    class procedure ForEachClass<AClass: class>(const Enum: TEnumerable<T>; const Proc: TProc<AClass>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil); overload;
    procedure ForEachClass<AClass: class>(const Proc: TProc<AClass>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil); overload;

    {AllClasses}
    class function AllClasses<AClass: class>(const Enum: TEnumerable<T>; const Predicate: TPredicate<AClass>): Boolean; overload;
    function AllClasses<AClass: class>(const Predicate: TPredicate<AClass>): Boolean; overload;

    {FreeAll}
    class procedure FreeAll(const List: TList<T>); overload;
    procedure FreeAll; overload;
  end;

  {$endregion ................................................................}

implementation
  uses
    System.Rtti; //for RttiContext

{$REGION 'TListEx' ------------------------------------------------------------}

{$region 'GetRandom'}

class function TListEx<T>.GetRandom(const List: TList<T>): T;
begin
  if Assigned(List) then
    with List do
    begin
      var c := Count;
      if c = 0 then
        result := Default(T) //returns Default value for type T
      else
        result := Items[Random(c)]; //TList uses 0-based index, and 0 <= Random(c) < c
    end
  else
    result := Default(T); //returns Default value for type T
end;

function TListEx<T>.GetRandom: T;
begin
  result := {TListEx<T>.}GetRandom(self);
end;

{$endregion}

{$region 'GetAll'}

class function TListEx<T>.GetAll(const Enum: TEnumerable<T>; const Predicate: TPredicate<T> = nil): {IListEx}TListEx<T>;
begin
  var ResultList := TListEx<T>.Create;
  if Assigned(Enum) then
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

{$region 'GetCount'}

class function TListEx<T>.GetCount(const Enum: TEnumerable<T>; const Predicate: TPredicate<T> = nil): Integer;
begin
  var Count: Integer := 0;
  ForEach(Enum, procedure(item: T) //TODO: if this doesn't get inlined should use a for loop like in GetAll
    begin
      Inc(Count);
    end,
  Predicate);
end;

function TListEx<T>.GetCount(const Predicate: TPredicate<T> = nil): Integer;
begin
  result := {TListEx<T>.}GetCount(self);
end;

{$endregion}

{$region 'GetFirst'}

class function TListEx<T>.GetFirst(const Enum: TEnumerable<T>; const Predicate: TPredicate<T> = nil): T;
begin
  if Assigned(Enum) then
    for var item in Enum do
      if (not Assigned(Predicate)) or Predicate(item) then
      begin
        result := item;
        exit;
      end;
  result := Default(T) //returns Default value for type T
end;

function TListEx<T>.GetFirst(const Predicate: TPredicate<T> = nil): T;
begin
  result := {TListEx<T>.}GetFirst(self);
end;

{$endregion}

{$region 'GetLast'}

class function TListEx<T>.GetLast(const List: TList<T>; const Predicate: TPredicate<T> = nil): T;
begin
  if Assigned(List) then
    for var i := List.Count-1 downto 0 do
    begin
      var item := List[i];
      if (not Assigned(Predicate)) or Predicate(item) then
      begin
        result := item;
        exit;
      end;
    end;
  result := Default(T) //returns Default value for type T
end;

function TListEx<T>.GetLast(const Predicate: TPredicate<T> = nil): T;
begin
  result := {TListEx<T>.}GetLast(self);
end;

{$endregion}

{$region 'ForEach'}

class procedure TListEx<T>.ForEach(const Enum: TEnumerable<T>; const Proc: TProc<T>; const Predicate: TPredicate<T> = nil);
begin
  if Assigned(Enum) and Assigned(Proc) then
    for var item in Enum do
      if (not Assigned(Predicate)) or Predicate(item) then
        Proc(item);
end;

procedure TListEx<T>.ForEach(const Proc: TProc<T>; const Predicate: TPredicate<T> = nil);
begin
  {TListEx<T>.}ForEach(self, Proc, Predicate);
end;

{$endregion}

{$region 'All'}

class function TListEx<T>.All(const Enum: TEnumerable<T>; const Predicate: TPredicate<T>): Boolean;
begin
  if Assigned(Enum) and Assigned(Predicate) then
    for var item in Enum do
      if not Predicate(item) then
      begin
        result := false;
        exit;
      end;
  result := true;
end;

function TListEx<T>.All(const Predicate: TPredicate<T>): Boolean;
begin
  result := {TListEx<T>.}All(Self, Predicate);
end;

{$endregion}

{$region 'Shuffle'}

class procedure TListEx<T>.Shuffle(const List: TList<T>); //based on: http://www.bytechaser.com/en/functions/p6sv9tve9v/randomly-shuffle-contents-of-any-list-in-c-sharp.aspx
begin
  if Assigned(List) then
  begin
    Randomize; //Seed the random number generator from clock
    var n := List.Count;
    while (n > 1) do
    begin
      dec(n);
      var k := random(n + 1);
      var value := list[k];
      list[k] := list[n];
      list[n] := value;
    end;
  end;
end;

procedure TListEx<T>.Shuffle;
begin
  {TListEx<T>.}Shuffle(self);
end;

{$endregion}

{$region 'AddOnce'}

class procedure TListEx<T>.AddOnce(const List: TList<T>; const Item: T);
begin
  if Assigned(List) and (not List.Contains(item)) then
    List.Add(item);
end;

procedure TListEx<T>.AddOnce(const Item: T);
begin
  {TListEx<T>.}AddOnce(self, Item);
end;

{$endregion}

{$ENDREGION ...................................................................}

{$REGION 'TInterfaceListEx' ------------------------------------------------------}

{$region 'GetAllInterface'}

class function TInterfaceListEx<T>.GetAllInterface<AInterface>(const Enum: TEnumerable<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): TInterfaceListEx<AInterface>;
begin
  var ListOfAInterface := TInterfaceListEx<AInterface>.Create;
  if Assigned(Enum) then
  begin
    var guid := TRttiInterfaceType(TRttiContext.Create.GetType(TypeInfo(AInterface))).GUID;
    var itemAsAInterface: AInterface;
    for var item in Enum do
      if Supports(item, guid, itemAsAInterface) and ((not Assigned(OuterPredicate)) or OuterPredicate(item))  then
        if (not Assigned(InnerPredicate)) or InnerPredicate(itemAsAInterface) then
          ListOfAInterface.Add(itemAsAInterface);
  end;
  result := ListOfAInterface;
end;

function TInterfaceListEx<T>.GetAllInterface<AInterface>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): TInterfaceListEx<AInterface>;
begin
  result := {TInterfaceListEx<T>.}GetAllInterface<AInterface>(Self, OuterPredicate, InnerPredicate);
end;

{$endregion}

{$region 'GetInterfaceCount'}

class function TInterfaceListEx<T>.GetInterfaceCount<AInterface>(const Enum: TEnumerable<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): Integer;
begin
  var Count: Integer := 0;
  ForEachInterface<AInterface>(Enum, procedure(item: AInterface) //TODO: if this doesn't get inlined should use a for loop like in GetAllInterface
    begin
      Inc(Count);
    end,
  OuterPredicate, InnerPredicate);
end;

function TInterfaceListEx<T>.GetInterfaceCount<AInterface>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): Integer;
begin
  result := {TInterfaceListEx<T>.}GetInterfaceCount<AInterface>(Self, OuterPredicate, InnerPredicate);
end;

{$endregion}

{$region 'GetFirstInterface'}

class function TInterfaceListEx<T>.GetFirstInterface<AInterface>(const Enum: TEnumerable<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): AInterface;
begin
  if Assigned(Enum) then
  begin
    var guid := TRttiInterfaceType(TRttiContext.Create.GetType(TypeInfo(AInterface))).GUID;
    var itemAsAInterface: AInterface;
    for var item in Enum do
      if Supports(item, guid, itemAsAInterface) and ((not Assigned(OuterPredicate)) or OuterPredicate(item)) then
        if (not Assigned(InnerPredicate)) or InnerPredicate(itemAsAInterface) then
        begin
          result := itemAsAInterface;
          exit;
        end;
  end;
  result := Default(AInterface); //returns nil
end;

function TInterfaceListEx<T>.GetFirstInterface<AInterface>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): AInterface;
begin
  result := {TInterfaceListEx<T>.}GetFirstInterface<AInterface>(Self, OuterPredicate, InnerPredicate);
end;

{$endregion}

{$region 'GetLastInterface'}

class function TInterfaceListEx<T>.GetLastInterface<AInterface>(const List: TList<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): AInterface;
begin
  if Assigned(List) then
  begin
    var guid := TRttiInterfaceType(TRttiContext.Create.GetType(TypeInfo(AInterface))).GUID;
    for var i := List.Count-1 downto 0 do
    begin
      var item := List[i];
      var itemAsAInterface: AInterface;
      if Supports(item, guid, itemAsAInterface) and ((not Assigned(OuterPredicate)) or OuterPredicate(item)) then
        if (not Assigned(InnerPredicate)) or InnerPredicate(itemAsAInterface) then
        begin
          result := itemAsAInterface;
          exit;
        end;
    end;
  end;
  result := Default(AInterface); //returns nil
end;

function TInterfaceListEx<T>.GetLastInterface<AInterface>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): AInterface;
begin
  result := {TInterfaceListEx<T>.}GetLastInterface<AInterface>(Self, OuterPredicate, InnerPredicate);
end;

{$endregion}

{$region 'ForEachInterface'}

class procedure TInterfaceListEx<T>.ForEachInterface<AInterface>(const Enum: TEnumerable<T>; const Proc: TProc<AInterface>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil);
begin
  if Assigned(Enum) and Assigned(Proc) then
  begin
    var guid := TRttiInterfaceType(TRttiContext.Create.GetType(TypeInfo(AInterface))).GUID;
    var itemAsAInterface: AInterface;
    for var item in Enum do
      if Supports(item, guid, itemAsAInterface) and ((not Assigned(OuterPredicate)) or OuterPredicate(item)) then
        if (not Assigned(InnerPredicate)) or InnerPredicate(itemAsAInterface) then
          Proc(itemAsAInterface);
  end;
end;

procedure TInterfaceListEx<T>.ForEachInterface<AInterface>(const Proc: TProc<AInterface>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil);
begin
  {TInterfaceListEx<T>.}ForEachInterface<AInterface>(Self, Proc, OuterPredicate, InnerPredicate);
end;

{$endregion}

{$region 'AllInterfaces'}

class function TInterfaceListEx<T>.AllInterfaces<AInterface>(const Enum: TEnumerable<T>; const Predicate: TPredicate<AInterface>): Boolean;
begin
  if Assigned(Enum) and Assigned(Predicate) then
  begin
    var guid := TRttiInterfaceType(TRttiContext.Create.GetType(TypeInfo(AInterface))).GUID;
    var itemAsAInterface: AInterface;
    for var item in Enum do
      if Supports(item, guid, itemAsAInterface) and not Predicate(itemAsAInterface) then
      begin
        result := false;
        exit;
      end;
  end;
  result := true;
end;

function TInterfaceListEx<T>.AllInterfaces<AInterface>(const Predicate: TPredicate<AInterface>): Boolean;
begin
  result := {TInterfaceListEx<T>.}AllInterfaces<AInterface>(Self, Predicate);
end;

{$endregion}

{$ENDREGION ...................................................................}

{$REGION 'TObjectListEx' ------------------------------------------------------}

{$region 'GetAllInterface'}

class function TObjectListEx<T>.GetAllInterface<AInterface>(const Enum: TEnumerable<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): TInterfaceListEx<AInterface>;
begin
  var ListOfAInterface := TInterfaceListEx<AInterface>.Create;
  if Assigned(Enum) then
  begin
    var guid := TRttiInterfaceType(TRttiContext.Create.GetType(TypeInfo(AInterface))).GUID;
    var itemAsAInterface: AInterface;
    for var item in Enum do
      if Supports(item, guid, itemAsAInterface) and ((not Assigned(OuterPredicate)) or OuterPredicate(item)) then
        if (not Assigned(InnerPredicate)) or InnerPredicate(itemAsAInterface) then
          ListOfAInterface.Add(itemAsAInterface);
  end;
  result := ListOfAInterface;
end;

function TObjectListEx<T>.GetAllInterface<AInterface>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): TInterfaceListEx<AInterface>;
begin
  result := {TObjectListEx<T>.}GetAllInterface<AInterface>(Self, OuterPredicate, InnerPredicate);
end;

{$endregion}

{$region 'GetInterfaceCount'}

class function TObjectListEx<T>.GetInterfaceCount<AInterface>(const Enum: TEnumerable<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): Integer;
begin
  var Count: Integer := 0;
  ForEachInterface<AInterface>(Enum, procedure(item: AInterface) //TODO: if this doesn't get inlined should use a for loop like in GetAllInterface
    begin
      Inc(Count);
    end,
  OuterPredicate, InnerPredicate);
end;

function TObjectListEx<T>.GetInterfaceCount<AInterface>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): Integer;
begin
  result := {TObjectListEx<T>.}GetInterfaceCount<AInterface>(Self, OuterPredicate, InnerPredicate);
end;

{$endregion}

{$region 'GetFirstInterface'}

class function TObjectListEx<T>.GetFirstInterface<AInterface>(const Enum: TEnumerable<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): AInterface;
begin
  if Assigned(Enum) then
  begin
    var guid := TRttiInterfaceType(TRttiContext.Create.GetType(TypeInfo(AInterface))).GUID;
    var itemAsAInterface: AInterface;
    for var item in Enum do
      if Supports(item, guid, itemAsAInterface) and ((not Assigned(OuterPredicate)) or OuterPredicate(item)) then
        if (not Assigned(InnerPredicate)) or InnerPredicate(itemAsAInterface) then
        begin
          result := itemAsAInterface;
          exit;
        end;
  end;
  result := Default(AInterface); //returns nil
end;

function TObjectListEx<T>.GetFirstInterface<AInterface>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): AInterface;
begin
  result := {TObjectListEx<T>.}GetFirstInterface<AInterface>(Self, OuterPredicate, InnerPredicate);
end;

{$endregion}

{$region 'GetLastInterface'}

class function TObjectListEx<T>.GetLastInterface<AInterface>(const List: TList<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): AInterface;
begin
  if Assigned(List) then
  begin
    var guid := TRttiInterfaceType(TRttiContext.Create.GetType(TypeInfo(AInterface))).GUID;
    for var i := List.Count-1 downto 0 do
    begin
      var item := List[i];
      var itemAsAInterface: AInterface;
      if Supports(item, guid, itemAsAInterface) and ((not Assigned(OuterPredicate)) or OuterPredicate(item)) then
        if (not Assigned(InnerPredicate)) or InnerPredicate(itemAsAInterface) then
        begin
          result := itemAsAInterface;
          exit;
        end;
    end;
  end;
  result := Default(AInterface); //returns nil
end;

function TObjectListEx<T>.GetLastInterface<AInterface>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): AInterface;
begin
  result := {TObjectListEx<T>.}GetLastInterface<AInterface>(Self, OuterPredicate, InnerPredicate);
end;

{$endregion}

{$region 'ForEachInterface'}

class procedure TObjectListEx<T>.ForEachInterface<AInterface>(const Enum: TEnumerable<T>; const Proc: TProc<AInterface>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil);
begin
  if Assigned(Enum) and Assigned(Proc) then
  begin
    var guid := TRttiInterfaceType(TRttiContext.Create.GetType(TypeInfo(AInterface))).GUID;
    var itemAsAInterface: AInterface;
    for var item in Enum do
      if Supports(item, guid, itemAsAInterface) and ((not Assigned(OuterPredicate)) or OuterPredicate(item)) then
        if (not Assigned(InnerPredicate)) or InnerPredicate(itemAsAInterface) then
          Proc(itemAsAInterface);
  end;
end;

procedure TObjectListEx<T>.ForEachInterface<AInterface>(const Proc: TProc<AInterface>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil);
begin
  {TObjectListEx<T>.}ForEachInterface<AInterface>(Self, Proc, OuterPredicate, InnerPredicate);
end;

{$endregion}

{$region 'AllInterfaces'}

class function TObjectListEx<T>.AllInterfaces<AInterface>(const Enum: TEnumerable<T>; const Predicate: TPredicate<AInterface>): Boolean;
begin
  if Assigned(Enum) and Assigned(Predicate) then
  begin
    var guid := TRttiInterfaceType(TRttiContext.Create.GetType(TypeInfo(AInterface))).GUID;
    var itemAsAInterface: AInterface;
    for var item in Enum do
      if Supports(item, guid, itemAsAInterface) and not Predicate(itemAsAInterface) then
      begin
        result := false;
        exit;
      end;
  end;
  result := true;
end;

function TObjectListEx<T>.AllInterfaces<AInterface>(const Predicate: TPredicate<AInterface>): Boolean;
begin
  result := {TObjectListEx<T>.}AllInterfaces<AInterface>(Self, Predicate);
end;

{$endregion}

{$region 'GetAllClass'}

class function TObjectListEx<T>.GetAllClass<AClass>(const Enum: TEnumerable<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil): TObjectListEx<AClass>;
begin
  var ListOfAClass := TObjectListEx<AClass>.Create;
  if Assigned(Enum) then
    for var item in Enum do
      if (item is AClass) and ((not Assigned(OuterPredicate)) or OuterPredicate(item)) then
        begin
        var itemAsAClass := item as AClass;
        if (not Assigned(InnerPredicate)) or InnerPredicate(ItemAsAClass) then
          ListOfAClass.Add(itemAsAClass);
        end;
  result := ListOfAClass;
end;

function TObjectListEx<T>.GetAllClass<AClass>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil): TListEx<AClass>;
begin
  result := {TObjectListEx<T>.}GetAllClass<AClass>(Self, OuterPredicate, InnerPredicate);
end;

{$endregion}

{$region 'GetClassCount'}

class function TObjectListEx<T>.GetClassCount<AClass>(const Enum: TEnumerable<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil): Integer;
begin
  var Count: Integer := 0;
  ForEachClass<AClass>(Enum, procedure(item: AClass) //TODO: if this doesn't get inlined should use a for loop like in GetAllClass
    begin
      Inc(Count);
    end,
  OuterPredicate, InnerPredicate);
end;

function TObjectListEx<T>.GetClassCount<AClass>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil): Integer;
begin
  result := {TObjectListEx<T>.}GetClassCount<AClass>(Self, OuterPredicate, InnerPredicate);
end;

{$endregion}

{$region 'GetFirstClass'}

class function TObjectListEx<T>.GetFirstClass<AClass>(const Enum: TEnumerable<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil): AClass;
begin
  if Assigned(Enum) then
    for var item in Enum do
      if (item is AClass) and ((not Assigned(OuterPredicate)) or OuterPredicate(item)) then
        begin
        var itemAsAClass := item as AClass;
        if (not Assigned(InnerPredicate)) or InnerPredicate(ItemAsAClass) then
          begin
          result := itemAsAClass;
          exit;
          end;
        end;
  result := Default(AClass); //returns nil
end;

function TObjectListEx<T>.GetFirstClass<AClass>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil): AClass;
begin
  result := {TObjectListEx<T>.}GetFirstClass<AClass>(Self, OuterPredicate, InnerPredicate);
end;

{$endregion}

{$region 'GetLastClass'}

class function TObjectListEx<T>.GetLastClass<AClass>(const List: TList<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil): AClass;
begin
  if Assigned(List) then
    for var i := List.Count-1 downto 0 do
      begin
      var item := List[i];
      if (item is AClass) and ((not Assigned(OuterPredicate)) or OuterPredicate(item)) then
        begin
        var itemAsAClass := item as AClass;
        if (not Assigned(InnerPredicate)) or InnerPredicate(ItemAsAClass) then
          begin
          result := itemAsAClass;
          exit;
          end;
        end;
      end;
  result := Default(AClass); //returns nil
end;

function TObjectListEx<T>.GetLastClass<AClass>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil): AClass;
begin
  result := {TObjectListEx<T>.}GetLastClass<AClass>(Self, OuterPredicate, InnerPredicate);
end;

{$endregion}

{$region 'ForEachClass'}

class procedure TObjectListEx<T>.ForEachClass<AClass>(const Enum: TEnumerable<T>; const Proc: TProc<AClass>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil);
begin
  if Assigned(Enum) and Assigned(Proc) then
    for var item in Enum do
      if (item is AClass) and ((not Assigned(OuterPredicate)) or OuterPredicate(item)) then
        begin
        var itemAsAClass := item as AClass;
        if (not Assigned(InnerPredicate)) or InnerPredicate(ItemAsAClass) then
          Proc(itemAsAClass);
        end;
end;

procedure TObjectListEx<T>.ForEachClass<AClass>(const Proc: TProc<AClass>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil);
begin
  {TObjectListEx<T>.}ForEachClass<AClass>(Self, Proc, OuterPredicate, InnerPredicate);
end;

{$endregion}

{$region 'AllClasses'}

class function TObjectListEx<T>.AllClasses<AClass>(const Enum: TEnumerable<T>; const Predicate: TPredicate<AClass>): Boolean;
begin
  if Assigned(Enum) and Assigned(Predicate) then
    for var item in Enum do
      if (item is AClass) and not Predicate(item as AClass) then
      begin
        result := false;
        exit;
      end;
  result := true;
end;

function TObjectListEx<T>.AllClasses<AClass>(const Predicate: TPredicate<AClass>): Boolean;
begin
  result := {TObjectListEx<T>.}AllClasses<AClass>(Self, Predicate);
end;

{$endregion}

{$region 'FreeAll'}

class procedure TObjectListEx<T>.FreeAll(const List: TList<T>);
begin
  for var i := List.Count-1 downto 0 do //working the list backwards since we're removing items from it
  begin
    var Item := List[i];
    List.Delete(i); //in case the list contains the item multiple times
    FreeAndNil(Item); //if this causes side-effect that calls List.Remove(Item) assuming it won't fail if item has already been removed. Issue may occur though if list contains Item multiple times since that would cause the 1st occurence found to be removed
  end;
end;

procedure TObjectListEx<T>.FreeAll;
begin
  {TObjectListEx<T>.}FreeAll(Self);
end;

{$endregion}

{$ENDREGION ...................................................................}

end.
