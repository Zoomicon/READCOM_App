unit Zoomicon.Generics.Collections;

interface
  uses
    SysUtils, //for TPredicate, TProc
    System.Generics.Collections; //for TList

type

  TListEx<T> = class(TList<T>)
    { GetRandom }
    class function GetRandom(const List: TList<T>): T; overload;
    function GetRandom: T; overload; virtual;

    { GetAll }
    class function GetAll(const Enum: TEnumerable<T>; const Predicate: TPredicate<T> = nil): TListEx<T>; overload;
    function GetAll(const Predicate: TPredicate<T> = nil): TListEx<T>; overload;

    { GetFirst }
    class function GetFirst(const Enum: TEnumerable<T>; const Predicate: TPredicate<T> = nil): T; overload;
    function GetFirst(const Predicate: TPredicate<T> = nil): T; overload;

    { GetLast }
    class function GetLast(const List: TList<T>; const Predicate: TPredicate<T> = nil): T; overload;
    function GetLast(const Predicate: TPredicate<T> = nil): T; overload;

    { ForEach }
    class procedure ForEach(const Enum: TEnumerable<T>; const Proc: TProc<T>; const Predicate: TPredicate<T> = nil); overload;
    procedure ForEach(const Proc: TProc<T>; const Predicate: TPredicate<T> = nil); overload;

    { All }
    class function All(const Enum: TEnumerable<T>; const Predicate: TPredicate<T>): Boolean; overload;
    function All(const Predicate: TPredicate<T>): Boolean; overload;

    { Shuffle }
    class procedure Shuffle(const List: TList<T>); overload; //based on: http://www.bytechaser.com/en/functions/p6sv9tve9v/randomly-shuffle-contents-of-any-list-in-c-sharp.aspx
    procedure Shuffle; overload;

    { AddOnce }
    class procedure AddOnce(const List: TList<T>; const Item: T); overload;
    procedure AddOnce(const Item: T); overload;
  end;

  //----------------------------------------------------------------------------

  TInterfaceListEx<T: IInterface> = class(TListEx<T>)
    { GetAllInterface }
    class function GetAllInterface<AInterface: IInterface>(const Enum: TEnumerable<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): TInterfaceListEx<AInterface>; overload;
    function GetAllInterface<AInterface: IInterface>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): TInterfaceListEx<AInterface>; overload;

    { GetFirstInterface }
    class function GetFirstInterface<AInterface: IInterface>(const Enum: TEnumerable<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): AInterface; overload;
    function GetFirstInterface<AInterface: IInterface>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): AInterface; overload;

    { GetLastInterface }
    class function GetLastInterface<AInterface: IInterface>(const List: TList<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): AInterface; overload;
    function GetLastInterface<AInterface: IInterface>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): AInterface; overload;

    { ForEachInterface }
    class procedure ForEachInterface<AInterface: IInterface>(const Enum: TEnumerable<T>; const Proc: TProc<AInterface>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil); overload;
    procedure ForEachInterface<AInterface: IInterface>(const Proc: TProc<AInterface>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil); overload;
  end;

  //----------------------------------------------------------------------------

  TObjectListEx<T: class> = class(TListEx<T>)
    { GetAllInterface }
    class function GetAllInterface<AInterface: IInterface>(const Enum: TEnumerable<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): TInterfaceListEx<AInterface>; overload;
    function GetAllInterface<AInterface: IInterface>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): TInterfaceListEx<AInterface>; overload;

    { GetFirstInterface }
    class function GetFirstInterface<AInterface: IInterface>(const Enum: TEnumerable<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): AInterface; overload;
    function GetFirstInterface<AInterface: IInterface>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): AInterface; overload;

    { GetLastInterface }
    class function GetLastInterface<AInterface: IInterface>(const List: TList<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): AInterface; overload;
    function GetLastInterface<AInterface: IInterface>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil): AInterface; overload;

    { GetAllClass }
    class function GetAllClass<AClass: class>(const Enum: TEnumerable<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil): TObjectListEx<AClass>; overload;
    function GetAllClass<AClass: class>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil): TListEx<AClass>; overload;

    { GetFirstClass }
    class function GetFirstClass<AClass: class>(const Enum: TEnumerable<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil): AClass; overload;
    function GetFirstClass<AClass: class>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil): AClass; overload;

    { GetLastClass }
    class function GetLastClass<AClass: class>(const List: TList<T>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil): AClass; overload;
    function GetLastClass<AClass: class>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil): AClass; overload;

    { ForEachInterface }
    class procedure ForEachInterface<AInterface: IInterface>(const Enum: TEnumerable<T>; const Proc: TProc<AInterface>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil); overload;
    procedure ForEachInterface<AInterface: IInterface>(const Proc: TProc<AInterface>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AInterface> = nil); overload;

    {ForEachClass}
    class procedure ForEachClass<AClass: class>(const Enum: TEnumerable<T>; const Proc: TProc<AClass>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil); overload;
    procedure ForEachClass<AClass: class>(const Proc: TProc<AClass>; const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil); overload;
  end;

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

class function TListEx<T>.GetAll(const Enum: TEnumerable<T>; const Predicate: TPredicate<T> = nil): TListEx<T>;
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
  result := false;
  if Assigned(Enum) and Assigned(Predicate) then
    for var item in Enum do
      if not Predicate(item) then exit;
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

{$ENDREGION ...................................................................}

end.
