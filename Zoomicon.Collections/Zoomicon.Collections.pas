unit Zoomicon.Collections;

interface
  uses
    SysUtils, //for TPredicate
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
    if (@Predicate = nil) or Predicate(item) then
      ResultList.Add(item);
  result := ResultList;
end;

function TListEx<T>.GetAll(const Predicate: TPredicate<T> = nil): TListEx<T>;
begin
  result := {TListEx<T>.}GetAll(self);
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
    if Supports(item, guid, itemAsAInterface) and ((@OuterPredicate = nil) or OuterPredicate(item))  then
      if (@InnerPredicate = nil) or InnerPredicate(itemAsAInterface) then
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
    if Supports(item, guid, itemAsAInterface) and ((@OuterPredicate = nil) or OuterPredicate(item))  then
      if (@InnerPredicate = nil) or InnerPredicate(itemAsAInterface) then
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
    if (item is AClass) and ((@OuterPredicate = nil) or OuterPredicate(item)) then
      begin
      var itemAsAClass := item as AClass;
      if (@InnerPredicate = nil) or InnerPredicate(ItemAsAClass) then
        ListOfAClass.Add(itemAsAClass);
      end;
  result := ListOfAClass;
end;

type y = TPredicate<TObject>;
type x = TPredicate<TListEx<Boolean>>;

function TObjectListEx<T>.GetAllOfClass<AClass>(const OuterPredicate: TPredicate<T> = nil; const InnerPredicate: TPredicate<AClass> = nil): TListEx<AClass>;
begin
  result := {TObjectListEx<T>.}GetAllOfClass<AClass>(Self, OuterPredicate, InnerPredicate);
end;

{$endregion}

{$ENDREGION ...................................................................}

end.
