unit Zoomicon.Collections;

interface
  uses
    System.Generics.Collections, //for TList
    System.Rtti; //for RttiContext

type

  TListEx<T> = class(TList<T>)
    { GetRandom }
    class function GetRandom(const list: TList<T>): T; overload;
    function GetRandom: T; overload; virtual;
  end;

  TInterfaceListEx<T: IInterface> = class(TListEx<T>)
  end;

  TObjectListEx<T: class> = class(TListEx<T>)
    { GetAllOfInterface }
    class function GetAllOfInterface<AInterface: IInterface>(const list: TList<T>): TInterfaceListEx<AInterface>; overload;
    function GetAllOfInterface<AInterface: IInterface>: TInterfaceListEx<AInterface>; overload;

    { GetAllOfClass }
    class function GetAllOfClass<AClass: class>(const list: TList<T>): TObjectListEx<AClass>; overload;
    function GetAllOfClass<AClass: class>: TListEx<AClass>; overload;
  end;

implementation
  uses
    SysUtils;

{$REGION 'TListEx'}

{$region 'GetRandom'}

class function TListEx<T>.GetRandom(const list: TList<T>): T;
begin
  with list do
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

{$ENDREGION}

{$REGION 'TClassListEx'}

{$region 'GetAllOfInterface'}

class function TObjectListEx<T>.GetAllOfInterface<AInterface>(const list: TList<T>): TInterfaceListEx<AInterface>;
var
  itemAsAInterface: AInterface;
begin
  var guid := TRttiInterfaceType(TRttiContext.Create.GetType(TypeInfo(AInterface))).GUID;
  var listOfAInterface := TInterfaceListEx<AInterface>.Create;
  for var item in list do
    if Supports(item, guid, itemAsAInterface) then
      listOfAInterface.Add(itemAsAInterface);
  result := listOfAInterface;
end;

function TObjectListEx<T>.GetAllOfInterface<AInterface>: TInterfaceListEx<AInterface>;
begin
  result := {TClassListEx<T>.}GetAllOfInterface<AInterface>(self);
end;

{$endregion}

{$region 'GetAllOfClass'}

class function TObjectListEx<T>.GetAllOfClass<AClass>(const list: TList<T>): TObjectListEx<AClass>;
begin
  var listOfAClass := TObjectListEx<AClass>.Create;
  for var item in list do
    if item is AClass then
      listOfAClass.Add(item as AClass);
  result := listOfAClass;
end;

function TObjectListEx<T>.GetAllOfClass<AClass>: TListEx<AClass>;
begin
  result := {TClassListEx<T>.}GetAllOfClass<AClass>(self);
end;

{$endregion}

{$ENDREGION}

end.
