unit Zoomicon.Collections;

interface
  uses System.Generics.Collections; //for TList

type

  TListExDecorator<T> = class abstract(TList<T>)
    function PickRandom: T; virtual; abstract;
  end;

  TListEx<T> = class(TListExDecorator<T>)
    function PickRandom: T; virtual;
  end;

implementation

{ TListEx<T> }

function TListEx<T>.PickRandom: T;
begin
  var c := Count;
  if c = 0 then
    result := Default(T) //returns Default value for type T
  else
    result := Items[Random(c)]; //TList uses 0-based index, and 0 <= Random(c) < c
end;

end.
