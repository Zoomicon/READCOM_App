unit Zoomicon.Generics.Functors;

interface
  uses SysUtils; //for TPredicate

type
  TFunctor = class
    class function Iff<T>(const Condition: Boolean; const ValueIfTrue, ValueIfFalse: T): T; overload; inline;
    class function Iff<T>(const Value: T; const Condition: TPredicate<T>; const ValueIfTrue, ValueIfFalse: T): T; overload; inline;
  end;

implementation

class function TFunctor.Iff<T>(const Condition: Boolean; const ValueIfTrue, ValueIfFalse: T): T;
begin
  if Condition then
    result := ValueIfTrue
  else
    result := ValueIfFalse;
end;

class function TFunctor.Iff<T>(const Value: T; const Condition: TPredicate<T>; const ValueIfTrue, ValueIfFalse: T): T;
begin
  Iff(Condition(Value), ValueIfTrue, ValueIfFalse);
end;

end.
