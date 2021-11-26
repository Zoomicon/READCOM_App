unit Zoomicon.Generics.Functors;

interface
  uses SysUtils; //for TPredicate

type
  TF = class
    class function Clamp<T>(const Value: T; const MinValue, MaxValue: T): T; inline; //EnsureRange
    class function Iff<T>(const Condition: Boolean; const ValueIfTrue, ValueIfFalse: T): T; overload; inline;
    class function Iff<T>(const Condition: Boolean; const ValueIfTrue, ValueIfFalse: TFunc<T>): T; overload; inline;
    class function Iff<T>(const Value: T; const Condition: TPredicate<T>; const ValueIfTrue, ValueIfFalse: T): T; overload; inline;
    class function Iff<T>(const Value: T; const Condition: TPredicate<T>; const ValueIfTrue, ValueIfFalse: TFunc<T>): T; overload; inline;
    class function Iff<T>(const Value: T; const Condition: TPredicate<T>; const ValueIfTrue, ValueIfFalse: TFunc<T,T>): T; overload; inline;
  end;

implementation
  uses System.Generics.Defaults; //for TComparer

class function TF.Clamp<T>(const Value: T; const MinValue, MaxValue: T): T;
begin
  result := Value;

  var Comparer := TComparer<T>.Default; //TODO: document if throws exception if T is something not comparable and/or for "nil" value or bounds
  if Comparer.Compare(result, MinValue) < 0 then //if (Value < MinValue)
    result := MinValue
  else if Comparer.Compare(result, MaxValue) > 0 then //if (Value > MaxValue)
    result := MaxValue;
end;

class function TF.Iff<T>(const Condition: Boolean; const ValueIfTrue, ValueIfFalse: T): T;
begin
  if Condition then
    result := ValueIfTrue
  else
    result := ValueIfFalse;
end;

class function TF.Iff<T>(const Condition: Boolean; const ValueIfTrue, ValueIfFalse: TFunc<T>): T;
begin
  if Condition and Assigned(ValueIfTrue) then
    result := ValueIfTrue
  else
    result := ValueIfFalse; //Note: will fail if ValueIfFalse is not assigned
end;

class function TF.Iff<T>(const Value: T; const Condition: TPredicate<T>; const ValueIfTrue, ValueIfFalse: T): T;
begin
  if Assigned(Condition) then
    result := Iff(Condition(Value), ValueIfTrue, ValueIfFalse)
  else
    result := ValueIfFalse;
end;

class function TF.Iff<T>(const Value: T; const Condition: TPredicate<T>; const ValueIfTrue, ValueIfFalse: TFunc<T>): T;
begin
  //result := Iff(Value, Condition, ValueIfTrue(), ValueIfFalse()); //use of () seems to be needed else compiler seems to match the same function (infinite recursion) //DOESN'T COMPILE (probably Delphi bug)
  if Assigned(Condition) then
    result := Iff(Condition(Value), ValueIfTrue, ValueIfFalse) //TODO: not sure if evaluation is deferred here (aka which "Iff" gets called [CTRL+Click in Delphi doesn't seem that clever], @ValueIfTrue/@ValueIfFalse to force that don't seem to compile)
  else
    result := ValueIfFalse; //Note: will fail if ValueIfFalse is not assigned
end;

class function TF.Iff<T>(const Value: T; const Condition: TPredicate<T>; const ValueIfTrue, ValueIfFalse: TFunc<T,T>): T;
begin
  //result := Iff(Value, Condition, ValueIfTrue(Value), ValueIfFalse(Value)); //DOESN'T COMPILE (probably Delphi bug)
  if Assigned(Condition) and Assigned(ValueIfTrue) {and Assigned(ValueIfFalse)} then //no need to check Assigned(ValueIfFalse) here, since in any case it will fail
    result := Iff(Condition(Value), ValueIfTrue(Value), ValueIfFalse(Value)) //Note: will fail if ValueIfFalse is not assigned
  else
    result := ValueIfFalse(Value); //Note: will fail if ValueIfFalse is not assigned
end;

end.
