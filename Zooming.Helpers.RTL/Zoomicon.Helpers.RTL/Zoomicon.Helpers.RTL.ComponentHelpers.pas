unit Zoomicon.Helpers.RTL.ComponentHelpers;

interface
  uses System.Classes; //for TComponent

  type

    TComponentNameHelper = class helper for TComponent
      public
        function FindSafeNewName(const NewName: TComponentName; AOwner: TComponent = nil): TComponentName;
    end;

implementation
  uses System.SysUtils; //for Exception

{$REGION 'TComponentNameHelper'}

function TComponentNameHelper.FindSafeNewName(const NewName: TComponentName; AOwner: TComponent = nil): TComponentName;
begin
  if not Assigned(AOwner) then //if AOwner is not defined, use current Owner
    AOwner := Owner;

  //See if name exists at other components that are owned by given owner and if it does try renaming with a 2,3,... suffix
  var theName := NewName;
  var counter: Integer := 1;
  if Assigned(AOwner) then
    while Assigned(AOwner.FindComponent(theName)) do
    begin
      Inc(counter);
      theName := NewName + IntToStr(counter);
    end;

  result := theName;
end;

{$ENDREGION}

end.
