unit Zoomicon.Manipulation.FMX.Manipulator;

interface

uses
  Zoomicon.Manipulation.FMX.CustomManipulator; //for TCustomManipulator

type

  {$REGION 'TManipulator' -----------------------------------------------------}

  TManipulator = class(TCustomManipulator)
  published
    property AreaSelector;
    property DropTarget;
    property AutoSize;
    property EditMode;
    property Proportional;
  end;

  {$ENDREGION}

procedure Register;

implementation
  uses
    System.Classes, //for GroupDecendentsWith, RegisterComponents
    FMX.Controls, //for TControl
    FMX.Types; //for RegisterFmxClasses

{$R *.fmx}

procedure RegisterClasses;
begin
  RegisterFmxClasses([TManipulator]); //register for persistence (in case they're used standalone)
end;

procedure Register;
begin
  GroupDescendentsWith(TManipulator, TControl);
  RegisterClasses;
  RegisterComponents('Zoomicon', [TManipulator]);
end;

initialization
  RegisterClasses; //don't call Register here, it's called by the IDE automatically on a package installation (fails at runtime)

end.
