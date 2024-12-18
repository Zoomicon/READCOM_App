unit FlowLayoutEx;

interface
  uses
    FMX.Layouts; //for TFlowLayout

  type
    TFlowLayoutEx = class(TFlowLayout)
    private
      FRules: TFlowLayoutRules;
    public
      procedure DoRealign; override;
    end;

implementation
  uses
    System.Types, //for TPointF, TRectF
    System.SysUtils, //for Supports, IfThen
    System.Classes, //for csDesigning
    System.Math, //for Max
    FMX.Controls, //for TControl
    FMX.Types; //for RegisterFmxClasses

  {$region 'TFlowLayoutHelper'}
  type
    TFlowLayoutHelper = class helper for TFlowLayout
    protected
      function Rules: TFlowLayoutRules;
    end;

  function TFlowLayoutHelper.Rules: TFlowLayoutRules;
  begin
    Result.Justify := Justify;
    Result.JustifyLast := JustifyLastLine;
    Result.Direction := FlowDirection;
    Result.HorizontalGap := HorizontalGap;
    Result.VerticalGap := VerticalGap;
  end;
  {$endregion}

  {$region 'TFlowLayoutBreakHelper'}
  type
    TFlowLayoutBreakHelper = class helper for TFlowLayoutBreak
    protected
      function Rules: TFlowLayoutRules;
    end;

  function TFlowLayoutBreakHelper.Rules: TFlowLayoutRules;
  begin
    Result.Justify := Justify;
    Result.JustifyLast := JustifyLastLine;
    Result.Direction := FlowDirection;
    Result.HorizontalGap := HorizontalGap;
    Result.VerticalGap := VerticalGap;
  end;
  {$endregion}

  procedure TFlowLayoutEx.DoRealign;

    function IsDesignerControl(const AControl: TControl): Boolean;
    begin
      Result := (csDesigning in ComponentState) and Supports(AControl, IDesignerControl);
    end;

    function ControlWidth(const Control: TControl): Single;
    begin
      if Control.Visible and not IsDesignerControl(Control) then
        Result := Control.Width + Control.Margins.Left + Control.Margins.Right
      else
        Result := 0;
    end;

    function ControlHeight(const Control: TControl; const Rules: TFlowLayoutRules): Single;
    begin
      if Control.Visible and not IsDesignerControl(Control) then
        Result := Control.Height
                + Control.Margins.Top + Control.Margins.Bottom
                + Rules.VerticalGap
      else
        Result := 0;
    end;

    function GetJustify(const LastLine: Boolean; const Rules: TFlowLayoutRules): TFlowJustify;
    begin
      if LastLine then
        Result := Rules.JustifyLast
      else
        Result := Rules.Justify;
    end;

    function Gap(const First: Boolean): Single;
    begin
      if First then
        Result := 0
      else
        Result := FRules.HorizontalGap;
    end;

  type
    TState = (NewLine, Measure, PreLayout, Layout, &End);
  const
    InitialState: array [False..True] of TState = (TState.&End, TState.NewLine);
  var
    I: Integer;
    State: TState;
    CurPos: TPointF;
    Control: TControl;
    ControlBounds: TRectF;
    LineStartIdx, LineEndIdx: Integer;
    WidthAccu: Single;
    LineHeight: Single;
    ClientWidth: Single;
    Add: Single;
    NextWidth: Single;
    NextRules: TFlowLayoutRules;
    CurrentRules: TFlowLayoutRules;
    ControlsList: TControlList;
    WasNewLineStarted: Boolean;
  begin
    if FDisableAlign or (Controls = nil) then
      Exit;

    FRules := Rules; //Calling Rules method (defined by TFlowLayoutHelper) and filling-in descendent's FRules field since the ancestor's FRules is private (could also have used a class helper to add a method that returns that private field)

    FDisableAlign := True;

    CurPos := Padding.Rect.TopLeft;
    ClientWidth := Width - Padding.Left - Padding.Right;
    LineHeight := 0;

    CurrentRules := FRules;
    NextRules := FRules;

    I := 0;
    LineStartIdx := 0;
    LineEndIdx := 0;
    WidthAccu := 0;
    WasNewLineStarted := True;
    Add := 0;
    State := InitialState[Controls.Count <> 0];
    ControlsList := TControlList.Create(Controls);
    try
      while State <> TState.End do
      begin
        Control := TControl(ControlsList[i]);
        //Control.Visible := true;

        //See here on where to see logs: https://github.com/DelphiWorlds/HowTo/blob/main/Solutions/LogViewers/Readme.md
        //TODO: should also log State 
        Log.d('TFlowLayoutEx.DoRealign', Control,
          (function: string
            begin
              result := Control.Name;
              if Control.Visible
                then result := result + ' Visible'
                else result := result + ' Invisible';
            end
          )()

          //IfThen<String>(Control.Visible, Control.Name + ' Visible', Control.Name + ' Invisible') //IfThen doesn't support string, see TF.Iff instead from Zoomicon.Generics

          //(if Control.Visible then Control.Name + ' Visible' else Control.Name + ' Invisible') //would be nice if Delphi had inline if statements (expressions)
        );

        case State of
          TState.NewLine:
            begin
              CurrentRules := NextRules;
              LineStartIdx := I;
              LineEndIdx := I;
              WasNewLineStarted := True;
              WidthAccu := 0;
              CurPos.Y := CurPos.Y + LineHeight;
              LineHeight := ControlHeight(Control, CurrentRules);
              State := TState.Measure;
            end;

          TState.Measure:
            begin
              // If there are invisible controls in the beginning of current line, we should skip them
              // for correct calculation first Gap.
              if WasNewLineStarted then
                if Control.Visible then
                  WasNewLineStarted := False
                else if (LineStartIdx < ControlsList.Count - 1) and not (Control is TFlowLayoutBreak) then
                  Inc(LineStartIdx);

              if Control.Visible and not IsDesignerControl(Control) then
                NextWidth := WidthAccu + Gap(I = LineStartIdx) + ControlWidth(Control)
              else
                NextWidth := WidthAccu;

              if Control is TFlowLayoutBreak then
              begin
                if TFlowLayoutBreak(Control).ChangesRules then
                  NextRules := TFlowLayoutBreak(Control).Rules; //Zoomicon: using Rules method from TFlowLayoutBreakHelper instead of FRules since we don't have access to private fields
                I := LineStartIdx;
                State := TState.PreLayout;
              end
              else if NextWidth > ClientWidth then
              begin
                // width overflow, layout the line

                // update WidthAccu if the line has only 1 control
                if I = LineStartIdx then
                  WidthAccu := NextWidth;

                I := LineStartIdx;
                State := TState.PreLayout;
              end
              else if I = ControlsList.Count - 1 then
              begin
                // no more ControlsList, layout the line and done
                WidthAccu := NextWidth;
                LineHeight := Max(LineHeight, ControlHeight(Control, CurrentRules));
                LineEndIdx := I;
                I := LineStartIdx;
                State := TState.PreLayout;
              end
              else
              begin
                // keep counting width
                WidthAccu := NextWidth;
                LineEndIdx := I;
                LineHeight := Max(LineHeight, ControlHeight(Control, CurrentRules));
                Inc(I);
              end;
            end;

          TState.PreLayout:
          begin
            // Prepare to layout the line
            Add := 0;

            case GetJustify(LineEndIdx = (ControlsList.Count - 1), CurrentRules) of
              TFlowJustify.Left:
                CurPos.X := Padding.Left;
              TFlowJustify.Right:
                CurPos.X := Self.Width - Padding.Right - WidthAccu;
              TFlowJustify.Center:
                CurPos.X := Padding.Left + ClientWidth / 2 - WidthAccu / 2;
              TFlowJustify.Justify:
                begin
                  CurPos.X := Padding.Left;
                  if LineEndIdx - LineStartIdx > 0 then
                    Add := (ClientWidth - WidthAccu) / (LineEndIdx - LineStartIdx);
                end;
            end;

            case CurrentRules.Direction of
              TFlowDirection.LeftToRight: I := LineStartIdx;
              TFlowDirection.RightToLeft: I := LineEndIdx;
            end;

            State := TState.Layout;
          end;

          TState.Layout:
          begin
            if not IsDesignerControl(ControlsList[I]) then
            begin
              ControlBounds := Control.BoundsRect;
              ControlBounds.SetLocation(CurPos + Control.Margins.Rect.TopLeft);
              Control.BoundsRect := ControlBounds;
              if Control.Visible then
                CurPos.X := CurPos.X + ControlWidth(Control) + Add + CurrentRules.HorizontalGap;
            end;

            // advance to the next control in line
            case CurrentRules.Direction of
              TFlowDirection.LeftToRight:
                begin
                  if I = ControlsList.Count - 1 then
                    State := TState.End
                  else if I = LineEndIdx then
                    State := TState.NewLine;

                  Inc(I);
                end;
              TFlowDirection.RightToLeft:
                begin
                  if I = LineStartIdx then
                  begin
                    I := LineEndIdx + 1;

                    if LineEndIdx = ControlsList.Count - 1 then
                      State := TState.End
                    else
                      State := TState.NewLine;
                  end
                  else
                    Dec(I);
                end;
            end;
          end;
        end;
      end;
    finally
      ControlsList.Free;
    end;
    FDisableAlign := false;
  end;

  procedure RegisterSerializationClasses;
  begin
    RegisterFmxClasses([
      TFlowLayoutEx
    ]);
  end;

  procedure Register;
  begin
    GroupDescendentsWith(TFlowLayoutEx, TFlowLayout);
    RegisterComponents('Zoomicon', [TFlowLayoutEx]);
  end;

  {$endregion}

initialization
  RegisterSerializationClasses; //don't call Register here, it's called by the IDE automatically on a package installation (fails at runtime)

end.
