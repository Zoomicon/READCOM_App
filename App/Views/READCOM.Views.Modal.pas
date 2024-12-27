//Description: Modal prompt frame
//Author: George Birbilis (http://zoomicon.com)

unit READCOM.Views.Modal;

interface
  {$region 'Used units'}
  uses
    System.SysUtils, //for FreeAndNil
    System.UITypes, //for crDefault, crHandPoint
    System.Classes, //for TComponent, TThread
    FMX.Controls, //added automatically by the IDE when saving the frame
    FMX.Types, //for TFmxObject
    FMX.Forms, //for TFrame
    FMX.Objects, //for TRectangle
    FMX.ImgList; //for TGlyph
  {$endregion}

  type
    TModalFrame = class(TFrame)
      rectBackground: TRectangle;
      GlyphIcon: TGlyph;
      procedure rectBackgroundClick(Sender: TObject);

    protected
      FCanClose: Boolean;

      const
        DEFAULT_CANCLOSE = true;

      class var
        Frame: TModalFrame;

    protected
      function GetCanClose: Boolean;
      procedure SetCanClose(const Value: Boolean);

      //class destructor Destroy;
      class function IsShown: Boolean; static;

    public
      constructor Create(AOwner: TComponent); override;

      class procedure ShowModal(const TheParent: TFmxObject; const VisibleFlag: Boolean = true);
      class procedure Close;
      class property Shown: Boolean read IsShown;

    published
      property CanClose: Boolean read GetCanClose write SetCanClose default DEFAULT_CANCLOSE;

    end;

implementation

  {$R *.fmx}

  {$region 'Life-time management'}

  constructor TModalFrame.Create(AOwner: TComponent);
  begin
    inherited;
    CanClose := DEFAULT_CANCLOSE;
  end;

  (*
  class destructor TModalFrame.Destroy;
  begin
    //FreeAndNil(Frame); //DO NOT DO THIS HERE, CAUSES INVALID POINTER OPERATION WHEN MAIN FORM IS CLOSED WHILE THE FRAME IS STILL OPEN IN IT (cause Frame is owned by MainForm and has been already freed by it when we reach this class destructor)
  end;
  *)

  {$endregion}

  {$region 'Properties'}

  {$region 'CanClose'}

  function TModalFrame.GetCanClose: Boolean;
  begin
    result := FCanClose;
  end;

  procedure TModalFrame.SetCanClose(const Value: Boolean);
  begin
    FCanClose := Value;
    if Value then
      rectBackground.Cursor := crHandPoint
    else
      rectBackground.Cursor := crDefault;
  end;

  {$endregion}

  {$endregion}

  {$region 'Methods'}

  class function TModalFrame.IsShown: Boolean;
  begin
    result := Assigned(Frame); //assumes ShowModal does FreeAndNil on the frame when closing, instead of just hiding it
  end;

  class procedure TModalFrame.ShowModal(const TheParent: TFmxObject; const VisibleFlag: Boolean = true);
  begin
    if not Assigned(Frame) then
    begin
      if VisibleFlag = false then
        exit; //Assuming Close does FreeAndNil to Frame instead of hiding it: no need to create a frame and then destroy it if hide is asked for without a modal frame being shown at that moment

      Frame := Create(Application); //use Application as the Owner since we reuse the same ModalFrame instance
      //Frame.Align := TAlignLayout.Content; //already set at frame designer (could be a property [Content, Center, Scale etc.])
    end;

    with Frame do
    begin
      Parent := TheParent;
      if not VisibleFlag then Close;
    end;
  end;

  class procedure TModalFrame.Close;
  begin
    //Frame.Visible := false; //not needed since we destroy the frame to save memory

    //Note: following code is needed, if we call FreeAndNil(Frame) it fails on MacOS-X cause TControl.MouseClick tries to call StartTriggerAnimation(Self, 'Pressed') at the already deleted button (cause its parent+owner frame was deleted by the Click event handler).
    //...should probably make this some utility method like FreeAndNilAsync or FreeAndNilQueued or something
    //message queuing logic based on TStyledControl.KillResourceLink
    {$IFDEF ANDROID} //TODO: not sure why Android needs different treatment (there was mention of RSP-17938)
    TThread.CreateAnonymousThread(
      procedure
      begin
        TThread.Queue(nil,
          procedure
          begin
            FreeAndNil(Frame); //destroy instead of hiding to save memory
          end);
      end).Start;
    {$ELSE} //TODO: maybe also see RSP-27656
    TThread.ForceQueue(nil, //ForceQueue will make sure that even when on main thread we'll queue the message instead of processing immediately
      procedure
      begin
        FreeAndNil(Frame); //destroy instead of hiding to save memory
      end);
    {$ENDIF}
  end;

  {$endregion}

  {$region 'Events'}

  procedure TModalFrame.rectBackgroundClick(Sender: TObject);
  begin
    if CanClose then
      Close;
  end;

  {$endregion}

end.
