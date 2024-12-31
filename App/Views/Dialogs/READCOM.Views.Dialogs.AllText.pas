//Description: READ-COM Story all-text editing dialog
//Author: George Birbilis (http://zoomicon.com)

unit READCOM.Views.Dialogs.AllText;

interface
  {$region 'Used units'}
  uses
    System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
    System.Actions,
    //
    FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
    FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation,
    FMX.Objects, FMX.SVGIconImage, FMX.ImgList, FMX.Layouts,
    FMX.ActnList,
    //
    Zoomicon.Media.FMX.ModalFrame, //for TModalFrame
    READCOM.Resources.Icons, //for Icons.SVGIconImageList
    READCOM.Models.Stories; //for IStoryItem
  {$endregion}

  type
    TAllTextFrame = class(TModalFrame)
      MemoAllText: TMemo;
      btnClose: TSpeedButton;
      rectBorder: TRectangle;
      lblInstructions: TLabel;
      btnApply: TSpeedButton;
      procedure btnCloseClick(Sender: TObject);
      procedure btnApplyClick(Sender: TObject);

    protected
      FStoryItem: IStoryItem;

    public
      destructor Destroy; override;

      class procedure ShowModal(const TheParent: TFmxObject; const AStoryItem: IStoryItem; const VisibleFlag: Boolean = true);
    end;

implementation
  {$region 'Used units'}
  uses
    READCOM.App.Main,
    READCOM.App.Messages;
  {$endregion}

  {$R *.fmx}

  {$region 'Life-time management'}

  destructor TAllTextFrame.Destroy;
  begin
    FStoryItem := nil; //don't call FreeAndNil since we don't own that (plus it's an interface and does ref counting)
    inherited;
  end;

  {$endregion}

  class procedure TAllTextFrame.ShowModal(const TheParent: TFmxObject; const AStoryItem: IStoryItem; const VisibleFlag: Boolean = true);

    procedure Opening;
    begin
      with TAllTextFrame(Frame) do
      begin
        FStoryItem := AStoryItem;

        if Assigned(AStoryItem) then
        begin
          var LReadOnly := not AStoryItem.EditMode;
          btnApply.Visible := not LReadOnly;
          if LReadOnly then
            lblInstructions.Text := '';
          with MemoAllText do
          begin
            var LLines := AStoryItem.AllText;
            try
              Lines := LLines; //this does Assign under the hood
            finally
              FreeAndNil(LLines); //must free these to not do memory leak
            end;
            ReadOnly := LReadOnly;
          end;
        end;

      end;
    end;

    procedure Closing;
    begin
      with TAllTextFrame(Frame) do
        if not MemoAllText.ReadOnly and Assigned(AStoryItem) then //if AllText memo wasn't ReadOnly
          AStoryItem.AllText := MemoAllText.Lines; //Apply text changes
    end;

  begin
     //TODO: (*) maybe have TModalFrame.ShowModal accept optional callback procs for code to execute at opening/creation and at closing/destruction and call that here passing references to "Opening" and "Closing" inner methods
    if not Assigned(Frame) then //on Creation-Showing
    begin
      Frame := Create(Application); //use Application as the Owner since we reuse the same WaitFrame instance
      //Frame.Align := TAlignLayout.Content; //already set at frame designer (could be a property [Content, Center, Scale etc.])
    end;

    with Frame do
    begin
      Parent := TheParent;
      if VisibleFlag then
        Opening //(*)
      else //on Cleanup-Hiding
      begin
        Closing; //(*)
        Close; //don't just call FreeAndNil(Frame), would fail on other platformns than MSWindows, TModalFrame.Close class method closes the frame and frees later on via the main thread's event queue
      end;
    end;
  end;

  {$region 'Events'}

  procedure TAllTextFrame.btnApplyClick(Sender: TObject);
  begin
    TAllTextFrame.ShowModal(Parent, FStoryItem, false); //close and apply changes
  end;

  procedure TAllTextFrame.btnCloseClick(Sender: TObject);
  begin
    FStoryItem := nil;
    Close; //close ignoring changes
  end;

  {$endregion}

end.
