//Description: READ-COM About dialog
//Author: George Birbilis (http://zoomicon.com)

unit READCOM.Views.AllText;

interface
  uses
    READCOM.App.Globals, //for Globals.SVGIconImageList
    READCOM.App.Models, //for IStoryItem
    System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
    FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
    FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation,
    FMX.Objects, FMX.SVGIconImage, FMX.ImgList, FMX.Layouts, System.Actions,
    FMX.ActnList;

type
  TAllTextFrame = class(TFrame)
    MemoAllText: TMemo;
    rectBackground: TRectangle;
    btnClose: TSpeedButton;
    rectBorder: TRectangle;
    lblInstructions: TLabel;
    btnApply: TSpeedButton;
    procedure btnCloseClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);

  protected
    class var
      Frame: TFrame;
      FStoryItem: IStoryItem;

    class function IsShown: Boolean; static;

  public
    constructor Create(AOwner: TComponent); override;
    class procedure ShowModal(const TheParent: TFmxObject; const AStoryItem: IStoryItem; const VisibleFlag: Boolean = true); //TODO: abstract design into a reusable ModalFrame class (see TWaitFrame too)

    class property Shown: Boolean read IsShown;
  end;

implementation
  uses
    Zoomicon.Helpers.FMX.Forms.ApplicationHelper, //for AppVersion
    READCOM.App.Main,
    READCOM.App.Messages,
    READCOM.App.URLs; //for OpenURLinBrowser

{$R *.fmx}

{$region 'Initialization'}

constructor TAllTextFrame.Create(AOwner: TComponent);
begin
  inherited;

  //...
end;

{$endregion}

class function TAllTextFrame.IsShown: Boolean;
begin
  result := Assigned(Frame); //assuming ShowModal kills the frame when closing
end;


class procedure TAllTextFrame.ShowModal(const TheParent: TFmxObject; const AStoryItem: IStoryItem; const VisibleFlag: Boolean = true);
begin
  if not Assigned(Frame) then //on Creation-Showing
  begin
    Frame := Create(Application); //use Application as the Owner since we reuse the same WaitFrame instance
    //Frame.Align := TAlignLayout.Content; //already set at frame designer (could be a property [Content, Center, Scale etc.])

    if Assigned(AStoryItem) then
    begin
      FStoryItem := AStoryItem;

      with TAllTextFrame(Frame) do
      begin
        var LReadOnly := not AStoryItem.EditMode;
        btnApply.Visible := not LReadOnly;
        if LReadOnly then
          lblInstructions.Text := '';
        with MemoAllText do
        begin
          var LLines := AStoryItem.AllText;
          Lines := LLines; //this does Assign under the hood
          FreeAndNil(LLines); //must free these to not do memory leak
          ReadOnly := LReadOnly;
        end;
      end;
    end;
  end;

  with TAllTextFrame(Frame) do
  begin
    Parent := TheParent;

    //Visible := VisibleFlag;
    if not VisibleFlag then //on Cleanup-Hiding
    begin
      if not MemoAllText.ReadOnly and Assigned(AStoryItem) then //if AllText memo wasn't ReadOnly
        AStoryItem.AllText := MemoAllText.Lines; //Apply text changes

      FreeAndNil(Frame); //destroy instead of hiding to save memory
    end;
  end;
end;

{$region 'Events'}

procedure TAllTextFrame.btnApplyClick(Sender: TObject);
begin
  TAllTextFrame.ShowModal(Parent, FStoryItem, false);
end;

procedure TAllTextFrame.btnCloseClick(Sender: TObject);
begin
  TAllTextFrame.ShowModal(Parent, nil, false);
end;

{$endregion}

end.
