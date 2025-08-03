unit READCOM.App.EventOverrides; //in a custom app could rename this to XX.App.Events

interface
  uses READCOM.Views.StoryForm; //for TStoryForm, TStory

  type
    TEventHandlers = class
      procedure StoryFormReady(const StoryForm: TStoryForm);
      procedure StoryLoaded(const Story: TStory);
    end;

  var
    EventHandlers: TEventHandlers;

implementation
  uses
    System.SysUtils, //for FreeAndNil
    FMX.Forms; //for Application

  procedure TEventHandlers.StoryFormReady(const StoryForm: TStoryForm);
  begin
    //Application.MainForm := StoryForm; //do this since it may have not been done yet and code we call may expect it to have been done

    (* //NOT USES, SHOWING MENU AND NAVIGATION BUTTONS BY DEFAULT
    //Untoggle actionMenu / hide layoutButtons by default (assuming content has its own navigation StoryItems with UrlAction set)
    StoryForm.HUD.actionMenu.Checked := false; //don't just do StoryForm.HUD.layoutButtons.Visible := false since that would make the menu toggle button unsync
    StoryForm.HUD.layoutButtons.Visible := false;
    *)

    //Use fullscreen by default
    {$IF Defined(Android)} //FullScreen still has issues on Windows (e.g. color popups don't show), only do on Android
    //StoryForm.HUD.Fullscreen := true; //TODO: doing this here on Android seems to result in blank screen
    {$ENDIF}
  end;

  procedure TEventHandlers.StoryLoaded(const Story: TStory);
  begin
    //add logic here to execute everytime a Story is loaded (including the default story)
  end;

initialization
  EventHandlers := TEventHandlers.Create;

finalization
  FreeAndNil(EventHandlers);

end.
