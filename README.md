# READ-COM: Reading Communities
[READ-COM](https://www.read-com-eu.uma.es/) Project's Multi-Platform Application

Design:
https://prezi.com/view/a1tJgWgr4pUiLwyM4eiP/

Under development, can try early releases here:
* https://github.com/Zoomicon/READCOM_App/releases
* https://apps.microsoft.com/store/detail/readcom-reading-communities/9NVM0CHRN7PH

Walk-through video:
https://www.youtube.com/watch?v=0vXp5jUf8cU

## HowTo: enable SVG previews for File Add/Open Dialogs on Windows

To have SVG preview at Add/Open file dialogs (if Preview pane is enabled at Windows Explorer), install Microsoft PowerToys from: https://docs.microsoft.com/en-us/windows/powertoys/install

Alternatively try this one which uses the same SVG library as the READ-COM App (however it sometimes freezes Windows Explorer): https://github.com/EtheaDev/SVGShellExtensions/releases

## Requirements: Source-code Dependencies:
need to install via "Tools/GetIt Package Manager" in Delphi IDE to build the project the (appropriate for your Delphi version) packages:
* [TFrameStand](https://getitnow.embarcadero.com/?q=tframestand) 
* [SVGIconImageList](https://getitnow.embarcadero.com/?q=SVGIconImageList)

## Testing

The default document shown when app first launches (or with New button) contains a link to the Tortoise and the Hare Aesop fable story to try.

Alternatively:
* can try downloading manually and opening [that story](https://raw.githubusercontent.com/zoomicon/READCOM_Gallery/master/Gallery/Stories/Aesop%20Fables/The%20Tortoise%20and%20the%20Hare/The%20Tortoise%20and%20the%20Hare.readcom) for testing (right click the link and select to Save As a .readcom file - to use that story's URL in URLActions of StoryItems right click the URL and select Copy, then right-click and Paste in the URLAction editbox at the popup dialog of the ActiveStoryItem in the app).
* can download the whole Gallery in a .ZIP file (extract its Gallery subfolder somewhere on your PC) and load that sample story from the "Stories/Aesop Fables" folder, per the instructions found at [https://github.com/Zoomicon/READCOM_Gallery/](https://github.com/Zoomicon/READCOM_Gallery). The Gallery also contains useful media assets of permissive license at "Assets" subfolder to add to your stories

## Screenshots

![image](https://user-images.githubusercontent.com/3461504/164836697-5d1b6d1e-e665-446b-a8f9-21fc8bfe8bce.png)

Older screenshots:

![image](https://user-images.githubusercontent.com/3461504/159692204-c61e2f77-61c6-4afe-860b-5d4e9acf840c.png)

![image](https://user-images.githubusercontent.com/3461504/153725195-3f952633-dbfa-4da1-a3d6-ca280608c6e8.png)

![image](https://user-images.githubusercontent.com/3461504/152587444-315f557f-55d9-453a-94f1-042d7b76e010.png)

![image](https://user-images.githubusercontent.com/3461504/150108636-95dbc253-33bc-46ab-b1cb-aa91d1f7a6fb.png)
