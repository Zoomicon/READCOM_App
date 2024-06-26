# READ-COM: Reading Communities
[READ-COM](https://www.read-com-eu.uma.es/) Project's Multi-Platform Application

The READ-COM project is about improving the Reading habits at home and at school, so that all the children could have the same opportunities in the future.

Its Reading Communities app is about reading, interacting with, editing / remixing, and authoring shareable Stories.

## Wiki
https://github.com/Zoomicon/READCOM_App/wiki

## Installation
Under continuous development/evolution, download latest release here:
* https://github.com/Zoomicon/READCOM_App/wiki/Installation
* https://github.com/Zoomicon/READCOM_App/releases
(also available: latest internal [debug build](https://github.com/Zoomicon/READCOM_App/tree/master/App/Win32/Debug) for Windows 32-bit)

## HowTo: enable SVG previews for File Add/Open Dialogs on Windows

To have SVG preview at Add/Open file dialogs (if Preview pane is enabled at Windows Explorer), install Microsoft PowerToys from: https://docs.microsoft.com/en-us/windows/powertoys/install

Alternatively try this one which uses the same SVG library as the READ-COM App (however it sometimes freezes Windows Explorer): https://github.com/EtheaDev/SVGShellExtensions/releases

## Testing

The default document shown when app first launches (or with New button) contains a link to the Tortoise and the Hare Aesop fable story (among others) to try.

Alternatively:
* can try downloading manually and opening [that story](https://raw.githubusercontent.com/zoomicon/READCOM_Gallery/master/Gallery/en/3-6/The%20Tortoise%20and%20the%20Hare.readcom) for testing (right click the link and select to Save As a .readcom file - to use that story's URL in URLActions of StoryItems right click the URL and select Copy, then right-click and Paste in the URLAction editbox at the popup dialog of the ActiveStoryItem in the app).
* can download the whole Gallery in a .ZIP file (extract its Gallery subfolder somewhere on your PC) and load that sample story from the "Stories/Aesop Fables" folder, per the instructions found at [https://github.com/Zoomicon/READCOM_Gallery/](https://github.com/Zoomicon/READCOM_Gallery). The Gallery also contains useful media assets of permissive license at "Assets" subfolder to add to your stories

## Screenshots

![READ-COM Application v0.6.0 showing The Tortoise and the Hare story in non-Edit mode](https://user-images.githubusercontent.com/3461504/172428202-baf8aa71-a125-48d4-9521-f211645a9446.png)

![READ-COM Application v0.6.0 showing The Tortoise and the Hare story in Edit mode](https://user-images.githubusercontent.com/3461504/172427727-8aeea65c-b8ee-4e76-a376-fc1a802f8fe8.png)

Older screenshots:

![image](https://user-images.githubusercontent.com/3461504/164836697-5d1b6d1e-e665-446b-a8f9-21fc8bfe8bce.png)

![image](https://user-images.githubusercontent.com/3461504/159692204-c61e2f77-61c6-4afe-860b-5d4e9acf840c.png)

![image](https://user-images.githubusercontent.com/3461504/153725195-3f952633-dbfa-4da1-a3d6-ca280608c6e8.png)

![image](https://user-images.githubusercontent.com/3461504/152587444-315f557f-55d9-453a-94f1-042d7b76e010.png)

![image](https://user-images.githubusercontent.com/3461504/150108636-95dbc253-33bc-46ab-b1cb-aa91d1f7a6fb.png)

## Build from Source code
see https://github.com/Zoomicon/READCOM_App/wiki/Source-code

## Attributions
Icons by:
* https://openmoji.org
* https://oNlineWebFonts.com
(see respective sites for license info)
