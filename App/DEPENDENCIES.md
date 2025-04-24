# Dependencies

+ GetIt Packages (install via GetIt Package Manager, included in Delphi IDE):
    - TFrameStand
    - SVGIconImageList
    - CodeSite Express
    - Boss Experts (optional)

+ Boss Packages (can install via command-line with Boss Package Manager update command [or place boss.exe in PATH and use Boss Experts from IDE] - configuration is in boss.json):
    - Zoomicon.Media.FMX (also a design-time package)
    - READCOM.Core (also a design-time package)
+ Other Boss Packages (autodownloaded as child dependencies):
    - Zoomicon.Generics
    - Zoomicon.Helpers.RTL
    - Zoomicon.Helpers.FMX (also a design-time package)
    - Zoomicon.Introspection.FMX (also a design-time package)
    - Zoomicon.Manipulation.FMX (also a design-time package)
    - Zoomicon.ZUI.FMX (also a design-time package)
    - Object-Debugger-for-Firemonkey

Notes:
- at Build/Compiler/Search path/All configurations - All platforms, using relative search paths to respective Boss cache folders (see App/modules folder)
- may need to open and right-click / install the BOSS packages that are for design-time too (see modules subfolder that boss creates) if BOSS doesn't install them as design packages
