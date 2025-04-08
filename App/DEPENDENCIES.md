# Dependencies

+ GetIt Packages (install via GetIt Package Manager, included in Delphi IDE):
    - TFrameStand
    - SVGIconImageList
    - CodeSite Express
    - Boss Experts (optional)

+ Boss Packages (install via Boss Package Manager [use Boss command-line tool and optionally Boss Experts for IDE] - configuration is in boss.json):
    - Zoomicon.Generics
    - Zoomicon.Helpers.RTL
    - Zoomicon.Helpers.FMX (also a design-time package)
    - Zoomicon.Introspection.FMX (also a design-time package) (also wraps Object-Debugger-for-Firemonkey)
    - Zoomicon.Manipulation.FMX (also a design-time package)
    - Zoomicon.Media.FMX (also a design-time package)
    - Zoomicon.ZUI.FMX (also a design-time package)
    - READCOM.Core

Notes:
- at Build/Compiler/Search path/All configurations - All platforms, using relative search paths to respective Boss cache folders (see App/modules folder)
- will need to manually install the BOSS packages that are for design-time too (see modules subfolder that it creates) if BOSS doesn't install them as design packages
