copy Debug\READCOM.App.exe READCOM.App.x64.exe
del READCOM.App.x64.upx.exe
upx -9 -o READCOM.App.x64.upx.exe READCOM.App.x64.exe
@pause
