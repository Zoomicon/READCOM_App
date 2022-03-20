copy Release\READCOM.App.exe .
del READCOM.App.upx.exe
upx -9 -o READCOM.App.upx.exe READCOM.App.exe
@pause
