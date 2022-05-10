copy Release\READCOM_App.exe READCOM_App.x64.exe
del READCOM_App.x64.upx.exe
upx -9 -o READCOM_App.x64.upx.exe READCOM_App.x64.exe
@pause
