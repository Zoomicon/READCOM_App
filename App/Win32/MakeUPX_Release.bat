copy Release\READCOM_App.exe .
del READCOM_App.upx.exe
upx -9 -o READCOM_App.upx.exe READCOM_App.exe
@pause
