@echo off

:-- Main script
call :DeleteSubfolders __history
call :DeleteSubfolders __restore
pause

:-- End of main script
goto :EOF

:-- Subroutine for deleting folders
:DeleteSubfolders
for /d /r %%i in (%1) do (
    if exist "%%i" (
        echo Deleting: %%i
        rmdir /s /q "%%i"
    )
)
goto :EOF