@echo off
setlocal

:: Save the original working directory
set "ORIG_DIR=%CD%"

call :RunBossUpdate "App"

pause
exit /b

:: === Subroutine ===
:RunBossUpdate
set "TARGET=%~1"
set "JUNCTION=C:\myTMP"

::-- Note: use a really short path for the JUNCTION

:: Always return to original dir before creating junction
cd /d "%ORIG_DIR%"

echo.
echo Creating junction: %JUNCTION% -^> %TARGET%
mklink /J "%JUNCTION%" "%TARGET%" > nul
if errorlevel 1 (
    echo Failed to create junction for %TARGET%
    exit /b 1
)

cd /d "%JUNCTION%"
echo Running boss update in %JUNCTION%...
boss update

echo Removing .dcp subfolders under modules
for /d /r modules %%d in (.dcp) do if exist "%%d" rd /s /q "%%d"

echo Removing junction: %JUNCTION%
rmdir "%JUNCTION%"

exit /b
