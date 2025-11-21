@echo off
setlocal

:: Save the original working directory
set "ORIG_DIR=%CD%"

call :RunBossInstall "App"

pause
exit /b

:: === Subroutine ===
:RunBossInstall
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

::-- Note: to change Delphi compiler use:
::-- boss config delphi list
::-- boss config delphi use X -g
::-- where X is the compiler you want from the list shown. The -g makes it remembered globally
::-- in that case make sure old modules folder gets removed, then do boss install

echo Removing old modules folder
rd /s/q modules

echo Running boss install in %JUNCTION%...
boss install

echo Removing .dcp subfolders under modules
for /d /r modules %%d in (.dcp) do if exist "%%d" rd /s /q "%%d"

echo Removing junction: %JUNCTION%
rmdir "%JUNCTION%"

exit /b
