@echo off

setlocal

::call "%ProgramFiles(x86)%\Embarcadero\Studio\37.0\bin\rsvars.bat"
call rsvars.bat

for %%f in (*.groupproj) do call :build "%%f"

pause
exit /b

::-------------------------------------------

:build
echo Building %~1...
echo:

msbuild "%~1" /t:Make /p:Config=Debug /p:Platform=Win32 /p:DCC_UseMSBuildExternally=true

echo:

exit /b
