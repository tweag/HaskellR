@ECHO OFF
::---------------------------------------------------------------------
:: Copyright: 2014 (c) EURL Tweag 
:: License:   BSD-2
::
:: This is a run script for H Terminal
:: Usage: H terminal.bat 
SET RRegKeyPath="HKLM\Software\R-core\R"
SET RRegKeyPath64="HKLM\Software\Wow6432Node\R-core\R"
SET HRegKeyPath="HKLM\Software\Tweag\H"
SET HRegKeyPath64="HKLM\Software\Wow6432Node\Tweag\H"
SET CabalPath=%appdata%\cabal\bin

FOR /F "skip=2 tokens=2*" %%A IN ('REG QUERY "%RRegKeyPath%" /v InstallPath 2^>nil') DO SET RRoot=%%B
IF "x%RRoot%x"=="xx" FOR /F "skip=2 tokens=2*" %%A IN ('REG QUERY "%RRegKeyPath64%" /v InstallPath') DO SET RRoot=%%B

FOR /F "skip=2 tokens=2*" %%A IN ('REG QUERY "%HRegKeyPath%" /v InstallPath 2^>nil') DO SET HRoot=%%B
IF "x%HRoot%x"=="xx" FOR /F "skip=2 tokens=2*" %%A IN ('REG QUERY "%HRegKeyPath64%" /v InstallPath') DO SET HRoot=%%B

SET ORIGINAL_PATH=%PATH%
SET Path=%appdata%\cabal\bin;%RRoot%\bin;%RRoot%\bin\i386;%PATH%

:: Check that H is installed
:verify
echo Verifying installation integrity
%CabalPath%\H --version >nil
IF /I %errorlevel%==0 goto run
IF    "x%ATTEMPT%x"=="xx" goto install
goto enderror

:install
SET ATTEMPT=1
echo %HRoot%
"%HRoot%"\utils\install-h.bat
goto verify

:run
ECHO Running H interactive session.. Please wait.
CALL H --version
CALL H --interactive

:: SET RRegKeyPath=
:cleanup
SET PATH=%ORIGINAL_PATH%
echo BYE!
exit

:enderror
SET ATTEMPT=
echo "Unable to run H after reinstall."
PAUSE
exit /B 1
