@ECHO OFF

:: Options parsing
IF "%~1"=="/hpath" SET HRoot=%~2
IF "%~3"=="/greet" GOTO greet
GOTO ungreet

:greet
ECHO "This terminal will run a compilation process. This may take a while."
ECHO "Please don't close it until it will be finished."
ECHO "In case if any error occur compilation terminal window will not be closed."
ECHO "And you can copy error messages and logs, otherwise window will be closed automatically."
:ungreet

SET CRegKeyPath="HKLM\Software\Cygwin\setup"
SET CRegKeyPath64="HKLM\Software\Wow6432Node\Cygwin\setup"
SET HRegKeyPath="HKLM\Software\Tweag\H"
SET HRegKeyPath64="HKLM\Software\Wow6432Node\Tweag\H"
SET RRegKeyPath="HKLM\Software\R-core\R"
SET RRegKeyPath64="HKLM\Software\Wow6432Node\R-core\R"
SET CabalPath=%appdata%\cabal\bin

:: Cygwin options

if "x%CRoot%x"=="xx" FOR /F "skip=2 tokens=2*" %%A IN ('REG QUERY "%CRegKeyPath%" /v rootdir 2^>nul') DO SET CRoot=%%B
if "x%CRoot%x"=="xx" FOR /F "skip=2 tokens=2*" %%A IN ('REG QUERY "%CRegKeyPath64%" /v rootdir 2^>nul') DO SET CRoot=%%B

:: H options
if "x%HRoot%x"=="xx" FOR /F "skip=2 tokens=2*" %%A IN ('REG QUERY "%HRegKeyPath%" /v InstallPath 2^>nul') DO SET HRoot=%%B
if "x%HRoot%x"=="xx" FOR /F "skip=2 tokens=2*" %%A IN ('REG QUERY "%HRegKeyPath64%" /v InstallPath 2^>nul') DO SET HRoot=%%B

:: R options
if "x%RRoot%x"=="xx" FOR /F "skip=2 tokens=2*" %%A IN ('REG QUERY "%RRegKeyPath%" /v InstallPath 2^>nul') DO SET RRoot=%%B
if "x%RRoot%x"=="xx" FOR /F "skip=2 tokens=2*" %%A IN ('REG QUERY "%RRegKeyPath64%" /v InstallPath 2^>nul') DO SET RRoot=%%B


echo RRoot==%RRoot%
echo HRoot==%HRoot%
echo CRoot==%CRoot%
:: Run installer
chdir "%CRoot%"\bin
bash --login -i "%HRoot%\utils\install-h.sh"
if %errorlevel%==0 exit
PAUSE
exit /B 123
