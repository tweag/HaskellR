@echo OFF
setlocal ENABLEEXTENSIONS
if "%1"=="R" goto valuer
if "%1"=="H" goto valueh
if "%1"=="HP" goto valuehp
if "%1"=="cygwin" goto valuecyg
goto valueerr

REM R Install Path
:valuer
set KEY_NAME="HKLM\Software\R-core\R32"
set VALUE_NAME="InstallPath"
goto run

REM H Install Path
:valueh
set KEY_NAME="HKLM\Software\Tweag\H"
set VALUE_NAME="InstallPath"
goto run

REM Haskell Platform Install Path
:valuehp
set KEY_NAME=HKLM\Software\Haskell\Haskell Platform\2013.2.0.0
set VALUE_NAME="InstallDir"
goto run

REM Cygwin Install Path
:valuecyg
set KEY_NAME=HKLM\Software\Cygwin\setup
set VALUE_NAME=rootdir
goto run

:run
FOR /F "usebackq skip=2 tokens=1-5" %%A IN (`REG QUERY "%KEY_NAME%" /v "%VALUE_NAME%" 2^>nul`) DO (
	echo %%C %%D %%E
)
goto end

:valueerr
echo "Incorrect value (%1)"

:end