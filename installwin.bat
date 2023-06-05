@echo off
rem installwin.bat - MS Windows installer
rem Runs stack install --extra-include-dirs=rinclude --extra-lib-dirs=rbin64
rem where rinclude=rhome/include, rbin64=rhome/bin/x64
rem Assumes R is in PATH (uses 64bit version only)

rem Get R home directory (short form without spaces)
set tempfile=%temp%\rversion.txt
Rscript -e "cat(R.home())" > tempfile
set /p rhome=<tempfile
del tempfile

set rinclude=%rhome%/include
set rbin64=%rhome%/bin/x64

rem Invoke stack
stack install --extra-include-dirs=^"%rinclude%^" --extra-lib-dirs=^"%rbin64%^"
