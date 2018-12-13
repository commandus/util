@echo off
for %%v in (%path%) do if exist %%v\BRCC32.exe goto brcc32_path
if exist %1\BRCC32.EXE goto brcc32_ok
goto no_brcc32

:brcc32_path
BRCC32 gif.rc
goto exit

:brcc32_ok
%1\BRCC32 gif.rc
goto exit

:no_brcc32
pause
echo •Error:
echo The Borland Resource Compiler BRCC32.EXE where
echo not found in your path.
echo  
echo You can either modify your path to include the
echo directory where BRCC32.EXE is located or specify
echo the path to BRCC32.EXE as a parameter to this
echo batch file like this:
echo   build_resource "c:\program files\borland\delphi 3\bin"
pause

:exit
