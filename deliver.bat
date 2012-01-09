@echo off
SET SRC=C:\sanlab
SET DEST=C:\Program Files\SANLab-CM
echo Building SANLab-CM
rmdir /S /Q "%DEST%"
mkdir "%DEST%"
cd %SRC%
"C:\Program Files\LispWorks\lispworks-6-0-0-x86-win32.exe" -build deliver.lisp
echo Copying support files
mkdir "%DEST%\Activities"
mkdir "%DEST%\Contents"
mkdir "%DEST%\Distributions"
mkdir "%DEST%\docs"
mkdir "%DEST%\Interactive Routines"
mkdir "%DEST%\Resources"
xcopy /Y /E Activities "%DEST%\Activities"
xcopy /Y /E Contents "%DEST%\Contents"
xcopy /Y /E Distributions "%DEST%\Distributions"
xcopy /Y /E docs "%DEST%\docs"
xcopy /Y /E "Interactive Routines" "%DEST%\Interactive Routines"
xcopy /Y /E Resources "%DEST%\Resources"
copy COPYING "%DEST%\COPYING"
copy COPYING.LESSER "%DEST%\COPYING.LESSER"
copy CURRENT_REVISION "%DEST%\CURRENT_REVISION"
copy properties.conf "%DEST%\properties.conf"
del C:\SANLab-CM-win32.zip
7z a C:\SANLab-CM-win32.zip "%DEST%"