echo OFF

REM Batch file to startup and run WEPSDBViewer
REM It must be run from the root installation directory

REM set JAVA=.\j2re1.4.2\bin\java.exe
REM set JAVA=.\jre1.5.0_01\bin\java.exe
set JAVA=java

REM start it up in the "jar" directory so that it finds
REM all the config, help and crop/op record files
cd Jar

set CLASSPATH="WEPSDBViewer.jar;jh.jar"

%JAVA% -cp %CLASSPATH% ex1.WEPSDBViewer %1%


REM Go back to our original installation directory
REM (Windows 95/98 don't do that automatically for us)
cd ..
