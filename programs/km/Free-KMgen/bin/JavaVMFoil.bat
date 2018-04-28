@echo off


set patH="C:\Program Files (x86)\Java\jdk1.7.0_51\bin\";%path%

:: hide this cmd window immediately
:: (not useful probably)
:: cmdow @ /hid

:: first procedure argument should be the basepath
:: set basepath=g:\pe-libs\
if not {%1}=={} set basepath=%1
pushd %basepath%

:: second procedure argument should be the ports
set ports=13578 13579
if not {%2}=={} set ports=%2
:: strip double quotes
set ports=%ports:"=%

:: 3rd procedure argument should be a title
set title=""
if not {%3}=={} set title=%3
:: strip double quotes
set title=%title:"=%

:: 4rd procedure argument to kill JVM (y kill , n don't)
if %4==n (goto :java)

:: if Foil server process exists, kill it
:: PsList, an utility from http://www.sysinternals.com/
PsList|find /I "Java" >NUL && (echo Killing Java& goto :KillJava)
goto :java
:killJava
PsKill java.exe
:: sleep needed !
sleep 1

:Java
echo starting Foil RuntimeServer (JVM)

set swtpath=%basepath%swt\
set foilpath=%basepath%foil\
set JGopath=%basepath%jgo\
set foiltitle="%title% Foil Server %ports%"
@echo on
start %foiltitle% /high /min ^
java -cp %foilpath%;%swtpath%swt.jar;%jgopath%JGoSWT.jar ^
-Djava.library.path=%swtpath% ^
com.richhickey.foil.RuntimeServer %ports%
@echo off

:: hide foil server cmd window
cmdow %foiltitle% /hid

:: extrait de xxcopy mailing list
goto :endhint

:XXpbar_wait
:: détection fenetre win_X
cmdow "win_X"
if not errorlevel 1 goto :XXpbar_ren
:: pas trouvé -> attente
ping -n 2 127.0.0.1 > nul
goto :XXpbar_wait

:XXpbar_ren
cmdow "win_X" /ren %2
goto :eof

:endhint
