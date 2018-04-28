@echo off
echo going to kill all java VMs
pause
set patH="C:\Program Files (x86)\Java\jdk1.7.0_51\bin\";%path%
PsList|find /I "Java" >NUL && (PsKill java.exe & pause & goto :end)
echo no Java VM to kill
pause

:END
