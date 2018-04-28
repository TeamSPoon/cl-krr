@echo on
@rem set home="c:\Users\Administrator\AppData\Roaming"

set patH="C:\Program Files (x86)\Java\jdk1.7.0_51\bin\";%path%
PsList|find /I "emacs" >NUL || "C:\Program Files\emacs\bin\runemacs.exe"
PsList|find /I "KMgen" >NUL ||  start KMgen.exe
@echo off
sleep 4
