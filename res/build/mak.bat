@echo off
rcc TypeWise.rc -I"C:\Program Files\Smalltalk MT 5\Support\Include" -32 -w-43
sc "empty.c" -32 -c
link empty,result.dll,,user32+kernel32/noi,,TypeWise.res
copy result.dll ..\TypeWise.dll
