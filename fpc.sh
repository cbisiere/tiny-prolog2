#!/usr/bin/env sh
sed 's/TP3/FPC/' src/Main.pas > _main.pas
fpc -g -Fu/usr/local/lib/fpc/3.2.2/units/x86_64-darwin/rtl-console -Mtp -FE. -otprolog2 -Fisrc _main.pas && rm _main.pas
rm *.o
