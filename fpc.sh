#!/usr/bin/env sh
sed 's/TP3/FPC/' src/Main.pas > _main.pas
fpc -Mtp -FE. -otprolog2 -Fisrc _main.pas
rm _main.pas
rm *.o
