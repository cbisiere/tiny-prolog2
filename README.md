# A Simple Prolog II Compiler-Interpreter 
A simple Prolog II compiler-interpreter written in Pascal

## Overview
I wrote this program as a course assignment, back in 1988, when I was a student at the University of Aix-Marseilles II, pursuing a MSc in Computer Science and Mathematics ("Diplôme d'Études Approfondies en Informatique et Mathématique"). The course, entitled "Prolog II", was taught by the late [Alain Colmerauer](https://en.wikipedia.org/wiki/Alain_Colmerauer).

## Compilation

### Turbo Pascal 3

The program was developed in [Turbo Pascal 3](https://en.wikipedia.org/wiki/Turbo_Pascal#Version_3) (TP3). Turbo Pascal 3.02A is [provided](https://web.archive.org/web/20101124092418/http://edn.embarcadero.com/article/20792) to the Borland community free of charge, as a [zip file] (https://web.archive.org/web/20110815014726/http://altd.embarcadero.com/download/museum/tp302.zip). 

You may use a FreeDOS box to install TP3, compile ``Main.pas``and run the Prolog II interpreter.

### Free Pascal Compiler

The program nicely compiles with [Free Pascal Compiler](https://en.wikipedia.org/wiki/Free_Pascal) (FPC) after a single line change, getting rid of a reference to the TP3-specific predefined file variable ``Kbd``. 

To compile the interpreter with FPC, just type:

```bash
fpc -Mtp -FE. -otprolog2 src/Main.pas
```

## Author

* [Christophe Bisière](https://github.com/cbisiere)

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

