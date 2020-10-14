## M2BSK Modula-2 Bootstrap Kernel Project ##
Welcome to the M2BSK Modula-2 Bootstrap Kernel Project

### Objective ###

The objective of this project is to develop a bootstrap compiler for the revised Modula-2 language described in

[*Modula-2 Revision 2010, Language Report for the Bootstrap Kernel Subset (BSK)*, by Kowarsch and Sutcliffe, July 2020](https://github.com/m2sf/PDFs/blob/master/M2BSK%20Language%20Description.pdf).

### Grammar ###

The grammar of the compiler is in the project repository

https://github.com/m2sf/m2bsk/blob/master/m2bsk-grammar.gll

For a graphical representation of the grammar, see section
[Syntax Diagrams](https://github.com/m2sf/m2bsk/wiki/Language-Specification-(D)-:-Syntax-Diagrams).

### Language Specification ###

An online version of the language specification is here:

https://github.com/m2sf/m2bsk/wiki/Language-Specification


The authoritative language specification (PDF) is available for download:

https://github.com/m2sf/PDFs/blob/master/M2BSK%20Language%20Description.pdf


### Scope ###

The compiler supports the Bootstrap Kernel (BSK) _**subset**_ of Modula-2 R10. It does not support earlier PIM or ISO dialects.

For a list of facilities that have been omitted in the subset, see [Omissions](https://github.com/m2sf/m2bsk/wiki/Omissions)

### Targets ###

The compiler will generate classic Modula-2 sources that can be compiled with any Modula-2 compiler that meets the prerequisites for compiling M2BSK itself. An LLVM backend will be added later to generate [LLVM IR](http://llvm.org/docs/LangRef.html).

### License ###

M2BSK is licensed under the GNU Lesser General Public License (LGPL) both v.2.1 and v.3.

### Prerequisites ###

M2BSK is written in a subset of the third and fourth editions of Niklaus Wirth's "Programming in Modula-2" that represents an intersection with ISO Modula-2 (IS 10514-1) in order to facilitate compilation with any classic Modula-2 compiler, regardless of dialect and platform.

Nevertheless, the host compiler needs to support **one** of the following memory models:

* 16-bit `CARDINAL` type and 32-bit `LONGINT` type
* 32-bit `CARDINAL` type and 32-bit `LONGINT` type
* 32-bit `CARDINAL` type and 64-bit `LONGINT` type

Furthermore, the following libraries from Wirth's "Programming in Modula-2" are required.

* Storage
* Terminal
* FileSystem

These libraries should be part of any PIM Modula-2 compiler's library. For use with ISO Modula-2 compilers, M2BSK provides shim libraries for adaptation to ISO Modula-2 libraries.

**There are no dependencies on any third party libraries.**

### OS support ###

M2BSK will run on any operating system with target support by the host-compiler.

### Development Languages ###

* M2BSK itself is written in classic Modula-2
* The syntax diagram generator script is written in TCL/TK (not required to build M2BSK)
* Build configuration scripts are written in the prevalent shell language of the hosting platform

### Collaboration ###

If you would like to contribute to the project, please get in touch by email

trijezdci (gmail)

+++
