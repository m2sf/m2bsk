## M2BSK Modula-2 Bootstrap Kernel Project ##

Welcome to the M2BSK Modula-2 Bootstrap Kernel Project

### Objective ###

The objective of this project is to develop a bootstrap compiler for the revised Modula-2 language described in

[*Modula-2 Revision 2010, Language Report for the Bootstrap Kernel Subset (BSK)*, by Kowarsch and Sutcliffe, July 2020](https://github.com/m2sf/PDFs/blob/master/M2BSK%20Language%20Description.pdf).

*NOTE*: Due to minor edits, the PDF is currently out of sync with the online version below.

### Grammar ###

The grammar of the compiler is in the project repository

https://github.com/m2sf/m2bsk/blob/master/m2bsk-grammar.gll

For a graphical representation of the grammar, see section
[Syntax Diagrams](https://github.com/m2sf/m2bsk/wiki/Language-Specification-(D)-:-Syntax-Diagrams).


### Language Specification ###

An online version of the language specification is here:

https://github.com/m2sf/m2bsk/wiki/Language-Specification


### Scope ###

The compiler supports the Bootstrap Kernel (BSK) _**subset**_ of Modula-2 R10. It does not support earlier PIM or ISO dialects.

For a list of facilities that have been omitted in the subset, see [Omissions](https://github.com/m2sf/m2bsk/wiki/Omissions)


### Targets ###

Two separate bootstrap compiler tracks exist.

* [M2C](https://github.com/m2sf/m2c), a C hosted bootstrap compiler, written in C99, generating C99 source code
* M2BSK, a Modula-2 hosted bootstrap compiler, written in a reduced subset of PIM M2, generating C99 source code

An LLVM backend will be added later (post-bootstrap).


### License ###

Both compilers are licensed under the GNU Lesser General Public License (LGPL), both v.2.1 and v.3.


### Prerequisites ###

Both compilers require a C99 supporting C compiler.

Additionally, M2BSK requires
[MOTTO](https://github.com/trijezdci/MOTTO),
a via-C feature-reduced PIM Modula-2 Compiler that supporst extensible records.

**There are no dependencies on any third party libraries.**

### OS support ###

Both compilers will run on any operating system with target support for the C99 host-compiler.

### Contact ###

If you have questions or would like to contribute to the project, get in touch via

* [Modula2 Telegram group](https://t.me/+hTKSWC2mWoM1OGVl) chat

* [email](mailto:REMOVE+REVERSE.com.gmail@trijezdci) to the project maintainer

### Journal ###

For a discussion of design principles visit the journal at

* [The Knights Of Type](https://knightsoftype.substack.com/)

+++
