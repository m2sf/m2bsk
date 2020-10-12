### The 64-bit Cardinal Library ###

The 64-bit Cardinal library provides a 64-bit Cardinal type and consists of three modules

* Module `CARD64` provides the type definition, relational and arithmetic operations
* Module `Card64BitOps` provides bitwise operations on the type
* Module `Card64Math` provides extended math operations

### Memory Models ###

There are three possible memory models employed by classical Modula-2 compilers

* 16-bit `CARDINAL` and 32-bit `LONGINT`, abbreviated 16/32
* 32-bit `CARDINAL` and 32-bit `LONGINT`, abbreviated 32/32
* 32-bit `CARDINAL` and 64-bit `LONGINT`, abbreviated 32/64

### Memory Model Specific Library Versions ###

In order to support any given memory model, two separate implementations of the library are provided

* a version based on 32-bit `LONGINT`, to be used for the 16/32 memory model
* a version based on 32-bit `CARDINAL`, to be used for the 32/32 and 32/64 memory models

### Filename Version Nomenclature ###

* Files specific to the 32-bit `LONGINT` versions contain `LONGINT32` in their filename.
* Files specific to the 32-bit `CARDINAL` versions contain `CARDINAL32` in their filename.
