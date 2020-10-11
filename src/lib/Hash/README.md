### The Hash Library ###

The Hash library provides two modules

* Module `Hash` for 32-bit hash values
* Module `LongHash` for 64-bit hash values

### Memory Models ###

There are three possible memory models employed by classical Modula-2 compilers

* 16-bit `CARDINAL` and 32-bit `LONGINT`, abbreviated 16/32
* 32-bit `CARDINAL` and 32-bit `LONGINT`, abbreviated 32/32
* 32-bit `CARDINAL` and 64-bit `LONGINT`, abbreviated 32/64

### Memory Model Specific Library Versions ###

Depending on the memory model, a specific version of the Hash library must be used

* for the 16/32 memory model, use `Hash.LONGINT32` and `LongHash.CARD64`
* for the 32/32 memory model, use `Hash.CARDINAL32` and `LongHash.CARD64`
* for the 32/64 memory model, use `Hash.CARDINAL32` and `LongHash.LONGINT64`
