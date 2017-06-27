(*!m2iso*) (* Copyright (c) 2017 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE Terminal; (* ISO Modula-2 only *)

(* Shim Library to adapt ISO Modula-2's STextIO to PIM Modula-2's Terminal *)

IMPORT STextIO; (* in ISO Modula-2's standard library *)


(* ---------------------------------------------------------------------------
 * procedure Read(ch)
 * ---------------------------------------------------------------------------
 * Blocking read operation. Reads a character from standard input.
 * ------------------------------------------------------------------------ *)

PROCEDURE Read ( VAR ch : CHAR );

BEGIN
  STextIO.ReadChar(ch)
END Read;


(* ---------------------------------------------------------------------------
 * procedure Write(ch)
 * ---------------------------------------------------------------------------
 * Writes the given character to standard output.
 * ------------------------------------------------------------------------ *)

PROCEDURE Write ( ch : CHAR );

BEGIN
  STextIO.WriteChar(ch)
END 


(* ---------------------------------------------------------------------------
 * procedure WriteString(array)
 * ---------------------------------------------------------------------------
 * Writes the given character array to standard output.
 * ------------------------------------------------------------------------ *)

PROCEDURE WriteString ( VAR (* CONST *) array : ARRAY OF CHAR );

BEGIN
  STextIO.WriteString(array)
END WriteString;


(* ---------------------------------------------------------------------------
 * procedure WriteLn
 * ---------------------------------------------------------------------------
 * Writes newline to standard output.
 * ------------------------------------------------------------------------ *)

PROCEDURE WriteLn;

BEGIN
  STextIO.WriteLn
END WriteLn;


END Terminal.