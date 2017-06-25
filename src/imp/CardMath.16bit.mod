(*!m2pim*) (* Copyright (c) 2017 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE CardMath; (* 16-bit version *)

(* Cardinal Math library *)

IMPORT Console;

FROM SYSTEM IMPORT TSIZE;


CONST
  Bitwidth = 16;


(* --------------------------------------------------------------------------
 * Data table for powers of 2
 * ----------------------------------------------------------------------- *)

VAR
  powerOf2 : ARRAY [0..Bitwidth-1] OF CARDINAL;


(* --------------------------------------------------------------------------
 * function pow2(n)
 * --------------------------------------------------------------------------
 * Returns the power of 2 for argument n
 * ----------------------------------------------------------------------- *)

PROCEDURE pow2 ( n : CARDINAL ) : CARDINAL;

BEGIN
  RETURN powerOf2[n]
END pow2;


(* --------------------------------------------------------------------------
 * function log2(n)
 * --------------------------------------------------------------------------
 * Returns the (truncated) logarithm of 2 for argument n
 * ----------------------------------------------------------------------- *)

PROCEDURE log2 ( n : CARDINAL ) : CARDINAL;

BEGIN
  CASE n OF
    0 : HALT
  | 1 : RETURN 0
  | 2 : RETURN 1
  | 3, 4 : RETURN 2
  | 5..8 : RETURN 3
  | 9..16 : RETURN 4
  | 17..32 : RETURN 5
  | 33..64 : RETURN 6
  | 65..128 : RETURN 7
  | 129..256 : RETURN 8
  | 257..512 : RETURN 9
  | 513..1024 : RETURN 10
  | 1025..2048 : RETURN 11
  | 2049..4096 : RETURN 12
  | 4097..8192 : RETURN 13
  | 8193..16384 : RETURN 14
  | 16385..32768 : RETURN 15
  | 32769..MAX(CARDINAL) : RETURN 16
  END (* CASE *)
END log2;


(* ************************************************************************ *
 * Private Operations                                                       *
 * ************************************************************************ *)

(* --------------------------------------------------------------------------
 * procedure: InitPow2Table
 * --------------------------------------------------------------------------
 * Initialises data table with powers of 2.
 * ----------------------------------------------------------------------- *)

PROCEDURE InitPow2Table;

BEGIN
  powerOf2[0] := 1;
  powerOf2[1] := 2;
  powerOf2[2] := 4;
  powerOf2[3] := 8;
  powerOf2[4] := 16;
  powerOf2[5] := 32;
  powerOf2[6] := 64;
  powerOf2[7] := 128;
  powerOf2[8] := 256;
  powerOf2[9] := 512;
  powerOf2[10] := 1024;
  powerOf2[11] := 2048;
  powerOf2[12] := 4096;
  powerOf2[13] := 8192;
  powerOf2[14] := 16384;
  powerOf2[15] := 32768
END InitPow2Table;


BEGIN (* CardMath *)
  (* bail out if CARDINAL is not 16-bit wide *)
  IF TSIZE(CARDINAL) # Bitwidth THEN
    Console.WriteChars("Library CardMath requires 16-bit CARDINALs.");
    Console.WriteLn;
    HALT
  END; (* IF *)
  
  (* Initialise data table *)
  InitPow2Table
END CardMath.