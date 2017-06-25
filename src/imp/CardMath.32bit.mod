(*!m2pim*) (* Copyright (c) 2017 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE CardMath; (* 32-bit version *)

(* Cardinal Math library *)

IMPORT Console;

FROM SYSTEM IMPORT TSIZE;


CONST
  Bitwidth = 32;


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
 * Returns the integral part of the logarithm of 2 for argument n
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
  | 32769..65536 : RETURN 16
  | 65537..131072 : RETURN 17
  | 131073..262144 : RETURN 18
  | 262145..524288 : RETURN 19
  | 524289..1048576 : RETURN 20
  | 1048577..2097152 : RETURN 21
  | 2097153..4194304 : RETURN 22
  | 4194305..8388608 : RETURN 23
  | 8388609..16777216 : RETURN 24
  | 16777217..33554432 : RETURN 25
  | 33554433..67108864 : RETURN 26
  | 67108865..134217728 : RETURN 27
  | 134217729..268435456 : RETURN 28
  | 268435457..536870912 : RETURN 29
  | 536870913..1073741824 : RETURN 30
  | 1073741825..2147483648 : RETURN 31
  | 2147483649..MAX(CARDINAL) : RETURN 32
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
  powerOf2[15] := 32768;
  powerOf2[16] := 65536;
  powerOf2[17] := 131072;
  powerOf2[18] := 262144;
  powerOf2[19] := 524288;
  powerOf2[20] := 1048576;
  powerOf2[21] := 2097152;
  powerOf2[22] := 4194304;
  powerOf2[23] := 8388608;
  powerOf2[24] := 16777216;
  powerOf2[25] := 33554432;
  powerOf2[26] := 67108864;
  powerOf2[27] := 134217728;
  powerOf2[28] := 268435456;
  powerOf2[29] := 536870912;
  powerOf2[30] := 1073741824;
  powerOf2[31] := 2147483648
END InitPow2Table;


BEGIN (* CardMath *)
  (* bail out if CARDINAL is not 32-bit wide *)
  IF TSIZE(CARDINAL) # Bitwidth THEN
    Console.WriteChars("Library CardMath requires 32-bit CARDINALs.");
    Console.WriteLn;
    HALT
  END; (* IF *)
  
  (* Initialise data table *)
  InitPow2Table
END CardMath.