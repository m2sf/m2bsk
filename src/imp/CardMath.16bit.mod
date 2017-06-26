(*!m2pim*) (* Copyright (c) 2017 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE CardMath; (* 16-bit version *)

(* Cardinal Math library *)

IMPORT Console;

FROM SYSTEM IMPORT TSIZE;


CONST
  Bitwidth = 16;
  MaxDecimalDigits = 5;


(* --------------------------------------------------------------------------
 * Data tables
 * ----------------------------------------------------------------------- *)

VAR
  powerOf2 : ARRAY [0..Bitwidth-1] OF CARDINAL;
  powerOf10 : ARRAY [0..MaxDecimalDigits-1] OF CARDINAL;
  degreeForOctetWidth : ARRAY [1..8] OF CARDINAL;


(* --------------------------------------------------------------------------
 * function pow2(n)
 * --------------------------------------------------------------------------
 * Returns the power of 2 for argument n
 * ----------------------------------------------------------------------- *)

PROCEDURE pow2 ( n : CARDINAL ) : CARDINAL;

BEGIN
  IF n > 15 THEN
    HALT
  END; (* IF *)
  
  RETURN powerOf2[n]
END pow2;


(* --------------------------------------------------------------------------
 * function log2(n)
 * --------------------------------------------------------------------------
 * Returns the integral part of the logarithm of 10 for argument n
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


(* --------------------------------------------------------------------------
 * function pow10(n)
 * --------------------------------------------------------------------------
 * Returns the power of 10 for argument n
 * ----------------------------------------------------------------------- *)

PROCEDURE pow10 ( n : CARDINAL ) : CARDINAL;

BEGIN
  IF n > 4 THEN
    HALT
  END; (* IF *)
  
  RETURN powerOf10[n]
END pow10;


(* --------------------------------------------------------------------------
 * function log10(n)
 * --------------------------------------------------------------------------
 * Returns the integral part of the logarithm of 10 for argument n
 * ----------------------------------------------------------------------- *)

PROCEDURE log10 ( n : CARDINAL ) : CARDINAL;

BEGIN
  CASE n OF
    0 : HALT
  | 1..9 : RETURN 0
  | 10..99 : RETURN 1
  | 100..999 : RETURN 2
  | 1000..9999 : RETURN 3
  | 10000..MAX(CARDINAL) : RETURN 4
  END (* CASE *)
END log10;


(* --------------------------------------------------------------------------
 * function deg10(n)
 * --------------------------------------------------------------------------
 * Returns the degree of a polynomial whose value is the largest unsigned
 * number that can be encoded in base-2 with n octets of 8 bits for x = 10.
 *
 *                   n              n-1                  1            0
 *   f(x) = digit * x  + digit   * x    + ... + digit * x  + digit * x
 *               n            n-1                    1            0
 * ----------------------------------------------------------------------- *)

PROCEDURE deg10 ( n : Card1To8 ) : CARDINAL;

BEGIN  
  RETURN degreeForOctetWidth[n]
END deg10;


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


(* --------------------------------------------------------------------------
 * procedure: InitPow10Table
 * --------------------------------------------------------------------------
 * Initialises data table with powers of 10.
 * ----------------------------------------------------------------------- *)

PROCEDURE InitPow10Table;

BEGIN
  powerOf10[0] := 1;
  powerOf10[1] := 10;
  powerOf10[2] := 100;
  powerOf10[3] := 1000;
  powerOf10[4] := 10000
END InitPow10Table;


(* --------------------------------------------------------------------------
 * procedure: InitDeg10Table
 * --------------------------------------------------------------------------
 * Initialises data table with degrees by octet width.
 * ----------------------------------------------------------------------- *)

PROCEDURE InitDeg10Table;

BEGIN
  degreeForOctetWidth[1] := 2;  (*  8 bits *)
  degreeForOctetWidth[2] := 4;  (* 16 bits *)
  degreeForOctetWidth[3] := 7;  (* 24 bits *)
  degreeForOctetWidth[4] := 9;  (* 32 bits *)
  degreeForOctetWidth[5] := 12; (* 40 bits *)
  degreeForOctetWidth[6] := 14; (* 48 bits *)
  degreeForOctetWidth[7] := 16; (* 56 bits *)
  degreeForOctetWidth[8] := 19  (* 64 bits *)
END InitDeg10Table;


BEGIN (* CardMath *)
  (* bail out if CARDINAL is not 16-bit wide *)
  IF TSIZE(CARDINAL) # Bitwidth THEN
    Console.WriteChars("Library CardMath requires 16-bit CARDINALs.");
    Console.WriteLn;
    HALT
  END; (* IF *)
  
  (* Initialise data tables *)
  InitPow2Table;
  InitPow10Table;
  InitDeg10Table
END CardMath.