(*!m2pim*) (* Copyright (c) 2017 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE Hash; (* requires 32-bit CARDINAL *)

(* General Purpose 32-bit Hash Function *)

IMPORT ASCII;

FROM SYSTEM IMPORT TSIZE;


(* ---------------------------------------------------------------------------
 * function:  Hash.valueForNextChar( hash, ch )
 * ------------------------------------------------------------------------ *)

PROCEDURE valueForNextChar ( hash : Key; ch : CHAR ) : Key;

BEGIN
  RETURN Key(ORD(ch) + SHL(hash, 6) + SHL(hash, 16) - hash
END valueForNextChar;


(* ---------------------------------------------------------------------------
 * Data table for powers of 2
 * ------------------------------------------------------------------------ *)

VAR
  pow2 : ARRAY [0..31] OF CARDINAL;


(* ---------------------------------------------------------------------------
 * function:  Hash.finalValue( hash )
 * ------------------------------------------------------------------------ *)

PROCEDURE finalValue ( hash : Key ) : Key;

BEGIN
  (* Clear bit 31 *)
  RETURN hash - pow2[31]
END finalValue;


(* ---------------------------------------------------------------------------
 * function:  Hash.valueForArray( array )
 * ------------------------------------------------------------------------ *)

PROCEDURE valueForArray ( VAR (* CONST *) array : ARRAY OF CHAR ) : Key;

VAR
  ch : CHAR;
  hash : Key;
  index : CARDINAL;
  
BEGIN
  index := 0;
  hash := initialValue;
  
  ch := array[index]
  WHILE (ch # ASCII.NUL) AND (index < HIGH(array)) DO
    hash := Key(ORD(ch)) + SHL(hash, 6) + SHL(hash, 16) - hash;
    index := index + 1;
    ch := array[index]
  END; (* WHILE *)
  
  (* Clear bit 31 and return hash *)
  RETURN hash - pow2[31]
END valueForArray;


(* ************************************************************************ *
 * Private Operations                                                       *
 * ************************************************************************ *)

(* ---------------------------------------------------------------------------
 * function: SHL( hash, shiftFactor )
 * ---------------------------------------------------------------------------
 * Returns the value of hash shifted left by shiftFactor.
 * ------------------------------------------------------------------------ *)

PROCEDURE SHL ( hash : Key; shiftFactor : CARDINAL ) : Key;

VAR
  pivotalBit : CARDINAL;
  
BEGIN
  (* shifting by more than 31 produces all zeroes *)
  IF shiftFactor > 31 THEN
    RETURN 0
  END; (* IF *)
  
  (* bit at position 32-shiftFactor is pivotal *)
  pivotalBit := 32 - shiftFactor;
  
  (* clear bits including and above pivotal bit to avoid overflow *)
  IF hash >= pow2[pivotalBit] THEN
    ClearHighBits(hash, pivotalBit)
  END; (* IF *)
  
  (* shift left safely *)
  RETURN hash * pow2[shiftFactor]
END SHL;


(* ---------------------------------------------------------------------------
 * procedure: ClearHighBits( hash, lowestBitToClear )
 * ---------------------------------------------------------------------------
 * Clears all bits including and above bit at position lowestBitToClear.
 * ------------------------------------------------------------------------ *)

PROCEDURE ClearHighBits ( VAR hash : Key; lowestBitToClear : CARDINAL );

VAR
  bitToClear : CARDINAL;
  
BEGIN
  FOR bitToClear := 32 TO lowestBitToClear BY -1 DO
    hash := hash - pow2[bitToClear]
  END (* FOR *)
END ClearHighBits;


(* ---------------------------------------------------------------------------
 * procedure: InitPow2Table
 * ---------------------------------------------------------------------------
 * Initialises data table with powers of 2.
 * ------------------------------------------------------------------------ *)

PROCEDURE InitPow2Table;

BEGIN
  pow2[0] := 1;
  pow2[1] := 2;
  pow2[2] := 4;
  pow2[3] := 8;
  pow2[4] := 16;
  pow2[5] := 32;
  pow2[6] := 64;
  pow2[7] := 128;
  pow2[8] := 256;
  pow2[9] := 512;
  pow2[10] := 1024;
  pow2[11] := 2048;
  pow2[12] := 4096;
  pow2[13] := 8192;
  pow2[14] := 16384;
  pow2[15] := 32768;
  pow2[16] := 65536;
  pow2[17] := 131072;
  pow2[18] := 262144;
  pow2[19] := 524288;
  pow2[20] := 1048576;
  pow2[21] := 2097152;
  pow2[22] := 4194304;
  pow2[23] := 8388608;
  pow2[24] := 16777216;
  pow2[25] := 33554432;
  pow2[26] := 67108864;
  pow2[27] := 134217728;
  pow2[28] := 268435456;
  pow2[29] := 536870912;
  pow2[30] := 1073741824;
  pow2[31] := 2147483648
END InitPow2Table;


BEGIN (* Hash *)
  (* bail out if CARDINAL is not 32-bit wide *)
  IF TSIZE(CARDINAL) # 32 THEN HALT END;
  
  (* Initialise data table *)
  InitPow2Table
END Hash.