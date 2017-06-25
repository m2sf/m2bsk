(*!m2pim*) (* Copyright (c) 2017 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE Hash; (* requires 32-bit CARDINAL *)

(* General Purpose 32-bit Hash Function *)

IMPORT ASCII;

FROM SYSTEM IMPORT TSIZE;
FROM CardMath IMPORT log2;


CONST
  CardBitwidth = 32;
  HashBitwidth = 32;


(* ---------------------------------------------------------------------------
 * function:  Hash.valueForNextChar( hash, ch )
 * ------------------------------------------------------------------------ *)

PROCEDURE valueForNextChar ( hash : Key; ch : CHAR ) : Key;

BEGIN
  RETURN Key(ORD(ch) + SHL(hash, 6) + SHL(hash, 16) - hash
END valueForNextChar;


(* ---------------------------------------------------------------------------
 * function:  Hash.finalValue( hash )
 * ------------------------------------------------------------------------ *)

PROCEDURE finalValue ( hash : Key ) : Key;

VAR
  pow2max : CARDINAL;
  
BEGIN
  (* Clear highest bit in hash value *)
  pow2max := pow2(HashBitwidth-1);
  IF hash >= pow2max THEN
    hash := hash - pow2max
  END; (* IF *)
  
  RETURN hash
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
  
  (* Clear highest bit in hash value *)
  pow2max := pow2(HashBitwidth-1);
  IF hash >= pow2max THEN
    hash := hash - pow2max
  END; (* IF *)
  
  RETURN hash
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
  (* shifting by HashBitwidth and more produces all zeroes *)
  IF shiftFactor > HashBitwidth-1 THEN
    RETURN 0
  END; (* IF *)
  
  (* bit at position CardBitwidth-shiftFactor is pivotal *)
  pivotalBit := CardBitwidth - shiftFactor;
  
  (* clear bits including and above pivotal bit to avoid overflow *)
  IF hash >= pow2(pivotalBit) THEN
    ClearBitsInclAndAbove(hash, pivotalBit)
  END; (* IF *)
  
  (* shift left safely *)
  RETURN hash * pow2(shiftFactor)
END SHL;


(* ---------------------------------------------------------------------------
 * procedure: ClearBitsInclAndAbove( value, lowestBitToClear )
 * ---------------------------------------------------------------------------
 * Clears all bits including and above bit at position lowestBitToClear.
 * ------------------------------------------------------------------------ *)

PROCEDURE ClearBitsInclAndAbove
  ( VAR value : CARDINAL; lowestBitToClear : CARDINAL );

VAR
  mask : Key;
  bitToClear : CARDINAL;
  
BEGIN
  (* shift lower bits out to the right *)
  mask := hash DIV Key(lowestBitToClear+1);
  
  (* shift them back, thereby clearing the low bits *)
  mask := mask * Key(pow2(lowestBitToClear+1));
  
  (* subtract the mask, thereby clearing the high bits *)
  hash := hash - mask;
END ClearBitsInclAndAbove;


BEGIN (* Hash *)
  (* bail out if CARDINAL is not 32-bit wide *)
  IF TSIZE(CARDINAL) # CardBitwidth THEN
    Console.WriteChars("Library Hash requires 32-bit CARDINALs.");
    Console.WriteLn;
    HALT
  END (* IF *)
END Hash.