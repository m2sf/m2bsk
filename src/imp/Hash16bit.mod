(*!m2pim*) (* Copyright (c) 2017 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE Hash; (* requires 16-bit CARDINAL *)

(* General Purpose 32-bit Hash Function *)

IMPORT ASCII;

FROM SYSTEM IMPORT TSIZE;


(* ---------------------------------------------------------------------------
 * function:  Hash.valueForNextChar( hash, ch )
 * ------------------------------------------------------------------------ *)

PROCEDURE valueForNextChar ( hash : Key; ch : CHAR ) : Key;

VAR
  result : Key;
  
BEGIN
  (* hash := ORD(ch) + SHL(hash, 6) + SHL(hash, 16) - hash *)
  result.lowBits := ORD(ch); result.highBits := 0;
  Add(result, SHL(hash, 6));
  Add(result, SHL(hash, 16));
  Sub(result, hash);
  
  RETURN result
END valueForNextChar;


(* ---------------------------------------------------------------------------
 * Data table for powers of 2
 * ------------------------------------------------------------------------ *)

VAR
  pow2 : ARRAY [0..15] OF CARDINAL;


(* ---------------------------------------------------------------------------
 * function:  Hash.finalValue( hash )
 * ------------------------------------------------------------------------ *)

PROCEDURE finalValue ( hash : Key ) : Key;

BEGIN
  (* Clear bit 31 *)
  IF hash.highBits >= pow2[15] THEN
    hash.highBits := hash.highBits - pow2[15]
  END; (* IF *)
  
  RETURN hash
END finalValue;


(* ---------------------------------------------------------------------------
 * function:  Hash.valueForArray( array )
 * ------------------------------------------------------------------------ *)

PROCEDURE valueForArray ( VAR (* CONST *) array : ARRAY OF CHAR ) : Key;

VAR
  ch : CHAR;
  index : CARDINAL;
  newHash, hash : Key;
  
BEGIN
  index := 0;
  hash := initialValue;
  
  ch := array[index]
  WHILE (ch # ASCII.NUL) AND (index < HIGH(array)) DO
    (* hash := ORD(ch) + SHL(hash, 6) + SHL(hash, 16) - hash *)
    newHash.lowBits := ORD(ch); newHash.highBits := 0;
    Add(newHash, SHL(hash, 6);
    Add(newHash, SHL(hash, 16);
    Sub(newHash, hash);
    
    (* prepare for next iteration *)
    index := index + 1;
    ch := array[index];
    hash := newHash
  END; (* WHILE *)
  
  (* Clear bit 31 *)
  IF hash.highBits >= pow2[15] THEN
    hash.highBits := hash.highBits - pow2[15]
  END; (* IF *)
  
  RETURN hash
END valueForArray;


(* ************************************************************************ *
 * Private Operations                                                       *
 * ************************************************************************ *)

(* ---------------------------------------------------------------------------
 * function: SHL( hash, shiftFactor )
 * ---------------------------------------------------------------------------
 * 32-bit Left-Shift. Returns the value of hash shifted left by shiftFactor.
 * ------------------------------------------------------------------------ *)

PROCEDURE SHL ( hash : Key; shiftFactor : CARDINAL ) : Key;

VAR
  pivotalBit, carryBits : CARDINAL;
  
BEGIN
  
  CASE shiftFactor OF
  (* shifting by 0 ... *) 
    0 :
      (* leaves result unchanged *)
      
  (* shifting by 1-15 ... *)
  | 1 .. 15 :
      (* partially moves lowBits into highBits *)
      
      (* bit at position 16-shiftFactor is pivotal *)
      pivotalBit := 16 - shiftFactor;
      
      (* remember bits that will be shifted out of lowBits *)
      carryBits := hash.lowBits DIV pivotalBit;
      
      (* shift lowBits *)
      
      (* clear bits including and above pivotal bit to avoid overflow *)
      IF hash.lowBits >= pow2[pivotalBit] THEN
        ClearBitsInclAndAbove(hash.lowBits, pivotalBit)
      END; (* IF *)
      
      (* safely shift lowBits *)
      hash.lowBits := hash.lowBits * pow2[shiftFactor];
      
      (* shift highBits *)
      
      (* clear bits including and above pivotal bit to avoid overflow *)
      IF hash.highBits >= pow2[pivotalBit] THEN
        ClearBitsInclAndAbove(hash.highBits, pivotalBit)
      END; (* IF *)
      
      (* safely shift highBits *)
      hash.highBits := hash.highBits * pow2[shiftFactor];
      
      (* add the carry bits shifted out of lowBits *)
      hash.highBits := hash.highBits + carryBits
      
  (* shifting by 16 ... *)
  | 16 :
      (* moves all lowBits into highBits and clears lowBits *)
      hash.highBits := hash.lowBits;
      hash.lowBits := 0
  
  (* shifting by 17-31 ... *)
  | 17 .. 31 :
      (* moves lowBits into highBits, clears lowBits, shifts highBits *)
      
      hash.highBits := hash.lowBits;
      hash.lowBits := 0;
      
      (* bit at position 32-shiftFactor is pivotal *)
      pivotalBit := 32 - shiftFactor;

      (* shift highBits *)
      
      (* clear bits including and above pivotal bit to avoid overflow *)
      IF hash.highBits >= pow2[pivotalBit] THEN
        ClearBitsInclAndAbove(hash.highBits, pivotalBit)
      END; (* IF *)
      
      (* safely shift highBits *)
      hash.highBits := hash.highBits * pow2[shiftFactor];
      
  (* shifting by more than 31 ... *)
  ELSE
    (* produces all zeroes *)
    hash.lowBits := 0;
    hash.highBits := 0
  END; (* CASE *)
  
  RETURN hash
END SHL;


(* ---------------------------------------------------------------------------
 * procedure: ClearBitsInclAndAbove( value, lowestBitToClear )
 * ---------------------------------------------------------------------------
 * Clears all bits including and above bit at position lowestBitToClear.
 * ------------------------------------------------------------------------ *)

PROCEDURE ClearBitsInclAndAbove
  ( VAR value : CARDINAL; lowestBitToClear : CARDINAL );

VAR
  mask, bitToClear : CARDINAL;
  
BEGIN
  (* no point clearing bits we don't have *)
  IF lowestBitToClear > 15 THEN
    RETURN
  END; (* IF *)
  
  (* clearing from bit 0 produces all zeroes *)
  IF lowestBitToClear = 0 THEN
    value := 0;
    RETURN
  END; (* IF *)
  
  (* shift lower bits out to the right *)
  mask := value DIV (lowestBitToClear + 1);
  
  (* shift them back, thereby clearing the low bits, obtaining a mask *)
  mask := mask * pow2[lowestBitToClear + 1];
  
  (* subtract the mask, thereby clearing the high bits *)
  value := value - mask
END ClearBitsInclAndAbove;


(* ---------------------------------------------------------------------------
 * procedure: Add(left, right)
 * ---------------------------------------------------------------------------
 * 32-bit Addition. Adds right to left, passes the result in left.
 * ------------------------------------------------------------------------ *)

PROCEDURE Add ( VAR left : Key; right : Key );

VAR
  lowBitsOverflow, highBitsOverflow : BOOLEAN;
  
BEGIN
  (* determine if lowBits addition causes overflow *)
  lowBitsOverflow :=
    (right.lowBits > 0) AND (left.lowBits > (65536 - right.lowBits));
  
  IF lowBitsOverflow THEN
    (* clear highest bit in left.lowBits, if set *)
    IF left.lowBits >= 32768 THEN
      left.lowBits := left.lowBits - 32768
    END; (* IF *)
    
    (* clear highest bit in right.lowBits, if set *)
    IF right.lowBits >= 32768 THEN
      right.lowBits := right.lowBits - 32768
    END; (* IF *)
  END; (* IF *)
    
  (* add the two lowBits *)
  left.lowBits := left.lowBits + right.lowBits;
  
   (* determine if highBits addition causes overflow *)
  highBitsOverflow :=
    (right.highBits > 0) AND (left.highBits > (65536 - right.highBits));
  
  IF highBitsOverflow THEN
    (* clear highest bit in left.highBits, if set *)
    IF left.highBits >= 32768 THEN
      left.highBits := left.highBits - 32768
    END; (* IF *)
    
    (* clear highest bit in right.highBits, if set *)
    IF right.highBits >= 32768 THEN
      right.highBits := right.highBits - 32768
    END; (* IF *)
  END; (* IF *)
    
  (* add the two highBits with carry from lowBits addition *)
  left.highBits := left.highBits + right.highBits + ORD(lowBitsOverflow)
END Add;


(* ---------------------------------------------------------------------------
 * procedure: Sub(left, right)
 * ---------------------------------------------------------------------------
 * 32-bit Subtraction. Subtracts right from left, passes the result in left.
 * ------------------------------------------------------------------------ *)

PROCEDURE Sub ( VAR left : Key; right : Key );

VAR
  complement : Key;
  
BEGIN
  (* calculate the two's complement of the subtrahend *)
  complement.lowBits := 65535 - right.lowBits + 1;
  complement.highBits := 65535 - right.highBits + 1;
  
  (* add the complement to the minuend *)
  Add(left, complement)
END Sub;


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
END InitPow2Table;


BEGIN (* Hash *)
  (* bail out if CARDINAL is not 32-bit wide *)
  IF TSIZE(CARDINAL) # 16 THEN HALT END;
  
  (* Initialise data table *)
  InitPow2Table
END Hash.