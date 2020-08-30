(*!m2pim*) (* Copyright (c) 2017 Modula-2 Software Foundation. *)

IMPLEMENTATION MODULE Hash; (* requires 16-bit CARDINAL *)

(* General Purpose 32-bit Hash Function *)

IMPORT ISO646;

FROM SYSTEM IMPORT TSIZE;
FROM CardMath IMPORT log2;


CONST
  CardBitwidth = 16;
  HashBitwidth = 32;


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
 * function:  Hash.finalValue( hash )
 * ------------------------------------------------------------------------ *)

PROCEDURE finalValue ( hash : Key ) : Key;

VAR
  pow2max := CARDINAL;
  
BEGIN
  (* Clear highest bit in hash value *)
  pow2max := pow2(CardBitwidth-1);
  IF hash.highBits >= pow2max THEN
    hash.highBits := hash.highBits - pow2max
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
  newHash, hash, pow2max : Key;
  
BEGIN
  index := 0;
  hash := initialValue;
  
  ch := array[index]
  WHILE (ch # ISO646.NUL) AND (index < HIGH(array)) DO
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
  
  (* Clear highest bit in hash value *)
  pow2max := pow2(CardBitwidth-1);
  IF hash.highBits >= pow2max THEN
    hash.highBits := hash.highBits - pow2max
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
      
      (* bit at position CardBitwidth-shiftFactor is pivotal *)
      pivotalBit := CardBitwidth - shiftFactor;
      
      (* remember bits that will be shifted out of lowBits *)
      carryBits := hash.lowBits DIV pivotalBit;
      
      (* shift lowBits *)
      
      (* clear bits including and above pivotal bit to avoid overflow *)
      IF hash.lowBits >= pow2(pivotalBit) THEN
        ClearBitsInclAndAbove(hash.lowBits, pivotalBit)
      END; (* IF *)
      
      (* safely shift lowBits *)
      hash.lowBits := hash.lowBits * pow2(shiftFactor);
      
      (* shift highBits *)
      
      (* clear bits including and above pivotal bit to avoid overflow *)
      IF hash.highBits >= pow2(pivotalBit) THEN
        ClearBitsInclAndAbove(hash.highBits, pivotalBit)
      END; (* IF *)
      
      (* safely shift highBits *)
      hash.highBits := hash.highBits * pow2(shiftFactor);
      
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
      
      (* bit at position CardBitwidth-shiftFactor is pivotal *)
      pivotalBit := CardBitwidth - shiftFactor;

      (* shift highBits *)
      
      (* clear bits including and above pivotal bit to avoid overflow *)
      IF hash.highBits >= pow2(pivotalBit) THEN
        ClearBitsInclAndAbove(hash.highBits, pivotalBit)
      END; (* IF *)
      
      (* safely shift highBits *)
      hash.highBits := hash.highBits * pow2[(shiftFactor MOD CardBitwidth)];
      
  (* shifting by HashBitwidth or more ... *)
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
  IF lowestBitToClear > CardBitwidth-1 THEN
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


BEGIN (* Hash *)
  (* bail out if CARDINAL is not 16-bit wide *)
  IF TSIZE(CARDINAL) # CardBitwidth THEN
    Console.WriteChars("Library Hash requires 16-bit CARDINALs.");
    Console.WriteLn;
    HALT
  END (* IF *)
END Hash.
