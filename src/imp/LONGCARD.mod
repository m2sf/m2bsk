(*!m2pim*) (* Copyright (c) 2017 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE LONGCARD;

(* Long Cardinal Type *)


CONST
  HalfBitwidth = Bitwidth DIV 2;
  HalfMaxCardPlusOne = (MAX(CARDINAL) DIV 2) + 1; (* 2^(Bitwidth-1) + 1 *)


(* --------------------------------------------------------------------------
 * function fromCard(n)
 * --------------------------------------------------------------------------
 * Returns the LONGCARD value of CARDINAL n.
 * ----------------------------------------------------------------------- *)

PROCEDURE fromCard ( n : CARDINAL ) : LONGCARD;

VAR
  longCard : LONGCARD;
  
BEGIN
  longCard.highBits := 0;
  longCard.lowBits := n;
  RETURN longCard
END fromCard;


(* --------------------------------------------------------------------------
 * function fromInt(i)
 * --------------------------------------------------------------------------
 * Returns the LONGCARD value of non-negative INTEGER i.
 * ----------------------------------------------------------------------- *)

PROCEDURE fromInt ( i : Int0ToMaxInt ) : LONGCARD;

VAR
  longCard : LONGCARD;
  
BEGIN
  longCard.highBits := 0;
  longCard.lowBits := VAL(CARDINAL, i);
  RETURN longCard
END fromInt;


(* --------------------------------------------------------------------------
 * function fromLongInt(i)
 * --------------------------------------------------------------------------
 * Returns the LONGCARD value of non-negative LONGINT i.
 * ----------------------------------------------------------------------- *)

PROCEDURE fromLongInt ( i : LongInt0ToMaxLongInt ) : LONGCARD;

VAR
  longCard : LONGCARD;
  mask, longIntHighBits : LONGINT;

BEGIN  
  longIntHighBits := i DIV HalfBitwidth;
  mask := longIntHighBits * HalfBitwidth;
  longCard.highBits := VAL(CARDINAL, longIntHighBits);
  longCard.lowBits := VAL(CARDINAL, i - mask);
  RETURN longCard
END fromLongInt;


(* --------------------------------------------------------------------------
 * function max(n)
 * --------------------------------------------------------------------------
 * Returns the largest value of type LONGCARD.
 * ----------------------------------------------------------------------- *)

PROCEDURE max ( ) : LONGCARD;

VAR
  longCard : LONGCARD;
  
BEGIN
  longCard.highBits := MAX(CARDINAL);
  longCard.lowBits := MAX(CARDINAL);
  RETURN longCard
END max;


(* --------------------------------------------------------------------------
 * function min(n)
 * --------------------------------------------------------------------------
 * Returns the smallest value of type LONGCARD.
 * ----------------------------------------------------------------------- *)

PROCEDURE min ( ) : LONGCARD;

VAR
  longCard : LONGCARD;
  
BEGIN
  longCard.highBits := 0;
  longCard.lowBits := 0;
  RETURN longCard
END min;


(* --------------------------------------------------------------------------
 * function odd(n)
 * --------------------------------------------------------------------------
 * Returns TRUE if n is odd, otherwise FALSE.
 * ----------------------------------------------------------------------- *)

PROCEDURE odd ( n : LONGCARD ) : BOOLEAN;

BEGIN
  RETURN ODD(n.lowBits)
END odd;


(* --------------------------------------------------------------------------
 * function eq(n, m)
 * --------------------------------------------------------------------------
 * Returns TRUE if n = m, otherwise FALSE.
 * ----------------------------------------------------------------------- *)

PROCEDURE eq ( n, m : LONGCARD ) : BOOLEAN;

BEGIN
  RETURN (n.highBits = m.highBits) AND (n.lowBits = m.lowBits)
END eq;


(* --------------------------------------------------------------------------
 * function neq(n, m)
 * --------------------------------------------------------------------------
 * Returns TRUE if n # m, otherwise FALSE.
 * ----------------------------------------------------------------------- *)

PROCEDURE neq ( n, m : LONGCARD ) : BOOLEAN;

BEGIN
  RETURN (n.highBits # m.highBits) OR (n.lowBits # m.lowBits)
END neq;


(* --------------------------------------------------------------------------
 * function gt(n, m)
 * --------------------------------------------------------------------------
 * Returns TRUE if n > m, otherwise FALSE.
 * ----------------------------------------------------------------------- *)

PROCEDURE gt ( n, m : LONGCARD ) : BOOLEAN;

BEGIN
  IF (n.highBits = m.highBits) THEN
    RETURN n.lowBits > m.lowBits
  ELSE
    RETURN n.highBits > m.highBits
  END (* IF *)
END gt;


(* --------------------------------------------------------------------------
 * function gteq(n, m)
 * --------------------------------------------------------------------------
 * Returns TRUE if n >= m, otherwise FALSE.
 * ----------------------------------------------------------------------- *)

PROCEDURE gteq ( n, m : LONGCARD ) : BOOLEAN;

BEGIN
  RETURN eq(n, m) OR gt(n, m)
END gteq;


(* --------------------------------------------------------------------------
 * function lt(n, m)
 * --------------------------------------------------------------------------
 * Returns TRUE if n < m, otherwise FALSE.
 * ----------------------------------------------------------------------- *)

PROCEDURE lt ( n, m : LONGCARD ) : BOOLEAN;

BEGIN
  IF (n.highBits = m.highBits) THEN
    RETURN n.lowBits < m.lowBits
  ELSE
    RETURN n.highBits < m.highBits
  END (* IF *)
END lt;


(* --------------------------------------------------------------------------
 * function lteq(n, m)
 * --------------------------------------------------------------------------
 * Returns TRUE if n <= m, otherwise FALSE.
 * ----------------------------------------------------------------------- *)

PROCEDURE lteq ( n, m : LONGCARD ) : BOOLEAN;

BEGIN
  RETURN eq(n, m) OR lt(n, m)
END lteq;


(* --------------------------------------------------------------------------
 * function sum(n, m)
 * --------------------------------------------------------------------------
 * Returns the value of n + m.
 * ----------------------------------------------------------------------- *)

PROCEDURE sum ( n, m : LONGCARD ) : LONGCARD;

VAR
  overflow : CARDINAL;
  
BEGIN
  ADD(n, m, overflow);
  
  IF overflow > 0 THEN HALT END;
  
  RETURN n
END sum;


(* --------------------------------------------------------------------------
 * function diff(n, m)
 * --------------------------------------------------------------------------
 * Returns the value of n - m.
 * ----------------------------------------------------------------------- *)

PROCEDURE diff ( n, m : LONGCARD ) : LONGCARD;

VAR
  complement : LONGCARD;
  
BEGIN
  (* calculate the two's complement of the subtrahend *)
  complement.lowBits := MAX(CARDINAL) - m.lowBits + 1;
  complement.highBits := MAX(CARDINAL) - m.highBits + 1;
  
  (* add the complement to the minuend *)
  RETURN sum(n, complement)
END diff;


(* --------------------------------------------------------------------------
 * function mul(n, m)
 * --------------------------------------------------------------------------
 * Returns the value of n * m.
 * ----------------------------------------------------------------------- *)

PROCEDURE mul ( n, m : LONGCARD ) : LONGCARD;

VAR
  prod : LONGCARD;
  bitIndex : CARDINAL;
  
BEGIN
  FOR bitIndex := 0 TO Bitwidth DO
    (* test LSB of m and add n to prod if set *)
    IF odd(m) THEN
      prod := sum(prod, n)
    END; (* IF *)
    
    (* shift n left by one *)
    SHL(n, 1);
    
    (* shift m right by one *)
    SHR(m, 1)
  END; (* FOR *)
  
  RETURN prod
END mul;


(* --------------------------------------------------------------------------
 * function div(n, m)
 * --------------------------------------------------------------------------
 * Returns the value of n DIV m.
 * ----------------------------------------------------------------------- *)

PROCEDURE div ( n, m : LONGCARD ) : LONGCARD;

BEGIN
  (* TO DO *)
END div;


(* --------------------------------------------------------------------------
 * function mod(n, m)
 * --------------------------------------------------------------------------
 * Returns the value of n MOD m.
 * ----------------------------------------------------------------------- *)

PROCEDURE mod ( n, m : LONGCARD ) : LONGCARD;

BEGIN
  (* TO DO *)
END mod;


(* ---------------------------------------------------------------------------
 * function shl( n, shiftFactor )
 * ---------------------------------------------------------------------------
 * Returns the value of n shifted left by shiftFactor.
 * ------------------------------------------------------------------------ *)

PROCEDURE shl ( n : LONGCARD; shiftFactor : CARDINAL ) : LONGCARD;

VAR
  result : LONGCARD;
  
BEGIN
  result := n;
  SHL(result, shiftFactor);
  RETURN result
END shl;


(* ---------------------------------------------------------------------------
 * function shr( n, shiftFactor )
 * ---------------------------------------------------------------------------
 * Returns the value of n shifted right by shiftFactor.
 * ------------------------------------------------------------------------ *)

PROCEDURE shr ( n : LONGCARD; shiftFactor : CARDINAL ) : LONGCARD;

VAR
  result : LONGCARD;
  
BEGIN
  result := n;
  SHR(result, shiftFactor);
  RETURN result
END shr;


(* --------------------------------------------------------------------------
 * procedure ADD(n, m, carry)
 * --------------------------------------------------------------------------
 * Adds m to n. If overflow occurs, one is passed in carry, else zero.
 * ----------------------------------------------------------------------- *)

PROCEDURE ADD ( VAR n : LONGCARD; m : LONGCARD; VAR carry : CARDINAL );

VAR
  lowBitsOverflow, highBitsOverflow1, highBitsOverflow2 : BOOLEAN;
  
BEGIN
  (* determine if lowBits addition causes overflow *)
  lowBitsOverflow := addOverflows(n.lowBits, m.lowBits);
  
  IF lowBitsOverflow THEN
    (* clear highest bit in n.lowBits, if set *)
    IF n.lowBits >= HalfMaxCardPlusOne THEN
      n.lowBits := n.lowBits - HalfMaxCardPlusOne
    END; (* IF *)
    
    (* clear highest bit in m.lowBits, if set *)
    IF m.lowBits >= HalfMaxCardPlusOne THEN
      m.lowBits := m.lowBits - HalfMaxCardPlusOne
    END; (* IF *)
  END; (* IF *)
    
  (* add the two lowBits *)
  n.lowBits := n.lowBits + m.lowBits;
    
  (* determine if highBits addition causes overflow *)
  highBitsOverflow1 := addOverflows(n.highBits, m.highBits);
  
  IF highBitsOverflow1 THEN
    (* clear highest bit in n.highBits, if set *)
    IF n.highBits >= HalfMaxCardPlusOne THEN
      n.highBits := n.highBits - HalfMaxCardPlusOne
    END; (* IF *)
    
    (* clear highest bit in m.highBits, if set *)
    IF m.highBits >= HalfMaxCardPlusOne THEN
      m.highBits := m.highBits - HalfMaxCardPlusOne
    END; (* IF *)
  END; (* IF *)
  
  (* add the two highBits *)
  n.highBits := n.highBits + m.highBits;
  
  (* determine if adding carry from lowBits addition causes overflow *)
  highBitsOverflow2 := addOverflows(n.highBits, ORD(lowBitsOverflow));
  
  IF highBitsOverflow2 THEN
    (* clear highest bit in n.highBits, if set *)
    IF n.highBits >= HalfMaxCardPlusOne THEN
      n.highBits := n.highBits - HalfMaxCardPlusOne
    END (* IF *)
  END; (* IF *)
  
  (* add carry from lowBits addition to n.highBits *)
  n.highBits := n.highBits + ORD(lowBitsOverflow);

  (* set carry if overflow occurred *)
  carry := ORD(highBitsOverflow1 OR highBitsOverflow2);
END ADD;


(* ---------------------------------------------------------------------------
 * procedure SHL(n, shiftFactor)
 * ---------------------------------------------------------------------------
 * Shifts n left by shiftFactor.
 * ------------------------------------------------------------------------ *)

PROCEDURE SHL ( VAR n : LONGCARD; shiftFactor : CARDINAL );

VAR
  pivotalBit, carryBits : CARDINAL;
  
BEGIN
  
  (* shifting by 0 *) 
  IF shiftFactor = 0 THEN
    (* leaves n unchanged *)
    RETURN
    
  (* shifting by Bitwidth/2 *)
  ELSIF shiftFactor = HalfBitwidth THEN
    (* moves all lowBits into highBits and clears lowBits *)
    n.highBits := n.lowBits;
    n.lowBits := 0;
    RETURN
      
  (* shifting by 1 to Bitwidth/2 *)
  ELSIF (shiftFactor >= 1) AND (shiftFactor < HalfBitwidth) THEN
    (* partially moves lowBits into highBits *)
    
    (* bit at position HalfBitwidth - shiftFactor is pivotal *)
    pivotalBit := HalfBitwidth - shiftFactor;
    
    (* remember bits that will be shifted out of lowBits *)
    carryBits := n.lowBits DIV pivotalBit;
    
    (* shift lowBits *)
    
    (* clear bits including and above pivotal bit to avoid overflow *)
    IF n.lowBits >= pow2(pivotalBit) THEN
      ClearBitsInclAndAbove(n.lowBits, pivotalBit)
    END; (* IF *)
    
    (* safely shift lowBits *)
    n.lowBits := n.lowBits * pow2(shiftFactor);
    
    (* shift highBits *)
    
    (* clear bits including and above pivotal bit to avoid overflow *)
    IF n.highBits >= pow2(pivotalBit) THEN
      ClearBitsInclAndAbove(n.highBits, pivotalBit)
    END; (* IF *)
    
    (* safely shift highBits *)
    n.highBits := n.highBits * pow2(shiftFactor);
    
    (* add the carry bits shifted out of lowBits *)
    n.highBits := n.highBits + carryBits;
    RETURN
    
  (* shifting by Bitwidth/2 to Bitwidth - 1 *)
  ELSIF (shiftFactor > HalfBitwidth) AND (shiftFactor < Bitwidth) THEN
    (* moves lowBits into highBits, clears lowBits, shifts highBits *)
    
    n.highBits := n.lowBits;
    n.lowBits := 0;
    
    (* bit at position HalfBitwidth - shiftFactor is pivotal *)
    pivotalBit := HalfBitwidth - shiftFactor;
    
    (* shift highBits *)
    
    (* clear bits including and above pivotal bit to avoid overflow *)
    IF n.highBits >= pow2(pivotalBit) THEN
      ClearBitsInclAndAbove(n.highBits, pivotalBit)
    END; (* IF *)
    
    (* safely shift highBits *)
    n.highBits := n.highBits * pow2((shiftFactor MOD HalfBitwidth));
    RETURN
    
  (* shifting by Bitwidth or more *)
  ELSE
    (* produces all zeroes *)
    n.lowBits := 0;
    n.highBits := 0;
    RETURN
  END (* IF *)
END SHL;


(* ---------------------------------------------------------------------------
 * procedure SHR(n, shiftFactor)
 * ---------------------------------------------------------------------------
 * Shifts n right by shiftFactor.
 * ------------------------------------------------------------------------ *)

PROCEDURE SHR ( VAR n : LONGCARD; shiftFactor : CARDINAL );

VAR
  pivotalBit, carryBits : CARDINAL;
  
BEGIN
  (* shifting by 0 *) 
  IF shiftFactor = 0 THEN
    (* leaves n unchanged *)
    RETURN
    
  (* shifting by Bitwidth/2 *)
  ELSIF shiftFactor = HalfBitwidth THEN
    (* moves all highBits into lowBits and clears highBits *)
    n.lowBits := n.highBits;
    n.highBits := 0;
    RETURN
    
  (* shifting by 1 to Bitwidth/2 *)
  ELSIF (shiftFactor >= 1) AND (shiftFactor < HalfBitwidth) THEN
    (* partially moves highBits into lowBits *)
    
    (* bit at position HalfBitwidth - shiftFactor is pivotal *)
    pivotalBit := HalfBitwidth - shiftFactor;
    
    (* remember bits that will be shifted out of highBits *)
    carryBits := n.highBits * pow2(pivotalBit);
    
    (* shift lowBits *)
    n.lowBits := n.lowBits DIV pow2(shiftFactor);
    
    (* shift highBits *)
    n.highBits := n.highBits DIV pow2(shiftFactor);
    
    (* add the bits shifted out of highBits to lowBits *)
    n.lowBits := n.lowBits + carryBits;
    RETURN
    
  (* shifting by Bitwidth/2 to Bitwidth - 1 *)
  ELSIF (shiftFactor > HalfBitwidth) AND (shiftFactor < Bitwidth) THEN
    (* moves highBits into lowBits, clears highBits, shifts lowBits *)
    
    n.lowBits := n.highBits;
    n.highBits := 0;
    
    (* bit at position HalfBitwidth - shiftFactor is pivotal *)
    pivotalBit := HalfBitwidth - shiftFactor;
    
    (* shift lowBits *)
    n.lowBits := n.lowBits DIV pow2(pivotalBit);
    RETURN
    
  (* shifting by Bitwidth or more *)
  ELSE
    (* produces all zeroes *)
    n.lowBits := 0;
    n.highBits := 0;
    RETURN  
  END (* IF *)
END SHR;


(* ************************************************************************ *
 * Private Operations                                                       *
 * ************************************************************************ *)

(* ---------------------------------------------------------------------------
 * function addOverflows(n, m)
 * ---------------------------------------------------------------------------
 * Returns TRUE if operation n + m would overflow, else FALSE.
 * ------------------------------------------------------------------------ *)

PROCEDURE addOverflows ( n, m : CARDINAL ) : BOOLEAN;

BEGIN
  RETURN (m > 0) AND (n > MAX(CARDINAL) - m)
END addOverflows;


(* ---------------------------------------------------------------------------
 * procedure ClearBitsInclAndAbove( value, lowestBitToClear )
 * ---------------------------------------------------------------------------
 * Clears all bits including and above bit at position lowestBitToClear.
 * ------------------------------------------------------------------------ *)

PROCEDURE ClearBitsInclAndAbove
  ( VAR value : CARDINAL; lowestBitToClear : CARDINAL );

VAR
  mask, bitToClear : CARDINAL;
  
BEGIN
  (* no point clearing bits we don't have *)
  IF lowestBitToClear > HalfBitwidth-1 THEN
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
  mask := mask * pow2(lowestBitToClear + 1);
  
  (* subtract the mask, thereby clearing the high bits *)
  value := value - mask
END ClearBitsInclAndAbove;


VAR
  powerOf2 : ARRAY [0..Bitwidth-1] OF CARDINAL;

PROCEDURE pow2 ( n : CARDINAL ) : CARDINAL;
BEGIN
  RETURN powerOf2[n]
END pow2;

PROCEDURE InitPow2Table;

VAR
  index : CARDINAL;

BEGIN
  powerOf2[0] := 1;
  FOR index := 1 TO Bitwidth-1 DO
    powerOf2[index] := powerOf2[index-1] * 2
  END (* FOR *)
END InitPow2Table;

BEGIN (* LONGCARD *)
  InitPow2Table
END LONGCARD.