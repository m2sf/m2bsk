(*!m2pim*) (* Copyright (c) 2020 Modula-2 Software Foundation. *)

IMPLEMENTATION MODULE CARD64; (* 32-bit CARDINAL version *)

(* 64-bit Cardinal Type *)


FROM SYSTEM IMPORT TSIZE;
FROM Card64TBitOps IMPORT Bitwidth, BitIndex, Shl, Shr, bit, SetBit, ClearBit;


(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * NOTE : Not all classic Modula-2 compilers support records as return types
 * of functions.  For  portability,  all functions  that return a result  of
 * type Card64T  are therefore  implemented  as procedures  where the result
 * is passed back in a VAR parameter instead.
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)
 

(* --------------------------------------------------------------------------
 * Procedure:  CARD64.FromCard(result, n)
 * --------------------------------------------------------------------------
 * Passes the Card64T value of CARDINAL n in result.
 * ----------------------------------------------------------------------- *)

PROCEDURE FromCard ( VAR result : Card64T; n : CARDINAL );

BEGIN
  result.highBits := 0;
  result.lowBits := n
END FromCard;


(* --------------------------------------------------------------------------
 * Procedure:  CARD64.FromInt(result, i)
 * --------------------------------------------------------------------------
 * Passes the Card64T value of non-negative INTEGER i in result.
 * ----------------------------------------------------------------------- *)

TYPE PosInt = INTEGER [0 .. MAX(INTEGER)];

PROCEDURE FromInt ( VAR result : Card64T; i : PosInt );

BEGIN
  result.highBits := 0;
  result.lowBits := VAL(CARDINAL, i)
END FromInt;


(* --------------------------------------------------------------------------
 * Procedure:  CARD64.FromLongInt(result, i)
 * --------------------------------------------------------------------------
 * Passes the Card64T value of non-negative LONGINT i in result.
 * ----------------------------------------------------------------------- *)

TYPE LongPosInt = INTEGER [0 .. MAX(LONGINT)];

PROCEDURE FromLongInt ( VAR result : Card64T; i : LongPosInt );

BEGIN
  result.highBits := 0;
  result.lowBits := VAL(CARDINAL, i)
END FromInt;


(* --------------------------------------------------------------------------
 * Procedure:  CARD64.SetZero(result)
 * --------------------------------------------------------------------------
 * Passes the smallest value of type Card64T in result.
 * ----------------------------------------------------------------------- *)

PROCEDURE SetZero ( VAR result : Card64T );

BEGIN
  result.highBits := 0;
  result.lowBits := 0
END SetZero;


(* --------------------------------------------------------------------------
 * Procedure:  CARD64.SetMax(result)
 * --------------------------------------------------------------------------
 * Passes the largest value of type Card64T in result.
 * ----------------------------------------------------------------------- *)

PROCEDURE SetMax ( VAR result : Card64T );

BEGIN
  result.highBits := MAX(CARDINAL);
  result.lowBits := MAX(CARDINAL)
END SetZero;


(* --------------------------------------------------------------------------
 * Function:  CARD64.odd(n)
 * --------------------------------------------------------------------------
 * Returns TRUE if n is odd, otherwise FALSE.
 * ----------------------------------------------------------------------- *)

PROCEDURE odd ( n : Card64T ) : BOOLEAN;

BEGIN
  RETURN ODD(n.lowBits)
END odd;


(* --------------------------------------------------------------------------
 * function:  CARD64.eq(n, m)
 * --------------------------------------------------------------------------
 * Returns TRUE if n = m, otherwise FALSE.
 * ----------------------------------------------------------------------- *)

PROCEDURE eq ( n, m : Card64T ) : BOOLEAN;

BEGIN
  RETURN (n.lowBits = m.lowBits) AND (n.highBits = m.highBits)
END eq;


(* --------------------------------------------------------------------------
 * Function:  CARD64.neq(n, m)
 * --------------------------------------------------------------------------
 * Returns TRUE if n # m, otherwise FALSE.
 * ----------------------------------------------------------------------- *)

PROCEDURE neq ( n, m : Card64T ) : BOOLEAN;

BEGIN
  RETURN (n.lowBits = m.lowBits) OR (n.highBits # m.highBits)
END eq;


(* --------------------------------------------------------------------------
 * Function:  CARD64.gt(n, m)
 * --------------------------------------------------------------------------
 * Returns TRUE if n > m, otherwise FALSE.
 * ----------------------------------------------------------------------- *)

PROCEDURE gt ( n, m : Card64T ) : BOOLEAN;

BEGIN
  IF (n.highBits = m.highBits) THEN
    RETURN n.lowBits > m.lowBits
  ELSE
    RETURN n.highBits > m.highBits
  END (* IF *)
END gt;


(* --------------------------------------------------------------------------
 * Function:  CARD64.gteq(n, m)
 * --------------------------------------------------------------------------
 * Returns TRUE if n >= m, otherwise FALSE.
 * ----------------------------------------------------------------------- *)

PROCEDURE gteq ( n, m : Card64T ) : BOOLEAN;

BEGIN
  RETURN eq(n, m) OR gt(n, m)
END gteq;


(* --------------------------------------------------------------------------
 * Function:  CARD64.lt(n, m)
 * --------------------------------------------------------------------------
 * Returns TRUE if n < m, otherwise FALSE.
 * ----------------------------------------------------------------------- *)

PROCEDURE lt ( n, m : Card64T ) : BOOLEAN;

BEGIN
  IF (n.highBits = m.highBits) THEN
    RETURN n.lowBits < m.lowBits
  ELSE
    RETURN n.highBits < m.highBits
  END (* IF *)
END lt;


(* --------------------------------------------------------------------------
 * Function:  CARD64.lteq(n, m)
 * --------------------------------------------------------------------------
 * Returns TRUE if n <= m, otherwise FALSE.
 * ----------------------------------------------------------------------- *)

PROCEDURE lteq ( n, m : Card64T ) : BOOLEAN;

BEGIN
  RETURN eq(n, m) OR lt(n, m)
END lteq;


(* --------------------------------------------------------------------------
 * Procedure:  CARD64.Add(n, m)
 * --------------------------------------------------------------------------
 * Passes the sum n + m back in n.
 * ----------------------------------------------------------------------- *)

PROCEDURE Add ( VAR n, m : Card64T );

VAR
  overflow : CARDINAL;
  
BEGIN
  ADDC(n, m, overflow);
  
  IF overflow > 0 THEN HALT END
END Add;


(* --------------------------------------------------------------------------
 * Procedure:  CARD64.Sub(n, m)
 * --------------------------------------------------------------------------
 * Passes the difference n - m back in n.
 * ----------------------------------------------------------------------- *)

PROCEDURE Sub ( VAR n, m : Card64T );

VAR
  overflow : CARDINAL;
  complement : Card64T;
  
BEGIN
  (* calculate the two's complement of the subtrahend *)
  complement.lowBits := MAX(CARDINAL) - m.lowBits + 1;
  complement.highBits := MAX(CARDINAL) - m.highBits + 1;
  
  (* add the complement to n *)
  ADDC(n, complement, overflow);
  
  IF overflow > 0 THEN HALT END
END Sub;


(* --------------------------------------------------------------------------
 * Procedure:  CARD64.Mul(n, m)
 * --------------------------------------------------------------------------
 * Passes the product n * m back in n.
 * ----------------------------------------------------------------------- *)

PROCEDURE Mul ( VAR n, m : Card64T );

VAR
  prod : Card64T;
  bitIndex : BitIndex;
  
BEGIN
  (* initialise prod *)
  prod.highBits := 0;
  prod.lowBits := 0;
  
  FOR bitIndex := 0 TO Bitwidth DO
    (* test LSB of m and add n to prod if set *)
    IF ODD(m.lowBits) THEN
      Add(prod, n)
    END; (* IF *)
    
    (* shift n left by one *)
    Card64BitsOps.Shl(n, 1);
    
    (* shift m right by one *)
    Card64BitsOps.Shr(m, 1)
  END; (* FOR *)
  
  (* pass result in n *)
  n := prod
END Mul;


(* --------------------------------------------------------------------------
 * Procedure:  CARD64.Div(n, m)
 * --------------------------------------------------------------------------
 * Passes the integer division quotient n DIV m back in n.
 * ----------------------------------------------------------------------- *)

PROCEDURE Div ( VAR n, m : Card64T );

VAR
  quotient, remainder : Card64T;
  
BEGIN
  DIVMOD(n, m, quotient, remainder);
  
  (* pass quotient in n *)
  n := quotient
END Div;


(* --------------------------------------------------------------------------
 * Procedure:  CARD64.Mod(n, m)
 * --------------------------------------------------------------------------
 * Passes the integer division modulus n MOD m back in n.
 * ----------------------------------------------------------------------- *)

PROCEDURE Mod ( VAR n, m : Card64T );

VAR
  quotient, remainder : Card64T;
  
BEGIN
  DIVMOD(n, m, quotient, remainder);
  
  (* pass remainder in n *)
  n := remainder
END Mod;


(* ************************************************************************ *
 * Private Operations                                                       *
 * ************************************************************************ *)

(* --------------------------------------------------------------------------
 * Private procedure:  ADDC(n, m, carry)
 * --------------------------------------------------------------------------
 * Adds m to n. If overflow occurs, one is passed in carry, else zero.
 * ----------------------------------------------------------------------- *)

CONST
  HalfBitwidth = Bitwidth DIV 2;
  HalfMaxCardPlusOne = (MAX(CARDINAL) DIV 2) + 1; (* 2^(Bitwidth-1) + 1 *)


PROCEDURE ADDC ( VAR n : Card64T; m : Card64T; VAR carry : CARDINAL );

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
END ADDC;


(* --------------------------------------------------------------------------
 * Private procedure:  DIVMOD(n, m, quotient, remainder)
 * --------------------------------------------------------------------------
 * Divides n by m, passes quotient and remainder.
 * ----------------------------------------------------------------------- *)

PROCEDURE DIVMOD ( n, m : Card64T; VAR quotient, remainder : Card64T );

VAR
  bitIndex : BitIndex;
  
BEGIN
  (* division by zero *)
  IF (m.lowBits = 0) AND (m.highBits = 0) THEN
    HALT (* abort *)
  
  (* division by one *)
  ELSIF (m.lowBits = 1) AND (m.highBits = 0) THEN
    quotient := n;
    remainder.lowBits := 0;
    remainder.highBits := 0;
    RETURN
  
  (* division by itself *)
  ELSIF (n.lowBits = m.lowBits) AND (n.highBits = m.highBits) THEN
    quotient.lowBits := 1;
    quotient.highBits := 0;
    remainder.lowBits := 0;
    remainder.highBits := 0;
    RETURN
  END; (* IF *)
  
  (* set initial values for remainder and quotient *)
  remainder.highBits := 0; remainder.lowBits := 0;
  quotient.highBits := 0; quotient.lowBits := 0;
  
  (* binary long division *)
  FOR bitIndex := Bitwidth-1 TO 0 BY -1 DO
    (* shift high bit out of remainder *)
    Card64BitOps.Shr(remainder, 1);
    
    (* copy bit at bitIndex of n into LSB of remainder *)
    IF bit(n, bitIndex) THEN
      SetBit(remainder, bitIndex)
    ELSE
      ClearBit(remainder, bitIndex)
    END; (* IF *)
    
    (* compute quotient *)
    IF gt(remainder, m) THEN
      Sub(remainder, m);
      SetBit(quotient, bitIndex)
    END (* IF *)
  END (* FOR *)  
END DIVMOD;


END CARD64.