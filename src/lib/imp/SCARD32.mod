(*!m2pim*) (* Copyright (c) 2020 Modula-2 Software Foundation. *)

IMPLEMENTATION MODULE SCARD32; (* Simulated 32-bit Cardinal *)

(* This library provides CARDINAL semantics for type LONGINT *)


IMPORT Console; (* for runtime error messages *)


CONST
  MinValue = 0;
  MaxValue = MIN(LONGINT);


(* --------------------------------------------------------------------------
 * function fromCard(n)
 * --------------------------------------------------------------------------
 * Returns the SCard32T value of CARDINAL n.
 * ----------------------------------------------------------------------- *)

PROCEDURE fromCard ( n : CARDINAL ) : SCard32T;
  
BEGIN
  RETURN VAL(LONGINT, n)
END fromCard;


(* --------------------------------------------------------------------------
 * function fromInt(i)
 * --------------------------------------------------------------------------
 * Returns the SCard32T value of non-negative INTEGER i.
 * ----------------------------------------------------------------------- *)

TYPE PosInt = INTEGER [0 .. MAX(INTEGER)];

PROCEDURE fromInt ( i : PosInt ) : SCard32T;

BEGIN
  RETURN VAL(LONGINT, i)
END fromInt;


(* --------------------------------------------------------------------------
 * function fromLongInt(i)
 * --------------------------------------------------------------------------
 * Returns the SCard32T value of non-negative LONGINT i.
 * ----------------------------------------------------------------------- *)

TYPE LongPosInt = INTEGER [0 .. MAX(LONGINT)];

PROCEDURE fromLongInt ( i : LongPosInt ) : SCard32T;

BEGIN
  RETURN i
END fromLongInt;


(* --------------------------------------------------------------------------
 * function max(n)
 * --------------------------------------------------------------------------
 * Returns the largest value of type SCard32T.
 * ----------------------------------------------------------------------- *)

PROCEDURE max : SCard32T;

BEGIN
  RETURN MaxValue
END max;


(* --------------------------------------------------------------------------
 * function min(n)
 * --------------------------------------------------------------------------
 * Returns the smallest value of type SCard32T.
 * ----------------------------------------------------------------------- *)

PROCEDURE min : SCard32T;

BEGIN
  RETURN MinValue
END min;


(* --------------------------------------------------------------------------
 * function odd(n)
 * --------------------------------------------------------------------------
 * Returns TRUE if n is odd, otherwise FALSE.
 * ----------------------------------------------------------------------- *)

PROCEDURE odd ( n : SCard32T ) : BOOLEAN;

BEGIN
  RETURN ODD(n)
END odd;


(* --------------------------------------------------------------------------
 * function eq(n, m)
 * --------------------------------------------------------------------------
 * Returns TRUE if n = m, otherwise FALSE.
 * ----------------------------------------------------------------------- *)

PROCEDURE eq ( n, m : SCard32T ) : BOOLEAN;

BEGIN
  RETURN (n = m)
END eq;


(* --------------------------------------------------------------------------
 * function neq(n, m)
 * --------------------------------------------------------------------------
 * Returns TRUE if n # m, otherwise FALSE.
 * ----------------------------------------------------------------------- *)

PROCEDURE neq ( n, m : SCard32T ) : BOOLEAN;

BEGIN
  RETURN (n # m)
END neq;


(* --------------------------------------------------------------------------
 * function gt(n, m)
 * --------------------------------------------------------------------------
 * Returns TRUE if n > m, otherwise FALSE.
 * ----------------------------------------------------------------------- *)

PROCEDURE gt ( n, m : SCard32T ) : BOOLEAN;

BEGIN
  IF n >= 0 THEN
    IF m >= 0 THEN
      RETURN (n > m)
    ELSE (* n >= 0, m < 0 *)
      RETURN FALSE
    END (* IF *)
  ELSE (* n < 0 *)
    IF m < 0 THEN
      RETURN (n < m)
    ELSE (* m >= 0 *)
      RETURN TRUE
    END (* IF *)
  END (* IF *)
END gt;


(* --------------------------------------------------------------------------
 * function gteq(n, m)
 * --------------------------------------------------------------------------
 * Returns TRUE if n >= m, otherwise FALSE.
 * ----------------------------------------------------------------------- *)

PROCEDURE gteq ( n, m : SCard32T ) : BOOLEAN;

BEGIN
  RETURN (n = m) OR gt(n, m)
END gteq;


(* --------------------------------------------------------------------------
 * function lt(n, m)
 * --------------------------------------------------------------------------
 * Returns TRUE if n < m, otherwise FALSE.
 * ----------------------------------------------------------------------- *)

PROCEDURE lt ( n, m : SCard32T ) : BOOLEAN;

BEGIN
  IF n >= 0 THEN
    IF m >= 0 THEN
      RETURN (n < m)
    ELSE (* n >= 0, m < 0 *)
      RETURN TRUE
    END (* IF *)
  ELSE (* n < 0 *)
    IF m < 0 THEN
      RETURN (n > m)
    ELSE (* m >= 0 *)
      RETURN FALSE
    END (* IF *)
  END (* IF *)
END lt;


(* --------------------------------------------------------------------------
 * function lteq(n, m)
 * --------------------------------------------------------------------------
 * Returns TRUE if n <= m, otherwise FALSE.
 * ----------------------------------------------------------------------- *)

PROCEDURE lteq ( n, m : SCard32T ) : BOOLEAN;

BEGIN
  RETURN (n = m) OR lt(n, m)
END lteq;


(* --------------------------------------------------------------------------
 * function sum(n, m)
 * --------------------------------------------------------------------------
 * Returns the value of n + m.
 * ----------------------------------------------------------------------- *)

PROCEDURE sum ( n, m : SCard32T ) : SCard32T;

VAR
  delta : LONGINT;
  
BEGIN
  IF n >= 0 THEN
    IF m >= 0 THEN
      (* positive n and m may overflow *)
      IF n <= MAX(LONGINT) - m THEN
        RETURN n + m
      ELSE (* signed overflow *)
        delta := TMAX(LONGINT) - n - m;
        RETURN TMIN(LONGINT) - delta 
      END (* IF *)
    ELSE (* n >= 0, m < 0 *)
      RETURN n + m
    END (* IF *)
  ELSE (* n < 0 *)
    IF m < 0 THEN
      (* negative n and m may underflow *)
      IF n <= TMIN(LONGINT) - m THEN
        RETURN n + m
      ELSE (* signed underflow *) 
        Console.WriteString("type overflow while executing SCARD32.sum()");
        Console.WriteLn;
        HALT (* unsigned overflow *)
      END (* IF *)
    ELSE (* m >= 0 *)
      RETURN n + m
    END (* IF *)
  END (* IF *)
END sum;


(* --------------------------------------------------------------------------
 * function diff(n, m)
 * --------------------------------------------------------------------------
 * Returns the value of n - m.
 * ----------------------------------------------------------------------- *)

PROCEDURE diff ( n, m : SCard32T ) : SCard32T;

VAR
  delta : LONGINT;
  
BEGIN
  IF n >= 0 THEN
    IF m >= 0 THEN
      RETURN n - m
    ELSE (* n >= 0, m < 0 *)
      (* positive n and negative m may overflow *)
      IF n <= MAX(LONGINT) + m THEN
        RETURN n + m
      ELSE (* signed overflow *)
        delta := TMAX(LONGINT) - n + m;
        RETURN TMIN(LONGINT) - delta
      END (* IF *)
    END (* IF *)
  ELSE (* n < 0 *)
    IF m < 0 THEN
      RETURN n - m
    ELSE (* n < 0, m >= 0 *)
      (* negative n and positive m may underflow *)
      IF n <= TMIN(LONGINT) + m THEN
        RETURN n + m
      ELSE (* signed underflow *) 
        Console.WriteString("type overflow while executing SCARD32.diff()");
        Console.WriteLn;
        HALT (* unsigned overflow *)
      END (* IF *)
    END (* IF *)
  END (* IF *)
END diff;


(* --------------------------------------------------------------------------
 * function mul(n, m)
 * --------------------------------------------------------------------------
 * Returns the value of n * m.
 * ----------------------------------------------------------------------- *)

PROCEDURE mul ( n, m : SCard32T ) : SCard32T;

VAR
  prod : SCard32T;
  bitIndex : BitIndex;
  
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

PROCEDURE div ( n, m : SCard32T ) : SCard32T;

BEGIN
  IF m = 0 THEN
    Console.WriteString("division by 0 while executing SCARD32.div()");
    Console.WriteLn;
    HALT
  END; (* IF *)

  (* TO DO *)
END div;


(* --------------------------------------------------------------------------
 * function mod(n, m)
 * --------------------------------------------------------------------------
 * Returns the value of n MOD m.
 * ----------------------------------------------------------------------- *)

PROCEDURE mod ( n, m : SCard32T ) : SCard32T;

BEGIN
  IF m = 0 THEN
    Console.WriteString("division by 0 while executing SCARD32.mod()");
    Console.WriteLn;
    HALT
  END; (* IF *)
  
  (* TO DO *)
END mod;


(* ---------------------------------------------------------------------------
 * function shl( n, shiftFactor )
 * ---------------------------------------------------------------------------
 * Returns the value of n shifted left by shiftFactor.
 * ------------------------------------------------------------------------ *)

PROCEDURE shl ( n : SCard32T; shiftFactor : BitIndex ) : SCard32T;

VAR
  highBit : BOOLEAN;
  pivotalBit : BitIndex;
  
BEGIN
  (* shifting by 0 *)
  IF shiftFactor = 0 THEN
    RETURN n

  (* shifting by Bitwidth *)
  ELSIF shiftFactor = BitWidth THEN
    RETURN 0
  END; (* IF *)
  
  (* clear MSB *)  
  IF n < 0 THEN
    n := n - MIN(LONGINT)
  END; (* IF *)
  
  (* shift bits 0..30 *)
  
    (* bit at position Bitwidth-1 - shiftFactor is pivotal *)
    pivotalBit := (Bitwidth-1) - shiftFactor;
    
    (* save bit that will become MSB *)
    highBit := (n >= pow2(pivotalBit-1));
    
    (* clear bits including and above pivotal bit to avoid overflow *)
    IF n >= pow2(pivotalBit) THEN
      ClearBitsInclAndAbove(n, pivotalBit)
    END; (* IF *)
    
    (* shift safely *)
    n := n * pow2(shiftFactor);
    
    (* determine and set MSB *)
    IF highBit THEN
      n := n + MIN(LONGINT)
    END (* IF *)
    
  (* shifting by Bitwidth *)
  ELSE
    (* all bits shifted out *)
    RETURN 0
  END (* IF *)
END shl;


(* ---------------------------------------------------------------------------
 * function shr( n, shiftFactor )
 * ---------------------------------------------------------------------------
 * Returns the value of n shifted right by shiftFactor.
 * ------------------------------------------------------------------------ *)

PROCEDURE shr ( n : SCard32T; shiftFactor : BitIndex ) : SCard32T;

VAR
  highBit : BOOLEAN;
  
BEGIN
  IF shiftFactor = 0 THEN
    RETURN n
  END; (* IF *)
  
  (* save and clear MSB *)
  highBit := ORD(n < 0);
  
  IF highBit THEN
    n := n - MIN(LONGINT)
  END; (* IF *)
  
  (* shift bits 0..30 *)
  n := n DIV pow2(shiftFactor);
  
  (* shift bit 31 *) *)
  IF highBit THEN
    n := n + pow2(31)
  END; (* IF *)
  
  RETURN n
END shr;


(* ---------------------------------------------------------------------------
 * procedure ClearBitsInclAndAbove( value, lowestBitToClear )
 * ---------------------------------------------------------------------------
 * Clears all bits including and above bit at position lowestBitToClear.
 * ------------------------------------------------------------------------ *)

PROCEDURE ClearBitsInclAndAbove
  ( VAR value : SCard32T; lowestBitToClear : CARDINAL );

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


(* ---------------------------------------------------------------------------
 * function pow2(n)
 * ---------------------------------------------------------------------------
 * Returns the power of 2 for argument n.
 * ------------------------------------------------------------------------ *)

PROCEDURE pow2 ( n : BitIndex ) : SCard32T;

BEGIN
  RETURN powerOf2[n]
END pow2;


(* ---------------------------------------------------------------------------
 * Powers of 2 table.
 * ------------------------------------------------------------------------ *)

VAR
  powerOf2 : ARRAY [0..31] OF LONGINT;


BEGIN (* SCARD32 *)
  (* assert LONGINT is 32-bit *)
  IF TSIZE(LONGINT) # 4 THEN
    Console.WriteStr("Library SCARD32 requires 32-bit LONGINT.");
    Console.WriteLn;
    HALT
  END; (* IF *)
  
  (* initialise powers of 2 table *)
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
  powerOf2[31] := -2147483648
END SCARD32.