(*!m2pim*) (* Copyright (c) 2017 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE CardMath; (* 64-bit version *)

(* Cardinal Math library *)

FROM SYSTEM IMPORT TSIZE;


CONST
  Bitwidth = TSIZE(CARDINAL) * 8;


(* Math operations *)

(* --------------------------------------------------------------------------
 * function abs(i)
 * --------------------------------------------------------------------------
 * Returns the absolute CARDINAL value of INTEGER i
 * ----------------------------------------------------------------------- *)

PROCEDURE abs ( i : INTEGER ) : CARDINAL;

BEGIN
  IF i = MAX(INTEGER) THEN
    RETURN pow2(Bitwidth-1)
  ELSE
    RETURN VAL(CARDINAL, ABS(i))
  END (* IF *)
END abs;



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
 * Returns the integral part of the logarithm of 2 for argument n.
 * ----------------------------------------------------------------------- *)

PROCEDURE log2 ( n : CARDINAL ) : CARDINAL;

VAR
  r, k : CARDINAL;
  
BEGIN
  (* bail out if n is zero *)
  IF n = 0 THEN HALT END;
  
  r := 0;
  
  (* any size of CARDINAL *)
  k := Bitwidth DIV 2;
  WHILE k > 0 DO
    
    IF n >= pow2(k) THEN
      r := r + k;
      n := shr(n, k)
    END; (* IF *)
    
    k := k DIV 2
  END; (* WHILE *)
  
  return r
END log2;


(* --------------------------------------------------------------------------
 * function pow10(n)
 * --------------------------------------------------------------------------
 * Returns the power of 10 for argument n
 * ----------------------------------------------------------------------- *)

PROCEDURE pow10 ( n : CARDINAL ) : CARDINAL;

BEGIN
  RETURN powerOf10[n]
END pow10;


(* --------------------------------------------------------------------------
 * function log10(n)
 * --------------------------------------------------------------------------
 * Returns the integral part of the logarithm of 10 for argument n
 * ----------------------------------------------------------------------- *)

PROCEDURE log10 ( n : CARDINAL ) : CARDINAL;

VAR
  index, approximation : CARDINAL;
  
BEGIN
  index := shr((log2(n) + 1) * 1233, 12);
  approximation := powerOf10[index];
  IF n < approximation THEN
    RETURN index - 1
  ELSE
    RETURN approximation
  END (* IF *)
END log10;


(* --------------------------------------------------------------------------
 * function MaxDecimalDigits(n)
 * --------------------------------------------------------------------------
 * Returns the number of decimal digits of the largest unsigned integer that
 * can be encoded in base-2 using n number of 8-bit octets for 1 <= n <= 16.
 * ----------------------------------------------------------------------- *)

PROCEDURE MaxDecimalDigits ( octets : Card1To16 ) : CARDINAL;


(* --------------------------------------------------------------------------
 * function twosComplement(n)
 * --------------------------------------------------------------------------
 * Returns the two's complement of n
 * ----------------------------------------------------------------------- *)

PROCEDURE twosComplement ( n : CARDINAL ) : CARDINAL;

BEGIN
  RETURN MAX(CARDINAL) - n + 1
END twosComplement;


(* ---------------------------------------------------------------------------
 * function addOverflows(n, m)
 * ---------------------------------------------------------------------------
 * Returns TRUE if operation n + m would overflow, else FALSE.
 * ------------------------------------------------------------------------ *)

PROCEDURE addOverflows ( n, m : CARDINAL ) : BOOLEAN;

BEGIN
  RETURN (m > 0) AND (n > MAX(CARDINAL) - m)
END addOverflows;


(* Bit operations *)

(* --------------------------------------------------------------------------
 * function shl(n, shiftFactor)
 * --------------------------------------------------------------------------
 * Returns the value of n, shifted left by shiftFactor.
 * ----------------------------------------------------------------------- *)

PROCEDURE shl ( n, shiftFactor : CARDINAL ) : CARDINAL;

BEGIN
  (* TO DO *)
END shl;


(* --------------------------------------------------------------------------
 * function shr(n, shiftFactor)
 * --------------------------------------------------------------------------
 * Returns the value of n, shifted right by shiftFactor.
 * ----------------------------------------------------------------------- *)

PROCEDURE shr ( n, shiftFactor : CARDINAL ) : CARDINAL;

BEGIN
  (* TO DO *)
END shr;


(* --------------------------------------------------------------------------
 * function MSB(n)
 * --------------------------------------------------------------------------
 * Returns TRUE if the most significant bit of n is set, else FALSE.
 * ----------------------------------------------------------------------- *)

PROCEDURE MSB ( n : CARDINAL ) : BOOLEAN;

BEGIN
  (* TO DO *)
END MSB;


(* --------------------------------------------------------------------------
 * procedure SetMSB(n)
 * --------------------------------------------------------------------------
 * Sets the most significant bit of n.
 * ----------------------------------------------------------------------- *)

PROCEDURE SetMSB ( VAR n : CARDINAL );

BEGIN
  (* TO DO *)
END SetMSB;


(* --------------------------------------------------------------------------
 * procedure ClearMSB(n)
 * --------------------------------------------------------------------------
 * Clears the most significant bit of n.
 * ----------------------------------------------------------------------- *)

PROCEDURE ClearMSB ( VAR n : CARDINAL );

BEGIN
  (* TO DO *)
END ClearMSB;


(* --------------------------------------------------------------------------
 * procedure ClearHighestNBits(value, n)
 * --------------------------------------------------------------------------
 * Clears bits [MSB..MSB-n] of value.
 * ----------------------------------------------------------------------- *)

PROCEDURE ClearHighestNBits ( VAR value : CARDINAL; n : CARDINAL );

VAR
  mask : CARDINAL;
  
BEGIN
  (* no point clearing bits we don't have *)
  IF n > HalfBitwidth-1 THEN
    RETURN
  END; (* IF *)
  
  (* clearing from bit 0 produces all zeroes *)
  IF n = 0 THEN
    value := 0;
    RETURN
  END; (* IF *)
  
  (* shift lower bits out to the right *)
  mask := value DIV (n + 1);
  
  (* shift them back, thereby clearing the low bits, obtaining a mask *)
  mask := mask * pow2(n + 1);
  
  (* subtract the mask, thereby clearing the high bits *)
  value := value - mask
END ClearHighestNBits;


(* ************************************************************************ *
 * Private Operations                                                       *
 * ************************************************************************ *)

(* --------------------------------------------------------------------------
 * Data table for powers of 2
 * ----------------------------------------------------------------------- *)

VAR
  powerOf2 : ARRAY [0..Bitwidth-1] OF CARDINAL;


(* --------------------------------------------------------------------------
 * procedure InitPow2Table
 * --------------------------------------------------------------------------
 * Initialises data table with powers of 2.
 * ----------------------------------------------------------------------- *)

PROCEDURE InitPow2Table;

VAR
  index : CARDINAL;

BEGIN
  powerOf2[0] := 1;
  
  (* any size of CARDINAL *)
  FOR index := 1 TO Bitwidth-1 DO
    powerOf2[index] := powerOf2[index-1] * 2
  END (* FOR *)
END InitPow2Table;


(* --------------------------------------------------------------------------
 * Data table for powers of 10
 * ----------------------------------------------------------------------- *)

VAR
  powerOf10 : ARRAY [0..Bitwidth-1] OF CARDINAL;


(* --------------------------------------------------------------------------
 * procedure InitPow10Table
 * --------------------------------------------------------------------------
 * Initialises data table with powers of 10.
 * ----------------------------------------------------------------------- *)

PROCEDURE InitPow10Table;

VAR
  index : CARDINAL;

BEGIN
  powerOf10[0] := 1;
  
  (* any size of CARDINAL *)
  FOR index := 1 TO MaxDecimalDigits(Bitwidth DIV 8) DO
    powerOf10[index] := powerOf10[index-1] * 10
  END (* FOR *)
END InitPow10Table;


(* --------------------------------------------------------------------------
 * Data table for maximum decimal digits
 * ----------------------------------------------------------------------- *)

VAR
  maxDecDigits : ARRAY [1..16] OF CARDINAL;


(* --------------------------------------------------------------------------
 * procedure: IntMaxDecDigitsTable
 * --------------------------------------------------------------------------
 * Initialises data table for maximum decimal digits.
 * ----------------------------------------------------------------------- *)

PROCEDURE IntMaxDecDigitsTable;

BEGIN
  maxDecDigits[1]  := 3;  (*   8 bits *)
  maxDecDigits[2]  := 5;  (*  16 bits *)
  maxDecDigits[3]  := 8;  (*  24 bits *)
  maxDecDigits[4]  := 10; (*  32 bits *)
  maxDecDigits[5]  := 13; (*  40 bits *)
  maxDecDigits[6]  := 15; (*  48 bits *)
  maxDecDigits[7]  := 17; (*  56 bits *)
  maxDecDigits[8]  := 20; (*  64 bits *)
  maxDecDigits[9]  := 22; (*  72 bits *)
  maxDecDigits[10] := 25; (*  80 bits *)
  maxDecDigits[11] := 27; (*  88 bits *)
  maxDecDigits[12] := 29; (*  96 bits *)
  maxDecDigits[13] := 32; (* 104 bits *)
  maxDecDigits[14] := 34; (* 112 bits *)
  maxDecDigits[15] := 37; (* 120 bits *)
  maxDecDigits[16] := 39  (* 128 bits *)
END IntMaxDecDigitsTable;


(* Initialise all data tables *)

BEGIN
  InitPow2Table;
  InitPow10Table;
  IntMaxDecDigitsTable
END CardMath.