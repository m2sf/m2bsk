(*!m2pim*) (* Copyright (c) 2020 Modula-2 Software Foundation. *)

IMPLEMENTATION MODULE Card64Math; (* 32-bit CARDINAL version *)

(* CARD64 Math Library *)


IMPORT CardBitOps;

FROM CARD64 IMPORT Card64T, SetZero, SetMax, Add, Sub, Mul;


(* --------------------------------------------------------------------------
 * Procedure: Card64Math.Pow2(n)
 * --------------------------------------------------------------------------
 * Passes the power of 2 for argument n in result.
 * ----------------------------------------------------------------------- *)

PROCEDURE Pow2 ( VAR result : Card64T; n : Pow2Arg );

BEGIN
  result := powerOf2[n]
END Pow2;


(* --------------------------------------------------------------------------
 * Procedure: Card64Math.Log2(n)
 * --------------------------------------------------------------------------
 * Passes the integral part of the logarithm of 2 for argument n in result.
 * ----------------------------------------------------------------------- *)

PROCEDURE Log2 ( VAR result : Card64T; n : Card64T );

VAR
  k : CARDINAL;
  k64b : Card64T;
  
BEGIN
  (* bail out if n is zero *)
  IF (n.highBits = 0) AND (n.lowBits = 0) THEN HALT END;
  
  (* initialise result *)
  result.highBits := 0;
  result.lowBits := 0;
  
  (* log2(n) *)
  k := Bitwidth DIV 2;
  WHILE k > 0 DO
    
    IF gteq(n, powerOf2[k]) THEN
      k64.highbits := 0;
      k64.lowBits := k;
      Add(result, k64);
      Card64BitOps.Shr(n, k)
    END; (* IF *)
    
    k := k DIV 2
  END (* WHILE *)  
END Log2;


(* --------------------------------------------------------------------------
 * Procedure: Card64Math.Pow10(n)
 * --------------------------------------------------------------------------
 * Passes the power of 10 for argument n in result.
 * ----------------------------------------------------------------------- *)

PROCEDURE Pow10 ( VAR result : Card64T; n : Pow10Arg );

BEGIN
  result := powerOf10[n]
END Pow10;


(* --------------------------------------------------------------------------
 * Procedure: Card64Math.Log10(n)
 * --------------------------------------------------------------------------
 * Passes the integral part of the logarithm of 10 for argument n in result.
 * ----------------------------------------------------------------------- *)

PROCEDURE Log10 ( VAR result : Card64T; n : Card64T );

VAR
  index : CARDINAL;
  approximation, log2n : Card64T;
  
BEGIN
  (* bail out if n is zero *)
  IF (n.highBits = 0) AND (n.lowBits = 0) THEN HALT END;
  
  Log2(log2n, n);
  
  (* SHR((log2(n) + 1) * 1233, 12) *)
  index := CardBitOps.shr((log2n.lowBits + 1) * 1233, 12);
  
  approximation := powerOf10[index];
  IF n < approximation THEN
    result.highBits := 0;
    result.lowBits := index - 1
  ELSE
    result := approximation
  END (* IF *)
END log10;


(* --------------------------------------------------------------------------
 * Function:  Card64Math.addOverflows(n, m)
 * --------------------------------------------------------------------------
 * Returns TRUE if operation n + m overflows, else FALSE.
 * ----------------------------------------------------------------------- *)

PROCEDURE addOverflows ( n, m : Card64T ) : BOOLEAN;

VAR
  limit : Card64T;

BEGIN
  SetMax(limit);
  IF m # 0 THEN
    Sub(limit, m)
  END; (* IF *)
  RETURN gt(n, limit)
END addOverflows;


(* --------------------------------------------------------------------------
 * Procedure:  Card64T.TwosComplement(n)
 * --------------------------------------------------------------------------
 * Passes the two's complement of n in n.
 * ----------------------------------------------------------------------- *)

PROCEDURE TwosComplement ( VAR n : Card64T );

VAR
  result : Card64T;
  
BEGIN
  SetMax(result);
  Sub(result, n);
  Add(result, 1);
  n := result
END TwosComplement;


(* ************************************************************************ *
 * Private Operations                                                       *
 * ************************************************************************ *)

(* --------------------------------------------------------------------------
 * Data table for powers of 2
 * ----------------------------------------------------------------------- *)

VAR
  powerOf2 : ARRAY [0..Bitwidth-1] OF Card64T;


(* --------------------------------------------------------------------------
 * procedure InitPow2Table
 * --------------------------------------------------------------------------
 * Initialises data table with powers of 2.
 * ----------------------------------------------------------------------- *)

PROCEDURE InitPow2Table;

VAR
  index : CARDINAL;

BEGIN
  (* initialise lower half table *)
  
  (* first element *)
  powerOf2[0].highBits := 0;
  powerOf2[0].lowBits := 1;
  
  (* remainder *)
  FOR index := 1 TO Bitwidth/2-1 DO
    powerOf2[index].highBits := 0;
    powerOf2[index[.lowBits := powerOf2[index-1].lowBits * 2
  END; (* FOR *)
  
  (* initialise upper half table *)
  
  (* first element *)
  powerOf2[Bitwidth/2].highBits := 1;
  powerOf2[Bitwidth/2].lowBits := 0;
  
  (* remainder *)
  FOR index := Bitwidth/2+1 TO Bitwidth-1 DO
    powerOf2[index].lowBits := 0;
    powerOf2[index[.highBits := powerOf2[index-1].highBits * 2
  END (* FOR *)
END InitPow2Table;


(* --------------------------------------------------------------------------
 * Data table for maximum decimal digits
 * ----------------------------------------------------------------------- *)

VAR
  maxDecDigits : ARRAY [1..Bitwidth DIV 8] OF CARDINAL;


(* --------------------------------------------------------------------------
 * procedure: InitMaxDecDigitsTable
 * --------------------------------------------------------------------------
 * Initialises data table for maximum decimal digits.
 * ----------------------------------------------------------------------- *)

PROCEDURE InitMaxDecDigitsTable;

BEGIN
  maxDecDigits[1]  := 3;  (*   8 bits *)
  maxDecDigits[2]  := 5;  (*  16 bits *)
  maxDecDigits[3]  := 8;  (*  24 bits *)
  maxDecDigits[4]  := 10; (*  32 bits *)
  maxDecDigits[5]  := 13; (*  40 bits *)
  maxDecDigits[6]  := 15; (*  48 bits *)
  maxDecDigits[7]  := 17; (*  56 bits *)
  maxDecDigits[8]  := 20  (*  64 bits *)
END InitMaxDecDigitsTable;


(* --------------------------------------------------------------------------
 * Data table for powers of 10
 * ----------------------------------------------------------------------- *)

VAR
  powerOf10 : ARRAY [0..Bitwidth-1] OF Card64T;


(* --------------------------------------------------------------------------
 * procedure InitPow10Table
 * --------------------------------------------------------------------------
 * Initialises data table with powers of 10.
 * ----------------------------------------------------------------------- *)

PROCEDURE InitPow10Table;

VAR
  index : CARDINAL;
  value, factor := Card64T;

BEGIN
  factor.highBits := 0;
  factor.lowBits := 10;
  
  (* first element *)
  powerOf10[0].highBits := 0;
  powerOf10[0].lowBits := 1;
  
  
  (* remainder: element[n] := 10 * element[n-1] *)
  FOR index := 1 TO maxDecDigits[Bitwidth DIV 8] DO
    value := powerOf10[index-1];
    Mul(value, factor);
    powerOf10[index] := value
  END (* FOR *)
END InitPow10Table;


(* --------------------------------------------------------------------------
 * Module initialisation
 * ----------------------------------------------------------------------- *)

BEGIN (* Card64Math *)
  InitMaxDecDigitsTable;
  InitPow2Table;
  InitPow10Table
END Card64Math.