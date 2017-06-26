(*!m2pim*) (* Copyright (c) 2016 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE Console;

(* Console output library *)

IMPORT ASCII, Terminal, String;

FROM String IMPORT StringT; (* alias for String.String *)

FROM CardMath IMPORT abs, pow2, pow10, deg10;
FROM LongIntMath IMPORT longIntPow2, longIntPow10;


(* ---------------------------------------------------------------------------
 * procedure WriteChars(chars)
 * ---------------------------------------------------------------------------
 * Prints the given character array to the console.
 * ------------------------------------------------------------------------ *)

PROCEDURE WriteChars ( chars : ARRAY OF CHAR );

VAR
  ch : CHAR;
  
BEGIN
  FOR index := 0 TO HIGH(chars) DO
    ch := chars[index];
    IF ch > ASCII.US THEN
      Terminal.Write(ch)
    ELSIF ch = ASCII.NUL THEN
      EXIT
    END (* IF *)
  END (* FOR *)
END WriteChars;


(* ---------------------------------------------------------------------------
 * procedure WriteStr(s)
 * ---------------------------------------------------------------------------
 * Prints the given string to the console.
 * ------------------------------------------------------------------------ *)

PROCEDURE WriteStr ( s : StringT );

BEGIN
  IF (s # NIL) AND (String.length(s) > 0) THEN
    String.WithCharsDo(s, Terminal.WriteString)
  END (* IF *)
END WriteStr;


(* ---------------------------------------------------------------------------
 * procedure WriteCharsAndStr(chars, s)
 * ---------------------------------------------------------------------------
 * Prints the given character array and string to the console.
 * ------------------------------------------------------------------------ *)

PROCEDURE WriteCharsAndStr ( VAR chars : ARRAY OF CHAR; s : StringT );

VAR
  ch : CHAR;
  
BEGIN
  (* print chars *)
  FOR index := 0 TO HIGH(chars) DO
    ch := chars[index];
    IF ch > ASCII.US THEN
      Terminal.Write(ch)
    ELSIF ch = ASCII.NUL THEN
      EXIT
    END (* IF *)
  END; (* FOR *)
  
  (* print s *)
  IF (s # NIL) AND (String.length(s) > 0) THEN
    String.WithCharsDo(s, Terminal.WriteString)
  END (* IF *)
END WriteCharsAndStr;


(* ---------------------------------------------------------------------------
 * procedure WriteLn
 * ---------------------------------------------------------------------------
 * Prints newline to the console.
 * ------------------------------------------------------------------------ *)

PROCEDURE WriteLn;

BEGIN
  Terminal.WriteLn
END WriteLn;


(* ---------------------------------------------------------------------------
 * procedure WriteBool(value)
 * ---------------------------------------------------------------------------
 * Prints the given value to the console. "TRUE" for TRUE, "FALSE" for FALSE.
 * ------------------------------------------------------------------------ *)

PROCEDURE WriteBool ( value : BOOLEAN );

BEGIN
  IF value = TRUE THEN
    Terminal.WriteString("TRUE")
  ELSE
    Terminal.WriteString("FALSE")
  END (* IF *)
END WriteBool;


(* ---------------------------------------------------------------------------
 * procedure WriteBoolCustom(value)
 * ---------------------------------------------------------------------------
 * Prints trueStr if value is TRUE, falseStr if value is FALSE.
 * ------------------------------------------------------------------------ *)

PROCEDURE WriteBoolCustom
  ( value : BOOLEAN; VAR (* CONST *) trueStr, falseStr : ARRAY OF CHAR );

BEGIN
  IF value = TRUE THEN
    Terminal.WriteString(trueStr)
  ELSE
    Terminal.WriteString(falseStr)
  END (* IF *)
END WriteBoolCustom;


(* ---------------------------------------------------------------------------
 * procedure WriteChar(chars)
 * ---------------------------------------------------------------------------
 * Prints the given character to the console.
 * ------------------------------------------------------------------------ *)

PROCEDURE WriteChar ( char : CHAR );

BEGIN
  IF char > ASCII.US THEN
    Terminal.Write(char)
  END (* IF *)
END WriteChar;


(* ---------------------------------------------------------------------------
 * procedure WriteCharU(chars)
 * ---------------------------------------------------------------------------
 * Prints the given character value in 0u notation to the console.
 * ------------------------------------------------------------------------ *)

PROCEDURE WriteCharU ( char : CHAR );

VAR
  value, n, weight, digit : CARDINAL;
  
BEGIN
  (* convert char *)
  value := ORD(char);
  
  (* print prefix *)
  WriteChars("0u");
  
  (* print digits, including leading zeroes *)
  weight := 16;
  FOR n := 1 TO 0 BY -1 DO
    digit := value DIV weight;
    IF digit <= 10 THEN
      Terminal.Write(CHR(digit + 48))
    ELSE (* A .. F *)
      Terminal.Write(CHR(digit + 55))
    END; (* IF *)
    value := value MOD weight;
    weight := weight DIV 16
  END (* FOR *)
END WriteCharU;


(* ---------------------------------------------------------------------------
 * procedure WriteCard(value)
 * ---------------------------------------------------------------------------
 * Prints the given cardinal value to the console.
 * ------------------------------------------------------------------------ *)

PROCEDURE WriteCard ( value : CARDINAL );

VAR
  m, n, weight, digit : CARDINAL;

BEGIN
  (* base-10 exponent of highest possible digit *)
  m := maxExponentBase10(TSIZE(CARDINAL));
  
  (* skip any leading zeroes *)
  WHILE value DIV pow10(m) = 0 DO
    m := m - 1
  END; (* WHILE *)
  
  (* print digits *)
  weight := pow10(m);
  FOR n := m TO 0 BY -1 DO
    digit := value DIV weight;
    Terminal.WriteChar(CHR(digit + 48));
    value := value MOD weight;
    weight := weight DIV 10
  END (* FOR *)
END WriteCard;


(* ---------------------------------------------------------------------------
 * procedure WriteCardX(value)
 * ---------------------------------------------------------------------------
 * Prints the given cardinal value in 0x notation to the console.
 * ------------------------------------------------------------------------ *)

PROCEDURE WriteCardX ( value : CARDINAL );

VAR
  m, n, weight, digit : CARDINAL;
  
BEGIN
  (* base-16 exponent of highest possible digit *)
  m := maxExponentBase16(TSIZE(CARDINAL));
  
  (* print prefix *)
  WriteChars("0x");
  
  (* print digits, including any leading zeroes *)
  weight := pow16(n);
  FOR n := m TO 0 BY -1 DO
    digit := value DIV weight;
    IF digit <= 10 THEN
      Terminal.WriteChar(CHR(digit + 48))
    ELSE (* A .. F *)
      Terminal.WriteChar(CHR(digit + 55))
    END; (* IF *)
    value := value MOD weight;
    weight := weight DIV 16
  END (* FOR *)
END WriteCardX;


(* ---------------------------------------------------------------------------
 * procedure WriteInt(value)
 * ---------------------------------------------------------------------------
 * Prints the given integer value to the console.
 * ------------------------------------------------------------------------ *)

PROCEDURE WriteInt ( value : INTEGER );

BEGIN
  (* print sign if negative *)
  IF value < 0 THEN
    Terminal.WriteChar("-")
  END; (* IF *)
  
  (* print unsigned value *)
  WriteCard(abs(value))
END WriteInt;


(* ---------------------------------------------------------------------------
 * procedure WriteIntX(value)
 * ---------------------------------------------------------------------------
 * Prints the given integer value in 0x notation to the console.
 * ------------------------------------------------------------------------ *)

PROCEDURE WriteIntX ( value : INTEGER );

BEGIN
  (* if value is positive *)
  IF value >= 0 THEN
    (* print absolute value *)
    WriteCardX(abs(value))
  ELSE (* negative *)
    (* print two's complement of absolute value *)
    WriteCardX(MAX(CARDINAL) - abs(value) + 1
  END (* IF *)
END WriteIntX;


(* ---------------------------------------------------------------------------
 * procedure WriteLongInt(value)
 * ---------------------------------------------------------------------------
 * Prints the given long integer value to the console.
 * ------------------------------------------------------------------------ *)

PROCEDURE WriteLongInt ( value : LONGINT );

VAR
  m, n : CARDINAL;
  weight, digit : LONGINT;

BEGIN
  (* skip any leading zeroes *)
  m := maxExponentBase10(TSIZE(LONGINT));
  WHILE value DIV LongIntPow10(m) = 0 DO
    m := m - 1
  END; (* WHILE *)
  
  (* print sign if negative *)
  IF value < 0 THEN
    Terminal.Write("-")
  END; (* IF *)
  
  (* print digits *)
  weight := longIntPow10(m);
  FOR n := m TO 0 BY -1 DO
    (* ABS(value) would overflow on MAX(LONGINT) *)
    digit := ABS(value DIV weight);
    Terminal.Write(CHR(digit + 48));
    value := value MOD weight;
    weight := weight DIV 10
  END (* FOR *)
END WriteLongInt;


(* ---------------------------------------------------------------------------
 * procedure WriteLongIntX(value)
 * ---------------------------------------------------------------------------
 * Prints the given long integer value in 0x notation to the console.
 * ------------------------------------------------------------------------ *)

PROCEDURE WriteLongIntX ( value : LONGINT );

VAR
  m, msb, digitIndex, bitIndex, bitWeight, digit : CARDINAL;
  
BEGIN
  (* base-16 exponent of highest possible digit *)
  m := maxExponentBase16(TSIZE(LONGINT));
    
  (* print prefix *)  
  Terminal.WriteString("0x");
  
  (* process first digit *)
  
  (* test sign bit *)
  IF value < 0 THEN
    digit := 8
  ELSE
    digit := 0
  END; (* IF *)
  
  (* test next three bits *)
  msb := (m + 1) * 4 - 1; bitWeight := 4;
  FOR bitIndex := msb-1 TO msb-3 BY -1 DO
    (* test bit at position bitIndex *)
    IF ODD(value DIV longIntPow2(bitIndex)) THEN
      (* bit is set *)
      digit := digit + bitWeight
    END; (* IF *)
    bitWeight := bitWeight DIV 2
  END; (* FOR *)
  
  (* print this digit *)
  IF digit < 10 THEN
    Terminal.Write(CHR(digit + 48))
  ELSE (* A .. F *)
    Terminal.Write(CHR(digit + 55))
  END (* IF *)

  (* process remaining digits *)
  
  FOR digitIndex := m-1 TO 0 BY -1 DO
    (* test next 4-bit group *)
    msb := (digitIndex + 1) * 4 - 1;
    digit := 0; bitWeight := 8;
    FOR bitIndex := msb TO msb-3 BY -1 DO
      (* test bit at position bitIndex *)
      IF ODD(value DIV longIntPow2(bitIndex)) THEN
        (* bit is set *)
        digit := digit + bitWeight
      END; (* IF *)
      bitWeight := bitWeight DIV 2
    END; (* FOR *)
    
    (* print this digit *)
    IF digit < 10 THEN
      Terminal.Write(CHR(digit + 48))
    ELSE (* A .. F *)
      Terminal.Write(CHR(digit + 55))
    END (* IF *)
  END (* FOR *)
END WriteLongIntX;


(* ************************************************************************ *
 * Private Operations                                                       *
 * ************************************************************************ *)

(* ---------------------------------------------------------------------------
 * function pow16(n)
 * ---------------------------------------------------------------------------
 * Returns the power of 16 for argument n.
 * ------------------------------------------------------------------------ *)

PROCEDURE pow16 ( n : CARDINAL ) : CARDINAL;

BEGIN
  RETURN pow2(n DIV 4)
END pow16;


(* ---------------------------------------------------------------------------
 * function maxExponentBase10(bitwidth)
 * ---------------------------------------------------------------------------
 * Returns the largest base-10 exponent for an unsigned number of bitwidth
 * ------------------------------------------------------------------------ *)

PROCEDURE maxExponentBase10 ( bitwidth : CARDINAL ) : CARDINAL;

BEGIN
  RETURN deg10(bitwidth DIV 8)
END maxExponentBase10;


(* ---------------------------------------------------------------------------
 * function maxExponentBase16(bitwidth)
 * ---------------------------------------------------------------------------
 * Returns the largest base-16 exponent for an unsigned number of bitwidth
 * ------------------------------------------------------------------------ *)

PROCEDURE maxExponentBase16 ( bitwidth : CARDINAL ) : CARDINAL;

BEGIN
  RETURN bitwidth DIV 4
END maxExponentBase16;


END Console.