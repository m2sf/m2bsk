(*!m2pim*) (* Copyright (c) 2016 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE Console;

(* Console output library *)

IMPORT ASCII, Terminal, String;

FROM String IMPORT StringT; (* alias for String.String *)


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
  IF (s # NIL) AND (String.length(s) > 0)
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
  IF (s # NIL) AND (String.length(s) > 0)
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

BEGIN
  (* TO DO *)
END WriteCharU;


(* ---------------------------------------------------------------------------
 * procedure WriteCard(value)
 * ---------------------------------------------------------------------------
 * Prints the given cardinal value to the console.
 * ------------------------------------------------------------------------ *)

PROCEDURE WriteCard ( value : CARDINAL );

BEGIN
  (* TO DO *)
END WriteCard;


(* ---------------------------------------------------------------------------
 * procedure WriteCardX(chars)
 * ---------------------------------------------------------------------------
 * Prints the given cardinal value in 0x notation to the console.
 * ------------------------------------------------------------------------ *)

PROCEDURE WriteCardX ( char : CHAR );

BEGIN
  (* TO DO *)
END WriteCardX;


(* ---------------------------------------------------------------------------
 * procedure WriteInt(value)
 * ---------------------------------------------------------------------------
 * Prints the given integer value to the console.
 * ------------------------------------------------------------------------ *)

PROCEDURE WriteInt ( value : INTEGER );

BEGIN
  (* TO DO *)
END WriteInt;


(* ---------------------------------------------------------------------------
 * procedure WriteIntX(value)
 * ---------------------------------------------------------------------------
 * Prints the given integer value in 0x notation to the console.
 * ------------------------------------------------------------------------ *)

PROCEDURE WriteIntX ( value : INTEGER );

BEGIN
  (* TO DO *)
END WriteIntX;


(* ---------------------------------------------------------------------------
 * procedure WriteLongInt(value)
 * ---------------------------------------------------------------------------
 * Prints the given long integer value to the console.
 * ------------------------------------------------------------------------ *)

PROCEDURE WriteLongInt ( value : LONGINT );

BEGIN
  (* TO DO *)
END WriteLongInt;


(* ---------------------------------------------------------------------------
 * procedure WriteLongIntX(value)
 * ---------------------------------------------------------------------------
 * Prints the given long integer value in 0x notation to the console.
 * ------------------------------------------------------------------------ *)

PROCEDURE WriteLongIntX ( value : LONGINT );

BEGIN
  (* TO DO *)
END WriteLongIntX;


END Console.