(*!m2pim*) (* Copyright (c) 2020 Modula-2 Software Foundation. *)

IMPLEMENTATION MODULE Hash; (* 32-bit LONGINT version *)

(* General Purpose 32-bit Hash Function *)


IMPORT ISO646, Console;

FROM SYSTEM IMPORT TSIZE;
FROM LongIntBitOps IMPORT shl, ClearBit;


(* ---------------------------------------------------------------------------
 * function:  Hash.valueForNextChar( hash, ch )
 * ------------------------------------------------------------------------ *)

PROCEDURE valueForNextChar ( hash : Key; ch : CHAR ) : Key;

BEGIN
  RETURN VAL(Key, ORD(ch)) + shl(hash, 6) + shl(hash, 16) - hash
END valueForNextChar;


(* ---------------------------------------------------------------------------
 * function:  Hash.finalValue( hash )
 * ------------------------------------------------------------------------ *)

PROCEDURE finalValue ( hash : Key ) : Key;

BEGIN
  (* Clear highest bit in hash value *)
  ClearBit(hash, Bitwidth-1);
  
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
  WHILE (ch # ISO646.NUL) AND (index < HIGH(array)) DO
    hash := VAL(Key, (ORD(ch)) + shl(hash, 6) + shl(hash, 16) - hash;
    index := index + 1;
    ch := array[index]
  END; (* WHILE *)
  
  (* Clear highest bit in hash value *)
  ClearBit(hash, Bitwidth-1);
  
  RETURN hash
END valueForArray;


(* ---------------------------------------------------------------------------
 * module initialisation
 * ------------------------------------------------------------------------ *)

BEGIN (* Hash *)
  (* assert that Key is 32-bit wide *)
  IF TSIZE(Key) # 32 THEN
    Console.WriteChars("Library Hash requires 32-bit LONGINT.");
    Console.WriteLn;
    HALT
  END (* IF *)
END Hash.