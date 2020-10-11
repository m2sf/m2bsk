(*!m2pim*) (* Copyright (c) 2020 Modula-2 Software Foundation. *)

IMPLEMENTATION MODULE LongHash; (* 64-bit LONGINT version *)

(* General Purpose 64-bit Hash Function *)


IMPORT ISO646, Console;

FROM SYSTEM IMPORT TSIZE;
FROM LongIntBitOps IMPORT shl, ClearBit;


(* ---------------------------------------------------------------------------
 * Initial value for incremental hash calculation
 * ------------------------------------------------------------------------ *)

CONST initialValue = 0;


(* ---------------------------------------------------------------------------
 * Procedure:  LongHash.SetInitialValue( hash )
 * ------------------------------------------------------------------------ *)

PROCEDURE SetInitialValue ( VAR hash : Key );

BEGIN
  hash := initialValue
END SetInitialValue;


(* ---------------------------------------------------------------------------
 * Procedure:  LongHash.ValueForNextChar( hash, ch )
 * ------------------------------------------------------------------------ *)

PROCEDURE ValueForNextChar ( VAR hash : Key; ch : CHAR );

BEGIN
  hash := VAL(Key, ORD(ch)) + shl(hash, 6) + shl(hash, 16) - hash
END valueForNextChar;


(* ---------------------------------------------------------------------------
 * Procedure:  LongHash.SetFinalValue( hash )
 * ------------------------------------------------------------------------ *)

PROCEDURE SetFinalValue ( VAR hash : Key );

BEGIN
  (* Clear highest bit in hash value *)
  ClearBit(hash, Bitwidth-1)
END finalValue;


(* ---------------------------------------------------------------------------
 * Procedure:  Hash.ValueForArray( hash, array )
 * ------------------------------------------------------------------------ *)

PROCEDURE ValueForArray
  ( VAR hash : Key; VAR (* CONST *) array : ARRAY OF CHAR );

VAR
  ch : CHAR;
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
  ClearBit(hash, Bitwidth-1)  
END valueForArray;


(* ---------------------------------------------------------------------------
 * module initialisation
 * ------------------------------------------------------------------------ *)

BEGIN (* LongHash *)
  (* assert that Key is 64-bit wide *)
  IF TSIZE(Key) # 64 THEN
    Console.WriteChars("Library LongHash requires 64-bit LONGINT.");
    Console.WriteLn;
    HALT
  END (* IF *)
END LongHash.