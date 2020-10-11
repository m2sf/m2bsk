(*!m2pim*) (* Copyright (c) 2020 Modula-2 Software Foundation. *)

IMPLEMENTATION MODULE LongHash; (* CARD64 version *)

(* General Purpose 64-bit Hash Function *)


IMPORT ISO646, CARD64, Console;

FROM SYSTEM IMPORT TSIZE;


(* ---------------------------------------------------------------------------
 * Initial value for incremental hash calculation
 * ------------------------------------------------------------------------ *)

CONST initialValue = 0;


(* ---------------------------------------------------------------------------
 * Procedure:  LongHash.SetInitialValue( hash )
 * ------------------------------------------------------------------------ *)

PROCEDURE SetInitialValue ( VAR hash : Key );

BEGIN
  CARD64.FromCard(hash, initialValue)
END SetInitialValue;


(* ---------------------------------------------------------------------------
 * Procedure:  LongHash.ValueForNextChar( hash, ch )
 * ------------------------------------------------------------------------ *)

PROCEDURE ValueForNextChar ( VAR hash : Key; ch : CHAR );

VAR
  n, result : Key;
  
BEGIN
  (* hash := ORD(ch) + SHL(hash, 6) + SHL(hash, 16) - hash *)
  
  (* result := ORD(ch) *)
  CARD64.FromCard(result, ORD(ch));
  
  (* result := result + SHL(hash, 6) *)
  n := hash; CARD64.Shl(n, 6); CARD64.Add(result, n);
  
  (* result := result + SHL(hash, 16) *)
  n := hash; CARD64.Shl(n, 16); CARD64.Add(result, n);
  
  (* result := result - hash *)
  CARD64.Sub(result, hash);
  
  hash := result
END ValueForNextChar;


(* ---------------------------------------------------------------------------
 * Procedure:  LongHash.SetFinalValue( hash )
 * ------------------------------------------------------------------------ *)

PROCEDURE SetFinalValue ( VAR hash : Key );

BEGIN
  (* Clear highest bit in hash value *)
  CARD64.ClearBit(hash, Bitwidth-1)
END finalValue;


(* ---------------------------------------------------------------------------
 * Procedure:  Hash.ValueForArray( hash, array )
 * ------------------------------------------------------------------------ *)

PROCEDURE ValueForArray
  ( VAR hash : Key; VAR (* CONST *) array : ARRAY OF CHAR );

VAR
  ch : CHAR;
  index : CARDINAL;
  n, newHash : Key;
  
BEGIN
  index := 0;
  SetInitialValue(hash);
  
  ch := array[index]
  WHILE (ch # ISO646.NUL) AND (index < HIGH(array)) DO
    (* hash := ORD(ch) + SHL(hash, 6) + SHL(hash, 16) - hash *)
    
    (* newHash := ORD(ch) *)
    CARD64.FromCard(newHash, ORD(ch));

    (* newHash := newHash + SHL(hash, 6) *)
    n := hash; CARD64.Shl(n, 6); CARD64.Add(newHash, n);
    
    (* newHash := newHash + SHL(hash, 16) *)
    n := hash; CARD64.Shl(n, 16); CARD64.Add(newHash, n);
    
    (* newHash := newHash - hash *)
    CARD64.Sub(newHash, hash);

    (* prepare for next iteration *)
    index := index + 1;
    ch := array[index];
    hash := newHash
  END; (* WHILE *)
  
  (* Clear highest bit in hash value *)
  CARD64.ClearBit(hash, Bitwidth-1)
END ValueForArray;


(* ---------------------------------------------------------------------------
 * module initialisation
 * ------------------------------------------------------------------------ *)

BEGIN (* LongHash *)
  (* assert that Key is 64-bit wide *)
  IF TSIZE(Key) # 64 THEN
    Console.WriteChars("Library LongHash requires CARD64.");
    Console.WriteLn;
    HALT
  END (* IF *)
END LongHash.