(*!m2pim*) (* Copyright (c) 2015 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE Resword;

IMPORT Token, String;

FROM Token IMPORT TokenT;

FROM String IMPORT StringT;

VAR lexeme : ARRAY [TokenT.Alias .. TokenT.Write] OF StringT;


PROCEDURE tokenForLexeme ( lexeme : StringT ) : TokenT;
(* If lexeme represents a reserved word, its corresponding reserved word's token
   value is returned, otherwise token value TokenT.Unknown is returned. *)

BEGIN
  IF lexeme = NIL THEN 
    RETURN TokenT.Unknown
  END; (* IF *)

  CASE String.length(lexeme) OF
    2 :
    CASE String.charAtIndex(lexeme, 0) OF
      'D' :

      (* 'DO' *)

      IF lexeme = do THEN
        RETURN TokenT.Do
      END (* IF *)

    | 'I' :
 
      (* 'IF' *)
      
      IF lexeme = if THEN
        RETURN TokenT.If
      
      (* 'IN' *)
      
      ELSIF lexeme = in THEN
        RETURN TokenT.In
      END (* IF *)
    
    | 'O' :

      (* 'OF' *)
      
      IF lexeme = of THEN
        RETURN TokenT.Of
      
      (* 'OR' *)
      
      ELSIF lexeme = or THEN
        RETURN TokenT.Or
      END (* IF *)
    
    | 'T' :

      (* 'TO' *)
      
      IF lexeme = to THEN
        RETURN TokenT.To
      END (* IF *)

    END (* CASE *)

  | 3 :
    CASE String.charAtIndex(lexeme, 0) OF
      'A' :
    
      (* 'AND' *)
      
      IF lexeme = and THEN
        RETURN TokenT.And
      END (* IF *)

    | 'D' :

      (* 'DIV' *)
      
      IF lexeme = div THEN
        RETURN TokenT.Div
      END (* IF *)

    | 'E' :
      
      (* 'END' *)
      
      IF lexeme = end THEN
        RETURN TokenT.End
      END (* IF *)
      
    | 'F' :
    
      (* 'FOR' *)
      
      IF lexeme = for THEN
        RETURN TokenT.For
      END (* IF *)

    | 'M' :

      (* 'MOD' *)
      
      IF lexeme = mod THEN
        RETURN TokenT.Mod
      END (* IF *)

    | 'N' :
     
      (* 'NEW' *)
      
      IF lexeme = new THEN
          RETURN TokenT.New
        END (* IF *)
      
      (* 'NOP' *)
      
      ELSIF lexeme = nop THEN
        RETURN TokenT.Nop

      (* 'NOT' *)
      
      ELSIF lexeme = not THEN
        RETURN TokenT.Not
      END (* IF *)

    | 'S' :

      (* 'SET' *)
      
      IF lexeme = set THEN
        RETURN TokenT.Set
      END (* IF *)

    | 'V' :

      (* 'VAR' *)
      
      IF lexeme = var THEN
        RETURN TokenT.Var
      END (* IF *)
    
    END (* CASE *)
  
  | 4 :
    CASE String.charAtIndex(lexeme, 1) OF
      'A' :

      (* 'CASE' *)
      
      IF lexeme = case THEN
        RETURN TokenT.Case
      END (* IF *)

    | 'E' :

      (* 'READ' *)
      
      IF lexeme = read THEN
        RETURN TokenT.Read
      END (* IF *)

    | 'H' :

      (* 'THEN' *)
      
      IF lexeme = then THEN
        RETURN TokenT.Then
      END (* IF *)

    | 'L' :
      
      (* 'ELSE' *)
      
      IF lexeme = else THEN
        RETURN TokenT.Else
      END (* IF *)

    | 'O' :
 
      (* 'COPY' *)
      
      IF lexeme = copy THEN
        RETURN TokenT.Copy

      (* 'LOOP' *)
      
      ELSIF lexeme = loop THEN
        RETURN TokenT.Loop
      END (* IF *)

    | 'X' :
      
      (* 'EXIT' *)
      
      IF lexeme = exit THEN
        RETURN TokenT.Exit
      END (* IF *)
      
    | 'Y' :
      
      (* 'TYPE' *)
      
      IF lexeme = type THEN
        RETURN TokenT.Type
      END (* IF *)
      
    END (* CASE *)
  
  | 5 :
    CASE String.charAtIndex(lexeme, 4) OF
      'E' :

      (* 'WHILE' *)
      
      IF lexeme = while THEN
        RETURN TokenT.While

      (* WRITE *)
      
      ELSIF lexeme = write THEN
        RETURN TokenT.Write
      END (* IF *)

    | 'F' :
    
      (* 'ELSIF' *)
      
      IF lexeme = elsif THEN
        RETURN TokenT.Elsif
      END (* IF *)
    
    | 'L' :
    
      (* 'UNTIL' *)
      
      IF lexeme = until THEN
        RETURN TokenT.Until
      END (* IF *)
    
    | 'N' :
    
      (* 'BEGIN' *)
      
      IF lexeme = begin THEN
        RETURN TokenT.Begin
      END (* IF *)
    
    | 'S' :
    
      (* 'ALIAS' *)
      
      IF lexeme = alias THEN
        RETURN TokenT.Alias
      END (* IF *)
    
    | 'T' :
    
      (* 'CONST' *)
      
      IF lexeme = const THEN
        RETURN TokenT.Const
      END (* IF *)
    
    | 'Y' :
    
      (* 'ARRAY' *)
      
      IF lexeme = array THEN
        RETURN TokenT.Array
      END (* IF *)
    
    END (* CASE *)

  | 6 :
    CASE String.charAtIndex(lexeme, 2) OF
      'A' :
    
      (* 'OPAQUE' *)
      
      IF lexeme = opaque THEN
        RETURN TokenT.Opaque
      END (* IF *)

    | 'C' :
    
      (* 'RECORD' *)
      
      IF lexeme = record THEN
        RETURN TokenT.Record
      END (* IF *)

    | 'D' :

      (* 'MODULE' *)
      IF lexeme = module THEN
        RETURN TokenT.Module
      END (* IF *)

    | 'P' :
     
      (* 'IMPORT' *)
      
      IF lexeme = import THEN
        RETURN TokenT.Import
      END (* IF *)

      (* 'REPEAT' *)
      
      ELSIF lexeme = repeat THEN
        RETURN TokenT.Repeat
      END (* IF *)

    | 'T' :

      (* 'RETAIN' *)
      
      IF lexeme = retain THEN
        RETURN TokenT.Retain

      (* 'RETURN' *)
      
      ELSIF lexeme = return THEN
        RETURN TokenT.Return
      END (* IF *)
    
    END (* CASE *)

  | 7 :
    CASE String.charAtIndex(lexeme, 7) OF
      'A' :
    
      (* 'ARGLIST' *)
      
      IF lexeme = arglist THEN
        RETURN TokenT.Arglist
      END (* IF *)
      
    | 'P' :
    
      (* 'POINTER' *)
      
      IF lexeme = pointer THEN
        RETURN TokenT.Pointer
      END (* IF *)
      
    | 'R' :
    
      (* 'RELEASE' *)
      
      IF lexeme = release THEN
        RETURN TokenT.Release
      END (* IF *)
      
    END (* CASE *)
    
  | 8 :

    (* 'OCTETSEQ' *)
    
    IF lexeme = octetseq THEN
      RETURN TokenT.Octetseq
    END (* IF *)
    
  | 9 :

    (* 'PROCEDURE' *)
    
    IF lexeme = procedure THEN
      RETURN TokenT.Procedure
    END (* IF *)
    
  | 10 :

    (* 'DEFINITION' *)
    
    IF lexeme = definition THEN
      RETURN TokenT.Definition
    END (* IF *)
    
  | 14 :

    (* 'IMPLEMENTATION' *)
    
    IF lexeme = implementation THEN
      RETURN TokenT.Implementation
    END (* IF *)

  END; (* CASE *)

  (* no match *)
  RETURN TokenT.Unknown
END tokenForLexeme;


PROCEDURE lexemeForToken ( token : TokenT ) : StringT;
(* If token represents a reserved word, an interned string with its corresponding
   lexeme is returned, otherwise NIL is returned. *)

BEGIN
  IF Token.isResword(token) THEN
    RETURN lexeme[token]
  ELSE
    RETURN NIL
  END (* IF *)
END lexemeForToken;


BEGIN (* initialise lexemes *)
  alias := String.forArray("ALIAS");
  lexeme[TokenT.Alias] := alias;

  and := String.forArray("AND");
  lexeme[TokenT.And] := and;

  arglist := String.forArray("ARGLIST");
  lexeme[TokenT.Arglist] := arglist;

  array := String.forArray("ARRAY");
  lexeme[TokenT.Array] := array;

  begin := String.forArray("BEGIN");
  lexeme[TokenT.Begin] := begin;

  case := String.forArray("CASE");
  lexeme[TokenT.Case] := case;

  const := String.forArray("CONST");
  lexeme[TokenT.Const] := const;

  copy := String.forArray("COPY");
  lexeme[TokenT.Copy] := copy;

  definition := String.forArray("DEFINITION");
  lexeme[TokenT.Definition] := definition;

  div := String.forArray("DIV");
  lexeme[TokenT.Div] := div;

  do := String.forArray("DO");
  lexeme[TokenT.Do] := do;

  else := String.forArray("ELSE");
  lexeme[TokenT.Else] := else;

  elsif := String.forArray("ELSIF");
  lexeme[TokenT.Elsif] := elsif;

  end := String.forArray("END");
  lexeme[TokenT.End] := end;

  exit := String.forArray("EXIT");
  lexeme[TokenT.Exit] := exit;

  for := String.forArray("FOR");
  lexeme[TokenT.For] := for;

  if := String.forArray("IF");
  lexeme[TokenT.If] := if;

  implementation := String.forArray("IMPLEMENTATION");
  lexeme[TokenT.Implementation] := implementation;

  import := String.forArray("IMPORT");
  lexeme[TokenT.Import] := import;

  in := String.forArray("IN");
  lexeme[TokenT.In] := in;

  loop := String.forArray("LOOP");
  lexeme[TokenT.Loop] := loop;

  mod := String.forArray("MOD");
  lexeme[TokenT.Mod] := mod;

  module := String.forArray("MODULE");
  lexeme[TokenT.Module] := module;

  new := String.forArray("NEW");
  lexeme[TokenT.New] := new;

  nop := String.forArray("NOP");
  lexeme[TokenT.Nop] := nop;

  not := String.forArray("NOT");
  lexeme[TokenT.Not] := not;

  octetseq := String.forArray("OCTETSEQ");
  lexeme[TokenT.Octetseq] := octetseq;

  of := String.forArray("OF");
  lexeme[TokenT.Of] := of;

  opaque := String.forArray("OPAQUE");
  lexeme[TokenT.Opaque] := opaque;

  or := String.forArray("OR");
  lexeme[TokenT.Or] := or;

  pointer := String.forArray("POINTER");
  lexeme[TokenT.Pointer] := pointer;

  procedure := String.forArray("PROCEDURE");
  lexeme[TokenT.Procedure] := procedure;

  read := String.forArray("READ");
  lexeme[TokenT.Read] := read;

  record := String.forArray("RECORD");
  lexeme[TokenT.Record] := record;

  release := String.forArray("RELEASE");
  lexeme[TokenT.Release] := release;

  repeat := String.forArray("REPEAT");
  lexeme[TokenT.Repeat] := repeat;

  retain := String.forArray("RETAIN");
  lexeme[TokenT.Retain] := retain;

  return := String.forArray("RETURN");
  lexeme[TokenT.Return] := return;

  set := String.forArray("SET");
  lexeme[TokenT.Set] := set;

  then := String.forArray("THEN");
  lexeme[TokenT.Then] := then;

  to := String.forArray("TO");
  lexeme[TokenT.To] := to;

  type := String.forArray("TYPE");
  lexeme[TokenT.Type] := type;

  until := String.forArray("UNTIL");
  lexeme[TokenT.Until] := until;

  var := String.forArray("VAR");
  lexeme[TokenT.Var] := var;

  while := String.forArray("WHILE");
  lexeme[TokenT.While] := while;

  write := String.forArray("WRITE");
  lexeme[TokenT.Write] := write
END Resword.
