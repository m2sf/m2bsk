(*!m2pim*) (* Copyright (c) 2015 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE Resword;

IMPORT Token, String;

FROM Token IMPORT TokenT;
FROM String IMPORT StringT;

VAR lexeme : ARRAY [Token.Alias .. Token.Write] OF StringT;


PROCEDURE tokenForLexeme ( lexeme : StringT; defaultToken : TokenT ) : TokenT;
(* If lexeme represents a reserved word, its corresponding reserved word's token
   value is returned, otherwise the value of defaultToken is returned. *)

BEGIN
  IF lexeme = NIL THEN
    (* TO DO : error message *)
    HALT
  END; (* IF *)

  CASE String.length(lexeme) OF
    2 :
    CASE String.charAtIndex(lexeme, 0) OF
      'D' :

      (* 'DO' *)

      IF lexeme = do THEN
        RETURN Token.Do
      END (* IF *)

    | 'I' :
 
      (* 'IF' *)
      
      IF lexeme = if THEN
        RETURN Token.If
      
      (* 'IN' *)
      
      ELSIF lexeme = in THEN
        RETURN Token.In
      END (* IF *)
    
    | 'O' :

      (* 'OF' *)
      
      IF lexeme = of THEN
        RETURN Token.Of
      
      (* 'OR' *)
      
      ELSIF lexeme = or THEN
        RETURN Token.Or
      END (* IF *)
    
    | 'T' :

      (* 'TO' *)
      
      IF lexeme = to THEN
        RETURN Token.To
      END (* IF *)

    END (* CASE *)

  | 3 :
    CASE String.charAtIndex(lexeme, 0) OF
      'A' :
    
      (* 'AND' *)
      
      IF lexeme = and THEN
        RETURN Token.And
      END (* IF *)

    | 'D' :

      (* 'DIV' *)
      
      IF lexeme = div THEN
        RETURN Token.Div
      END (* IF *)

    | 'E' :
      
      (* 'END' *)
      
      IF lexeme = end THEN
        RETURN Token.End
      END (* IF *)
      
    | 'F' :
    
      (* 'FOR' *)
      
      IF lexeme = for THEN
        RETURN Token.For
      END (* IF *)

    | 'M' :

      (* 'MOD' *)
      
      IF lexeme = mod THEN
        RETURN Token.Mod
      END (* IF *)

    | 'N' :
     
      (* 'NEW' *)
      
      IF lexeme = new THEN
          RETURN Token.New
        END (* IF *)
      
      (* 'NOP' *)
      
      ELSIF lexeme = nop THEN
        RETURN Token.Nop

      (* 'NOT' *)
      
      ELSIF lexeme = not THEN
        RETURN Token.Not
      END (* IF *)

    | 'S' :

      (* 'SET' *)
      
      IF lexeme = set THEN
        RETURN Token.Set
      END (* IF *)

    | 'V' :

      (* 'VAR' *)
      
      IF lexeme = var THEN
        RETURN Token.Var
      END (* IF *)
    
    END (* CASE *)
  
  | 4 :
    CASE String.charAtIndex(lexeme, 1) OF
      'A' :

      (* 'CASE' *)
      
      IF lexeme = case THEN
        RETURN Token.Case
      END (* IF *)

    | 'E' :

      (* 'READ' *)
      
      IF lexeme = read THEN
        RETURN Token.Read
      END (* IF *)

    | 'H' :

      (* 'THEN' *)
      
      IF lexeme = then THEN
        RETURN Token.Then
      END (* IF *)

    | 'L' :
      
      (* 'ELSE' *)
      
      IF lexeme = else THEN
        RETURN Token.Else
      END (* IF *)

    | 'O' :
 
      (* 'COPY' *)
      
      IF lexeme = copy THEN
        RETURN Token.Copy

      (* 'LOOP' *)
      
      ELSIF lexeme = loop THEN
        RETURN Token.Loop
      END (* IF *)

    | 'X' :
      
      (* 'EXIT' *)
      
      IF lexeme = exit THEN
        RETURN Token.Exit
      END (* IF *)
      
    | 'Y' :
      
      (* 'TYPE' *)
      
      IF lexeme = type THEN
        RETURN Token.Type
      END (* IF *)
      
    END (* CASE *)
  
  | 5 :
    CASE String.charAtIndex(lexeme, 4) OF
      'E' :

      (* 'WHILE' *)
      
      IF lexeme = while THEN
        RETURN Token.While

      (* WRITE *)
      
      ELSIF lexeme = write THEN
        RETURN Token.Write
      END (* IF *)

    | 'F' :
    
      (* 'ELSIF' *)
      
      IF lexeme = elsif THEN
        RETURN Token.Elsif
      END (* IF *)
    
    | 'L' :
    
      (* 'UNTIL' *)
      
      IF lexeme = until THEN
        RETURN Token.Until
      END (* IF *)
    
    | 'N' :
    
      (* 'BEGIN' *)
      
      IF lexeme = begin THEN
        RETURN Token.Begin
      END (* IF *)
    
    | 'S' :
    
      (* 'ALIAS' *)
      
      IF lexeme = alias THEN
        RETURN Token.Alias
      END (* IF *)
    
    | 'T' :
    
      (* 'CONST' *)
      
      IF lexeme = const THEN
        RETURN Token.Const
      END (* IF *)
    
    | 'Y' :
    
      (* 'ARRAY' *)
      
      IF lexeme = array THEN
        RETURN Token.Array
      END (* IF *)
    
    END (* CASE *)

  | 6 :
    CASE String.charAtIndex(lexeme, 2) OF
      'A' :
    
      (* 'OPAQUE' *)
      
      IF lexeme = opaque THEN
        RETURN Token.Opaque
      END (* IF *)

    | 'C' :
    
      (* 'RECORD' *)
      
      IF lexeme = record THEN
        RETURN Token.Record
      END (* IF *)

    | 'D' :

      (* 'MODULE' *)
      IF lexeme = module THEN
        RETURN Token.Module
      END (* IF *)

    | 'P' :
     
      (* 'IMPORT' *)
      
      IF lexeme = import THEN
        RETURN Token.Import
      END (* IF *)

      (* 'REPEAT' *)
      
      ELSIF lexeme = repeat THEN
        RETURN Token.Repeat
      END (* IF *)

    | 'T' :

      (* 'RETAIN' *)
      
      IF lexeme = retain THEN
        RETURN Token.Retain

      (* 'RETURN' *)
      
      ELSIF lexeme = return THEN
        RETURN Token.Return
      END (* IF *)
    
    END (* CASE *)

  | 7 :
    CASE String.charAtIndex(lexeme, 7) OF
      'A' :
    
      (* 'ARGLIST' *)
      
      IF lexeme = arglist THEN
        RETURN Token.Arglist
      END (* IF *)
      
    | 'P' :
    
      (* 'POINTER' *)
      
      IF lexeme = pointer THEN
        RETURN Token.Pointer
      END (* IF *)
      
    | 'R' :
    
      (* 'RELEASE' *)
      
      IF lexeme = release THEN
        RETURN Token.Release
      END (* IF *)
      
    END (* CASE *)
    
  | 8 :

    (* 'OCTETSEQ' *)
    
    IF lexeme = octetseq THEN
      RETURN Token.Octetseq
    END (* IF *)
    
  | 9 :

    (* 'PROCEDURE' *)
    
    IF lexeme = procedure THEN
      RETURN Token.Procedure
    END (* IF *)
    
  | 10 :

    (* 'DEFINITION' *)
    
    IF lexeme = definition THEN
      RETURN Token.Definition
    END (* IF *)
    
  | 14 :

    (* 'IMPLEMENTATION' *)
    
    IF lexeme = implementation THEN
      RETURN Token.Implementation
    END (* IF *)

  END; (* CASE *)

  (* no match *)
  RETURN defaultToken
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
  lexeme[Token.Alias] := alias;

  and := String.forArray("AND");
  lexeme[Token.And] := and;

  arglist := String.forArray("ARGLIST");
  lexeme[Token.Arglist] := arglist;

  array := String.forArray("ARRAY");
  lexeme[Token.Array] := array;

  begin := String.forArray("BEGIN");
  lexeme[Token.Begin] := begin;

  case := String.forArray("CASE");
  lexeme[Token.Case] := case;

  const := String.forArray("CONST");
  lexeme[Token.Const] := const;

  copy := String.forArray("COPY");
  lexeme[Token.Copy] := copy;

  definition := String.forArray("DEFINITION");
  lexeme[Token.Definition] := definition;

  div := String.forArray("DIV");
  lexeme[Token.Div] := div;

  do := String.forArray("DO");
  lexeme[Token.Do] := do;

  else := String.forArray("ELSE");
  lexeme[Token.Else] := else;

  elsif := String.forArray("ELSIF");
  lexeme[Token.Elsif] := elsif;

  end := String.forArray("END");
  lexeme[Token.End] := end;

  exit := String.forArray("EXIT");
  lexeme[Token.Exit] := exit;

  for := String.forArray("FOR");
  lexeme[Token.For] := for;

  if := String.forArray("IF");
  lexeme[Token.If] := if;

  implementation := String.forArray("IMPLEMENTATION");
  lexeme[Token.Implementation] := implementation;

  import := String.forArray("IMPORT");
  lexeme[Token.Import] := import;

  in := String.forArray("IN");
  lexeme[Token.In] := in;

  loop := String.forArray("LOOP");
  lexeme[Token.Loop] := loop;

  mod := String.forArray("MOD");
  lexeme[Token.Mod] := mod;

  module := String.forArray("MODULE");
  lexeme[Token.Module] := module;

  new := String.forArray("NEW");
  lexeme[Token.New] := new;

  nop := String.forArray("NOP");
  lexeme[Token.Nop] := nop;

  not := String.forArray("NOT");
  lexeme[Token.Not] := not;

  octetseq := String.forArray("OCTETSEQ");
  lexeme[Token.Octetseq] := octetseq;

  of := String.forArray("OF");
  lexeme[Token.Of] := of;

  opaque := String.forArray("OPAQUE");
  lexeme[Token.Opaque] := opaque;

  or := String.forArray("OR");
  lexeme[Token.Or] := or;

  pointer := String.forArray("POINTER");
  lexeme[Token.Pointer] := pointer;

  procedure := String.forArray("PROCEDURE");
  lexeme[Token.Procedure] := procedure;

  read := String.forArray("READ");
  lexeme[Token.Read] := read;

  record := String.forArray("RECORD");
  lexeme[Token.Record] := record;

  release := String.forArray("RELEASE");
  lexeme[Token.Release] := release;

  repeat := String.forArray("REPEAT");
  lexeme[Token.Repeat] := repeat;

  retain := String.forArray("RETAIN");
  lexeme[Token.Retain] := retain;

  return := String.forArray("RETURN");
  lexeme[Token.Return] := return;

  set := String.forArray("SET");
  lexeme[Token.Set] := set;

  then := String.forArray("THEN");
  lexeme[Token.Then] := then;

  to := String.forArray("TO");
  lexeme[Token.To] := to;

  type := String.forArray("TYPE");
  lexeme[Token.Type] := type;

  until := String.forArray("UNTIL");
  lexeme[Token.Until] := until;

  var := String.forArray("VAR");
  lexeme[Token.Var] := var;

  while := String.forArray("WHILE");
  lexeme[Token.While] := while;

  write := String.forArray("WRITE");
  lexeme[Token.Write] := write
END Resword.
