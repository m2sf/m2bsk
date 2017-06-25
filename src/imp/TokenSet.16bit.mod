(*!m2pim*) (* Copyright (c) 2015 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE TokenSet; (* requires 16-bit CARDINAL *)

(* Token Set ADT for Modula-2 R10 Bootstrap Kernel *)

IMPORT Console;

FROM Token IMPORT TokenT; (* alias for Token.Token *)

FROM CardMath IMPORT pow2;
FROM SYSTEM IMPORT TSIZE;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;


CONST
  Bitwidth = 16;
  SegCount = Bitwidth ORD(MAX(TokenT)) DIV Bitwidth;


(* --------------------------------------------------------------------------
 * TokenSet type
 * ----------------------------------------------------------------------- *)

TYPE TokenSet = POINTER TO Descriptor;

TYPE Descriptor = RECORD
  count   : CARDINAL;
  segment : ARRAY [0..SegCount-1] OF CARDINAL
END; (* Descriptor *)


(* Operations *)

(* --------------------------------------------------------------------------
 * procedure NewFromRawData(set, seg5, seg4, seg3, seg2, seg1, seg0)
 * --------------------------------------------------------------------------
 * Passes a newly allocated and initialised TokenSet instance back in set.
 * The set is initalised from parameters segment5 to segment0 as follows:
 *  
 *   bit 127                                                bit 0
 *    v                                                        v
 *   [<---------------------------set-------------------------->]
 *   [segment5][segment4][segment3][segment2][segment1][segment0]
 *    ^         ^         ^         ^         ^         ^
 *   bit 15    bit 15    bit 15    bit 15    bit 15    bit 15
 *  
 * The bits in set correspond to the token values of type Token.
 * ----------------------------------------------------------------------- *)

PROCEDURE NewFromRawData
  ( VAR set : TokenSet;
    segment5, segment4, segment3, segment2, segment1, segment0 : CARDINAL );

VAR
  token : TokenT;
  newSet : TokenSetT;
  mask, highBitInSeg5, bit, segIndex : CARDINAL;
  
BEGIN
  (* allocate new set *)
  ALLOCATE(newSet, TSIZE(Descriptor));
  
  (* initialise segment0 to segment4 as passed in *)
  newSet^segment[0] := segment0;
  newSet^segment[1] := segment1;
  newSet^segment[2] := segment2;
  newSet^segment[3] := segment3;
  newSet^segment[4] := segment4;
  
  (* initialise segment5 by clearing unused higher bits *)
  
  (* determine highest token bit in segment2 *)
  highBitInSeg5 := ORD(MAX(TokenT)) MOD Bitwidth;
  
  (* shift lower bits out to the right *)
  mask := segment5 DIV (highBitInSeg5 + 1);
  
  (* shift them back, thereby clearing the low bits to obtain a mask *)
  mask := mask * pow2[highBitInSeg5 + 1];
  
  (* subtract the mask, thereby clearing the bits above the highest bit *)  
  newSet^segment[5] := segment5 - mask;
  
  (* count total number of bits to initialise counter *)
  newSet^.count := 0;
  FOR segIndex := 0 TO SegCount-1 DO
    FOR bit := 0 TO Bitwidth-1 DO
      IF ODD(segment[segIndex] DIV pow2(bit)) THEN
        newSet^.count := newSet^.count + 1
      END (* IF *)
    END (* FOR *)
  END (* FOR *)
  
  (* pass back new set *)
  set := newSet
END NewFromRawData;


(* --------------------------------------------------------------------------
 * procedure NewFromArray(set, tokenList)
 * --------------------------------------------------------------------------
 * Passes a newly allocated and initialised TokenSet instance back in set.
 * The set is initialised with the tokens passed in the tokenList array.
 * Passes back NIL if allocation is unsuccessful.
 * ----------------------------------------------------------------------- *)

PROCEDURE NewFromArray
  ( VAR set : TokenSet; tokenList : ARRAY OF Token );

BEGIN
  (* allocate new set *)
  ALLOCATE(newSet, TSIZE(Descriptor));
  
  (* initialise as an empty set *)
  newSet^.count := 0;
  newSet^.segment[0] := 0;
  newSet^.segment[1] := 0;
  newSet^.segment[2] := 0;
  newSet^.segment[3] := 0;
  newSet^.segment[4] := 0;
  newSet^.segment[5] := 0;
  
  (* add each token in tokenList to the new set *)
  FOR index := 0 TO HIGH(tokenList) DO
    Insert(newSet, tokenList[index])
  END (* FOR *)
  
  (* pass back new set *)
  set := newSet
END NewFromArray;


(* --------------------------------------------------------------------------
 * procedure Insert(set, token)
 * --------------------------------------------------------------------------
 * Inserts token into set.
 * ----------------------------------------------------------------------- *)

PROCEDURE Insert ( set : TokenSet; token : TokenT );

VAR
  segIndex, bit, pow2bit : CARDINAL;
  
BEGIN
  (* bail out if set is invalid *)
  IF set = NIL THEN
    RETURN
  END; (* IF *)
  
  (* determine segment and bit where token is stored *)
  segIndex := ORD(token) DIV Bitwidth;
  bit := ORD(token) MOD Bitwidth;
  pow2bit := pow2(bit);
  
  (* test bit in segment *)
  IF ODD(set^.segment[segIndex] DIV pow2bit) (* bit is set *) THEN
    RETURN
  ELSE (* bit is not set *)
    (* set the bit *)
    set^.segment[segIndex] := set^.segment[segIndex] + pow2bit;
    
    (* update counter *)
    set^.count := set^.count + 1
  END (* END *)
END Insert;


(* --------------------------------------------------------------------------
 * procedure Remove(set, token)
 * --------------------------------------------------------------------------
 * Removes token from set.
 * ----------------------------------------------------------------------- *)

PROCEDURE Remove ( set : TokenSet; token : TokenT );

VAR
  segIndex, bit, pow2bit : CARDINAL;
  
BEGIN
  (* bail out if set is invalid *)
  IF set = NIL THEN
    RETURN
  END; (* IF *)
  
  (* determine segment and bit where token is stored *)
  segIndex := ORD(token) DIV Bitwidth;
  bit := ORD(token) MOD Bitwidth;
  pow2bit := pow2(bit);
  
  (* test bit in segment *)
  IF ODD(set^.segment[segIndex] DIV pow2bit) (* bit is set *) THEN
    (* clear the bit *)
    set^.segment[segIndex] := set^.segment[segIndex] - pow2bit;
    
    (* update counter *)
    set^.count := set^.count - 1
  END (* END *)
END Remove;


(* --------------------------------------------------------------------------
 * function isEmpty(set)
 * --------------------------------------------------------------------------
 * Returns TRUE if set is empty, otherwise FALSE.
 * ----------------------------------------------------------------------- *)

PROCEDURE isEmpty ( set : TokenSet ) : BOOLEAN;

BEGIN
  IF set = NIL THEN
    RETURN TRUE
  END; (* IF *)
  
  RETURN (set^.count = 0)
END isEmpty;


(* --------------------------------------------------------------------------
 * function isElem(set)
 * --------------------------------------------------------------------------
 * Returns TRUE if token is an element of set, otherwise FALSE.
 * ----------------------------------------------------------------------- *)

PROCEDURE isElem ( set : TokenSet; token : TokenT ) : BOOLEAN;

BEGIN
  (* bail out if set is invalid *)
  IF set = NIL THEN
    RETURN FALSE
  END; (* IF *)
  
  (* determine segment and bit where token is stored *)
  segIndex := ORD(token) DIV Bitwidth;
  bit := ORD(token) MOD Bitwidth;
  
  RETURN ODD(set^.segment[segIndex] DIV pow2(bit)) (* bit is set *)
END isElem;


(* --------------------------------------------------------------------------
 * function count(set)
 * --------------------------------------------------------------------------
 * Returns the number of tokens in set.
 * ----------------------------------------------------------------------- *)

PROCEDURE count ( set : TokenSet ) : CARDINAL;

BEGIN
  IF set = NIL THEN
    RETURN 0
  END (* IF *)
  
  RETURN set^.count
END count;


(* --------------------------------------------------------------------------
 * procedure PrintTokenList(set)
 * --------------------------------------------------------------------------
 * Prints a comma separated list of tokens in set.
 * ----------------------------------------------------------------------- *)

PROCEDURE PrintTokenList ( set : TokenSet );

VAR
  counter : CARDINAL;
  
BEGIN
  (* bail out if set is invalid *)
  IF set = NIL THEN
    Console.WriteChars("(NIL)")
  END; (* IF *)
  
  (* all clear -- print token list *)
  counter := 0;
  FOR token := MIN(TokenT) TO MAX(TokenT) DO
    IF isElem(set, token) THEN
      Console.WriteString(Token.lexemeForToken(token));
      counter := counter + 1;
      IF counter < set^.count THEN
        Console.WriteChars(", ")
      END (* IF *)
    END (* IF *)
  END (* FOR *)
END PrintTokenList;


(* --------------------------------------------------------------------------
 * procedure PrintSegments(set)
 * --------------------------------------------------------------------------
 * Prints a comma separated list of the data segments of set in base-16.
 * ----------------------------------------------------------------------- *)

PROCEDURE PrintSegments ( set : TokenSet );

BEGIN
  (* bail out if set is invalid *)
  IF set = NIL THEN
    Console.WriteChars("(NIL)")
  END; (* IF *)
  
  (* all clear -- print segments *)
  Console.WriteCardX(set^.segment5); Console.WriteChars(", ");
  Console.WriteCardX(set^.segment4); Console.WriteChars(", ");
  Console.WriteCardX(set^.segment3); Console.WriteChars(", ");
  Console.WriteCardX(set^.segment2); Console.WriteChars(", ");
  Console.WriteCardX(set^.segment1); Console.WriteChars(", ");
  Console.WriteCardX(set^.segment0)
END PrintSegments;


(* --------------------------------------------------------------------------
 * procedure Release(set)
 * --------------------------------------------------------------------------
 * Releases set and passes back NIL.
 * ----------------------------------------------------------------------- *)

PROCEDURE Release ( VAR set : TokenSet );

BEGIN
  (* bail out if set is invalid *)
  IF set = NIL THEN
    RETURN
  END; (* IF *)
  
  (* deallocate set and pass NIL *)
  DEALLOCATE(set);
  set := NIL
END Release;


BEGIN (* TokenSet *)
  (* bail out if CARDINAL is not 16-bit wide *)
  IF TSIZE(CARDINAL) # Bitwidth THEN
    Console.WriteChars("Library TokenSet requires 16-bit CARDINALs.");
    Console.WriteLn;
    HALT
  END (* IF *)
END TokenSet.