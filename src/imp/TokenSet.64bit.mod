(*!m2pim*) (* Copyright (c) 2015 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE TokenSet; (* requires 64-bit CARDINAL *)

(* Token Set ADT for Modula-2 R10 Bootstrap Kernel *)

IMPORT Console;

FROM Token IMPORT TokenT; (* alias for Token.Token *)

FROM SYSTEM IMPORT TSIZE;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;


CONST
  Bitwidth = 64;
  SegCount = Bitwidth ORD(MAX(TokenT)) DIV Bitwidth;


(* --------------------------------------------------------------------------
 * TokenSet type
 * ----------------------------------------------------------------------- *)

TYPE TokenSet = POINTER TO Descriptor;

TYPE Descriptor = RECORD
  count   : CARDINAL;
  segment : ARRAY [0..SegCount-1] OF CARDINAL
END; (* Descriptor *)


(* --------------------------------------------------------------------------
 * Data table for powers of 2
 * ----------------------------------------------------------------------- *)

VAR
  pow2 : ARRAY [0..Bitwidth-1] OF CARDINAL;


(* Operations *)

(* --------------------------------------------------------------------------
 * procedure NewFromRawData(set, segment2, segment1, segment0)
 * --------------------------------------------------------------------------
 * Passes a newly allocated and initialised TokenSet instance back in set.
 * The set is initalised from parameters segment1 and segment0 as follows:
 *  
 *   bit 127        bit 0
 *    v                v
 *   [<-------set------>]
 *   [segment1][segment0]
 *    ^         ^
 *   bit 63    bit 63
 *  
 * The bits in set correspond to the token values of type Token.
 * ----------------------------------------------------------------------- *)

PROCEDURE NewFromRawData
  ( VAR set : TokenSet; segment1, segment0 : CARDINAL );

VAR
  token : TokenT;
  newSet : TokenSetT;
  mask, highBitInSeg1, bit, segIndex : CARDINAL;
  
BEGIN
  (* allocate new set *)
  ALLOCATE(newSet, TSIZE(Descriptor));
  
  (* initialise segment0 as passed in *)
  newSet^segment[0] := segment0;
  
  (* initialise segment1 by clearing unused higher bits *)
  
  (* determine highest token bit in segment1 *)
  highBitInSeg1 := ORD(MAX(TokenT)) MOD Bitwidth;
  
  (* shift lower bits out to the right *)
  mask := segment1 DIV (highBitInSeg1 + 1);
  
  (* shift them back, thereby clearing the low bits to obtain a mask *)
  mask := mask * pow2[highBitInSeg1 + 1];
  
  (* subtract the mask, thereby clearing the bits above the highest bit *)  
  newSet^segment[1] := segment1 - mask;
  
  (* count total number of bits to initialise counter *)
  newSet^.count := 0;
  FOR segIndex := 0 TO SegCount-1 DO
    FOR bit := 0 TO Bitwidth-1 DO
      IF ODD(segment[segIndex] DIV pow2[bit]) THEN
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
  segIndex, bit : CARDINAL;
  
BEGIN
  (* bail out if set is invalid *)
  IF set = NIL THEN
    RETURN
  END; (* IF *)
  
  (* determine segment and bit where token is stored *)
  segIndex := ORD(token) DIV Bitwidth;
  bit := ORD(token) MOD Bitwidth;
  
  (* test bit in segment *)
  IF ODD(set^.segment[segIndex] DIV pow2[bit]) (* bit is set *) THEN
    RETURN
  ELSE (* bit is not set *)
    (* set the bit *)
    set^.segment[segIndex] := set^.segment[segIndex] + pow2[bit];
    
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
  segIndex, bit : CARDINAL;
  
BEGIN
  (* bail out if set is invalid *)
  IF set = NIL THEN
    RETURN
  END; (* IF *)
  
  (* determine segment and bit where token is stored *)
  segIndex := ORD(token) DIV Bitwidth;
  bit := ORD(token) MOD Bitwidth;
  
  (* test bit in segment *)
  IF ODD(set^.segment[segIndex] DIV pow2[bit]) (* bit is set *) THEN
    (* clear the bit *)
    set^.segment[segIndex] := set^.segment[segIndex] - pow2[bit];
    
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
  
  RETURN ODD(set^.segment[segIndex] DIV pow2[bit]) (* bit is set *)
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


(* ************************************************************************ *
 * Private Operations                                                       *
 * ************************************************************************ *)

(* --------------------------------------------------------------------------
 * procedure: InitPow2Table
 * --------------------------------------------------------------------------
 * Initialises data table with powers of 2.
 * ----------------------------------------------------------------------- *)

PROCEDURE InitPow2Table;

BEGIN
  pow2[0] := 1;
  pow2[1] := 2;
  pow2[2] := 4;
  pow2[3] := 8;
  pow2[4] := 16;
  pow2[5] := 32;
  pow2[6] := 64;
  pow2[7] := 128;
  pow2[8] := 256;
  pow2[9] := 512;
  pow2[10] := 1024;
  pow2[11] := 2048;
  pow2[12] := 4096;
  pow2[13] := 8192;
  pow2[14] := 16384;
  pow2[15] := 32768;
  pow2[16] := 65536;
  pow2[17] := 131072;
  pow2[18] := 262144;
  pow2[19] := 524288;
  pow2[20] := 1048576;
  pow2[21] := 2097152;
  pow2[22] := 4194304;
  pow2[23] := 8388608;
  pow2[24] := 16777216;
  pow2[25] := 33554432;
  pow2[26] := 67108864;
  pow2[27] := 134217728;
  pow2[28] := 268435456;
  pow2[29] := 536870912;
  pow2[30] := 1073741824;
  pow2[31] := 2147483648
  pow2[32] := 4294967296;
  pow2[33] := 8589934592;
  pow2[34] := 17179869184;
  pow2[35] := 34359738368;
  pow2[36] := 68719476736;
  pow2[37] := 137438953472;
  pow2[38] := 274877906944;
  pow2[39] := 549755813888;
  pow2[40] := 1099511627776;
  pow2[41] := 2199023255552;
  pow2[42] := 4398046511104;
  pow2[43] := 8796093022208;
  pow2[44] := 17592186044416;
  pow2[45] := 35184372088832;
  pow2[46] := 70368744177664;
  pow2[47] := 140737488355328;
  pow2[48] := 281474976710656;
  pow2[49] := 562949953421312;
  pow2[50] := 1125899906842624;
  pow2[51] := 2251799813685248;
  pow2[52] := 4503599627370496;
  pow2[53] := 9007199254740992;
  pow2[54] := 18014398509481984;
  pow2[55] := 36028797018963968;
  pow2[56] := 72057594037927936;
  pow2[57] := 144115188075855872;
  pow2[58] := 288230376151711744;
  pow2[59] := 576460752303423488;
  pow2[60] := 1152921504606846976;
  pow2[61] := 2305843009213693952;
  pow2[62] := 4611686018427387904;
  pow2[63] := 9223372036854775808
END InitPow2Table;


BEGIN (* TokenSet *)
  (* bail out if CARDINAL is not 64-bit wide *)
  IF TSIZE(CARDINAL) # Bitwidth THEN
    Console.WriteChars("Library TokenSet requires 64-bit CARDINALs.");
    Console.WriteLn;
    HALT
  END; (* IF *)
  
  (* Initialise data table *)
  InitPow2Table
END TokenSet.