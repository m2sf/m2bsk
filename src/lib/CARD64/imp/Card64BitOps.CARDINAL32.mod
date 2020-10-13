(*!m2pim*) (* Copyright (c) 2020 Modula-2 Software Foundation. *)

IMPLEMENTATION MODULE Card64BitOps; (* 32-bit CARDINAL version *)

(* Bit Operations on Type CARD64 *)


FROM SYSTEM IMPORT TSIZE;
FROM CARD64 IMPORT Card64T;


(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * NOTE : Not all classic Modula-2 compilers support records as return types
 * of functions.  For  portability,  all functions  that return a result  of
 * type Card64T  are therefore  implemented  as procedures  where the result
 * is passed back in a VAR parameter instead.
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)


(* --------------------------------------------------------------------------
 * Half Bitwidth
 * ----------------------------------------------------------------------- *)

CONST HalfBitwidth = Bitwidth DIV 2;


(* ---------------------------------------------------------------------------
 * Procedure:  Shl( n, shiftFactor )
 * ---------------------------------------------------------------------------
 * Passes n shifted left by shiftFactor in n.
 * ------------------------------------------------------------------------ *)

PROCEDURE Shl ( VAR n : Card64T; shiftFactor : BitIndex );

VAR
  pivotalBit, carryBits : CARDINAL;
  
BEGIN
  
  (* shifting by 0 *) 
  IF shiftFactor = 0 THEN
    (* leaves n unchanged *)
    RETURN
    
  (* shifting by Bitwidth/2 *)
  ELSIF shiftFactor = HalfBitwidth THEN
    (* moves all lowBits into highBits and clears lowBits *)
    n.highBits := n.lowBits;
    n.lowBits := 0;
    RETURN
      
  (* shifting by 1 to Bitwidth/2 *)
  ELSIF (shiftFactor >= 1) AND (shiftFactor < HalfBitwidth) THEN
    (* partially moves lowBits into highBits *)
    
    (* bit at position HalfBitwidth - shiftFactor is pivotal *)
    pivotalBit := HalfBitwidth - shiftFactor;
    
    (* remember bits that will be shifted out of lowBits *)
    carryBits := n.lowBits DIV powerOf2[pivotalBit];
    
    (* shift lowBits *)
    
    (* clear bits including and above pivotal bit to avoid overflow *)
    IF n.lowBits >= powerOf2[pivotalBit] THEN
      ClearMSBtoN(n.lowBits, pivotalBit)
    END; (* IF *)
    
    (* safely shift lowBits *)
    n.lowBits := n.lowBits * powerOf2[shiftFactor];
    
    (* shift highBits *)
    
    (* clear bits including and above pivotal bit to avoid overflow *)
    IF n.highBits >= powerOf2[pivotalBit] THEN
      ClearMSBtoN(n.highBits, pivotalBit)
    END; (* IF *)
    
    (* safely shift highBits *)
    n.highBits := n.highBits * powerOf2[shiftFactor];
    
    (* add the carry bits shifted out of lowBits *)
    n.highBits := n.highBits + carryBits;
    RETURN
    
  (* shifting by Bitwidth/2 to Bitwidth - 1 *)
  ELSIF (shiftFactor > HalfBitwidth) AND (shiftFactor < Bitwidth) THEN
    (* moves lowBits into highBits, clears lowBits, shifts highBits *)
    
    n.highBits := n.lowBits;
    n.lowBits := 0;
    
    (* bit at position HalfBitwidth - shiftFactor is pivotal *)
    pivotalBit := HalfBitwidth - shiftFactor;
    
    (* shift highBits *)
    
    (* clear bits including and above pivotal bit to avoid overflow *)
    IF n.highBits >= powerOf2[pivotalBit] THEN
      ClearMSBtoN(n.highBits, pivotalBit)
    END; (* IF *)
    
    (* safely shift highBits *)
    n.highBits := n.highBits * powerOf2[shiftFactor MOD HalfBitwidth];
    RETURN
    
  (* shifting by Bitwidth or more *)
  ELSE
    (* produces all zeroes *)
    n.lowBits := 0;
    n.highBits := 0;
    RETURN
  END (* IF *)
END Shl;


(* ---------------------------------------------------------------------------
 * Procedure:  Shr( n, shiftFactor )
 * ---------------------------------------------------------------------------
 * Passes n logically shifted right by shiftFactor in n.
 * ------------------------------------------------------------------------ *)

PROCEDURE Shr ( VAR n : Card64T; shiftFactor : BitIndex );

VAR
  pivotalBit, carryBits : CARDINAL;
  
BEGIN
  (* shifting by 0 *) 
  IF shiftFactor = 0 THEN
    (* leaves n unchanged *)
    RETURN
    
  (* shifting by Bitwidth/2 *)
  ELSIF shiftFactor = HalfBitwidth THEN
    (* moves all highBits into lowBits and clears highBits *)
    n.lowBits := n.highBits;
    n.highBits := 0;
    RETURN
    
  (* shifting by 1 to Bitwidth/2 *)
  ELSIF (shiftFactor >= 1) AND (shiftFactor < HalfBitwidth) THEN
    (* partially moves highBits into lowBits *)
    
    (* bit at position HalfBitwidth - shiftFactor is pivotal *)
    pivotalBit := HalfBitwidth - shiftFactor;
    
    (* remember bits that will be shifted out of highBits *)
    carryBits := n.highBits * powerOf2[pivotalBit];
    
    (* shift lowBits *)
    n.lowBits := n.lowBits DIV powerOf2[shiftFactor];
    
    (* shift highBits *)
    n.highBits := n.highBits DIV powerOf2[shiftFactor];
    
    (* add the bits shifted out of highBits to lowBits *)
    n.lowBits := n.lowBits + carryBits;
    RETURN
    
  (* shifting by Bitwidth/2 to Bitwidth - 1 *)
  ELSIF (shiftFactor > HalfBitwidth) AND (shiftFactor < Bitwidth) THEN
    (* moves highBits into lowBits, clears highBits, shifts lowBits *)
    
    n.lowBits := n.highBits;
    n.highBits := 0;
    
    (* bit at position HalfBitwidth - shiftFactor is pivotal *)
    pivotalBit := HalfBitwidth - shiftFactor;
    
    (* shift lowBits *)
    n.lowBits := n.lowBits DIV powerOf2[pivotalBit];
    RETURN
    
  (* shifting by Bitwidth or more *)
  ELSE
    (* produces all zeroes *)
    n.lowBits := 0;
    n.highBits := 0;
    RETURN  
  END (* IF *)
END Shr;


(* ---------------------------------------------------------------------------
 * Procedure:  AShr( n, shiftFactor )
 * ---------------------------------------------------------------------------
 * Passes n arithmetically shifted right by shiftFactor in n.
 * ------------------------------------------------------------------------ *)

PROCEDURE AShr ( VAR n : Card64T; shiftFactor : BitIndex );

BEGIN
  (* TO DO *)
END AShr;


(* ---------------------------------------------------------------------------
 * Function:  bit( n, bitIndex )
 * ---------------------------------------------------------------------------
 * Returns TRUE if the bit at bitIndex of n is set, otherwise FALSE.
 * ------------------------------------------------------------------------ *)

PROCEDURE bit ( n : Card64T; bitIndex : BitIndex ) : BOOLEAN;

BEGIN
  (* bitIndex falls into lowBits *)
  IF bitIndex < HalfBitwidth THEN
    RETURN ODD(n.lowBits DIV powerOf2[bitIndex])
    
  (* bitIndex falls into highBits *)
  ELSE (* bitIndex >= HalfBitwidth *)
    RETURN ODD(n.highBits DIV powerOf2[bitIndex-HalfBitwidth])
  END (* IF *)
END bit;


(* ---------------------------------------------------------------------------
 * Procedure:  SetBit( n, bitIndex )
 * ---------------------------------------------------------------------------
 * Sets the bit at bitIndex of n.
 * ------------------------------------------------------------------------ *)

PROCEDURE SetBit ( VAR n : Card64T; bitIndex : BitIndex );

BEGIN
  (* bitIndex falls into lowBits *)
  IF bitIndex < HalfBitwidth THEN
    IF NOT ODD(n.lowBits DIV powerOf2[bitIndex]) THEN
      n.lowBits := n.lowBits + powerOf2[bitIndex]
    END (* IF *)
    
  (* bitIndex falls into highBits *)
  ELSE (* bitIndex >= HalfBitwidth *)
    IF NOT ODD(n.highBits DIV powerOf2[bitIndex-HalfBitwidth]) THEN
      n.highBits := n.highBits + powerOf2[bitIndex-HalfBitwidth]
    END (* IF *)
  END (* IF *)
END SetBit;


(* ---------------------------------------------------------------------------
 * Procedure:  ClearBit( n, bitIndex )
 * ---------------------------------------------------------------------------
 * Clears the bit at bitIndex of n.
 * ------------------------------------------------------------------------ *)

PROCEDURE ClearBit ( VAR n : Card64T; bitIndex : BitIndex );

BEGIN
  (* bitIndex falls into lowBits *)
  IF bitIndex < HalfBitwidth THEN
    IF ODD(n.lowBits DIV powerOf2[bitIndex]) THEN
      n.lowBits := n.lowBits - powerOf2[bitIndex]
    END (* IF *)
    
  (* bitIndex falls into highBits *)
  ELSE (* bitIndex >= HalfBitwidth *)
    IF ODD(n.highBits DIV powerOf2[bitIndex-HalfBitwidth]) THEN
      n.highBits := n.highBits - powerOf2[bitIndex-HalfBitwidth]
    END (* IF *)
  END (* IF *)
END SetBit;


(* ---------------------------------------------------------------------------
 * Procedure:  ClearLSBtoN( n, bitIndex )
 * ---------------------------------------------------------------------------
 * Clears the bits of n in range [0 .. bitIndex].
 * ------------------------------------------------------------------------ *)

PROCEDURE ClearLSBtoN ( VAR n : Card64T; bitIndex : BitIndex );

VAR
  mask : CARDINAL;
  
BEGIN
  IF bitIndex >= HalfBitwidth THEN
        
    (* clear bits 0 .. bitIndex-HalfBitwidth of n.highBits *)
    
    (* clear n.lowBits *)
    n.lowBits := 0
    
  ELSIF (* bitIndex < HalfBitwidth AND *) bitIndex > 0 THEN
    
    (* n.highBits remain unaffected *)
    
    (* clear bits 0 .. bitIndex of n.lowBits *)
    
  ELSE (* bitIndex = 0 *)
  
    (* n.highBits remain unaffected *)
  
    (* clear bit 0 *)
    IF ODD(n.lowBits) THEN
      n.lowBits := n.lowBits - 1
    END (* IF *)
  END (* IF *)
END ClearLSBtoN;


(* ---------------------------------------------------------------------------
 * Procedure:  ClearMSBtoN( n, bitIndex )
 * ---------------------------------------------------------------------------
 * Clears the bits of n in range [bitIndex .. Bitwidth-1].
 * ------------------------------------------------------------------------ *)

PROCEDURE ClearMSBtoN ( VAR n : Card64T; bitIndex : BitIndex );

VAR
  mask : CARDINAL;
  
BEGIN
  IF bitIndex > HalfBitwidth THEN
    
    (* shift bits [0..bitIndex-1] of n.highBits out to the right *)
    mask := n.highBits DIV powerOf2[bitIndex-HalfBitwidth];
    
    (* shift them back, thereby clearing them, obtaining a mask *)
    mask := mask * powerOf2[bitIndex-HalfBitwidth];
    
    (* subtract the mask, thereby clearing the higher bits *)
    n.highBits := n.highBits - mask
    
    (* n.lowBits remain unaffected *)
    
  ELSIF bitIndex = HalfBitwidth THEN
    n.highBits := 0
    
    (* n.lowBits remain unaffected *)
    
  ELSIF (* bitIndex < HalfBitwidth AND *) bitIndex > 0 THEN
    (* all bits in n.highBits are cleared *)
    n.highBits := 0;
    
    (* shift bits [0..bitIndex-1] of n.lowBits out to the right *)
    mask := n.lowBits DIV powerOf2[bitIndex];
    
    (* shift them back, thereby clearing them, obtaining a mask *)
    mask := mask * powerOf2[bitIndex];
    
    (* subtract the mask, thereby clearing the higher bits *)
    n.lowBits := n.lowBits - mask
        
  ELSE (* bitIndex = 0 *)
    n.highBits := 0;
    n.lowBits := 0
  END (* IF *)
END ClearMSBtoN;


(* ************************************************************************ *
 * Private Operations                                                       *
 * ************************************************************************ *)

(* ---------------------------------------------------------------------------
 * Powers of 2 table
 * ------------------------------------------------------------------------ *)

VAR
  powerOf2 : ARRAY [0..MAX(BitIndex)] OF CARDINAL;
  

(* ---------------------------------------------------------------------------
 * private procedure InitPow2Table
 * ---------------------------------------------------------------------------
 * Initialises the powers of 2 table
 * ------------------------------------------------------------------------ *)

PROCEDURE InitPow2Table;

VAR
  index : BitIndex;
  
BEGIN
  powerOf2[0] := 1;
  FOR index := 1 TO Bitwidth-2 DO
    powerOf2[index] := powerOf2[index-1] * 2
  END; (* FOR *)
  powerOf2[Bitwidth-1] := MAX(CARDINAL)
END InitPow2Table;


(* ---------------------------------------------------------------------------
 * Module initialisation.
 * ------------------------------------------------------------------------ *)

BEGIN (* Card64BitOps *)
  InitPow2Table
END Card64BitOps.