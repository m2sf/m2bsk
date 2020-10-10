(*!m2pim*) (* Copyright (c) 2020 Modula-2 Software Foundation. *)

IMPLEMENTATION MODULE LongIntBitOps; (* Bit Operations on Type LONGINT *)

(* Portable across all PIM and ISO dialects *)


(* ---------------------------------------------------------------------------
 * function shl( i, shiftFactor )
 * ---------------------------------------------------------------------------
 * Returns i shifted left by shiftFactor.
 *
 *             M S B                                           L S B
 *            +-----+-----+-----+           +-----+-----+-----+-----+
 *  bit index | n-1 | n-2 | n-3 |   . . .   |  3  |  2  |  1  |  0  |  before
 *            +-----+-----+-----+           +-----+-----+-----+-----+
 *                   /     /                       /     /     /
 *                  /     /                       /     /     /
 *                 /     /                       /     /     /     0
 *                /     /                       /     /     /     /
 *            +-----+-----+-----+           +-----+-----+-----+-----+
 *  bit index | n-1 | n-2 | n-3 |   . . .   |  3  |  2  |  1  |  0  |  after
 *            +-----+-----+-----+           +-----+-----+-----+-----+
 * ------------------------------------------------------------------------ *)

PROCEDURE shl ( i : LONGINT; shiftFactor : BitIndex ) : LONGINT;

VAR
  carryBits : LONGINT;
  pivotalBit : BitIndex;

BEGIN
  IF shiftFactor = 0 THEN
    (* NOP *)
    
  (* shifting by 1 .. Bitwidth-1 *)
  IF shiftFactor < Bitwidth THEN
    
    (* clear high bit *)
    IF i < 0 THEN
      i := i - MIN(LONGINT)
    END; (* IF *)
    
    (* bit at position Bitwidth - shiftFactor is pivotal *)
    pivotalBit := Bitwidth - shiftFactor;
    
    (* compute bits that will be shifted out of i *)
    carryBits := i DIV powerOf2[pivotalBit];
    
    (* clear bits that will be shifted out to avoid overflow *)
    ClearBitsInclAndAbove(i, pivotalBit);
    
    (* shift safely *)
    i := i * powerOf2[shiftFactor];
    
    (* set high bit if pivotal bit is set *)
    IF ODD(carryBits) THEN
      i := i + MIN(LONGINT)    
    END; (* IF *)
    
  (* shifting by Bitwidth *)
  ELSE (* shiftFactor = Bitwidth *)
    i := 0
  END; (* IF *)
  
  RETURN i
END shl;


(* ---------------------------------------------------------------------------
 * function shr( i, shiftFactor )
 * ---------------------------------------------------------------------------
 * Returns i logically shifted right by shiftFactor.
 *
 *             M S B                                           L S B
 *            +-----+-----+-----+           +-----+-----+-----+-----+
 *  bit index | n-1 | n-2 | n-3 |   . . .   |  3  |  2  |  1  |  0  |  before
 *            +-----+-----+-----+           +-----+-----+-----+-----+
 *                 \     \                       \     \     \
 *                  \     \                       \     \     \
 *             0     \     \                       \     \     \
 *              \     \     \                       \     \     \
 *            +-----+-----+-----+           +-----+-----+-----+-----+
 *  bit index | n-1 | n-2 | n-3 |   . . .   |  3  |  2  |  1  |  0  |  after
 *            +-----+-----+-----+           +-----+-----+-----+-----+
 * ------------------------------------------------------------------------ *)

PROCEDURE shr ( i : LONGINT; shiftFactor : BitIndex ) : LONGINT;

VAR
  pivotalBit : BitIndex;

BEGIN
  IF shiftFactor = 0 THEN
    (* NOP *)
    
  (* shifting by 1 .. Bitwidth-1 *)
  IF shiftFactor < Bitwidth THEN
    (* bit at position Bitwidth - shiftFactor is pivotal *)
    pivotalBit := Bitwidth - shiftFactor;

    IF i >= 0 THEN
      (* shift *)
      i := i DIV powerOf2[pivotalBit]
      
    ELSE (* i < 0 *)
      (* clear high bit *)
      i := i - MIN(LONGINT);
            
      (* shift by 1 and copy high bit to Bitwidth-2 *)
      i := i DIV 2 + powerOf2[Bitwidth-2];
      
      (* shift by index of pivotal bit less the one already shifted *)
      i := i DIV powerOf2[pivotalBit-1]
    END (* IF *)
    
  (* shifting by Bitwidth *)
  ELSE (* shiftFactor = Bitwidth *)
    i := 0
  END; (* IF *)
  
  RETURN i
END shr;


(* ---------------------------------------------------------------------------
 * function ashr( i, shiftFactor )
 * ---------------------------------------------------------------------------
 * Returns i arithmetically shifted right by shiftFactor.
 *
 *             M S B                                           L S B
 *            +-----+-----+-----+           +-----+-----+-----+-----+
 *  bit index | n-1 | n-2 | n-3 |   . . .   |  3  |  2  |  1  |  0  |  before
 *            +-----+-----+-----+           +-----+-----+-----+-----+
 *               | \     \                       \     \     \
 *               |  \     \                       \     \     \
 *               |   \     \                       \     \     \
 *               |    \     \                       \     \     \
 *            +-----+-----+-----+           +-----+-----+-----+-----+
 *  bit index | n-1 | n-2 | n-3 |   . . .   |  3  |  2  |  1  |  0  |  after
 *            +-----+-----+-----+           +-----+-----+-----+-----+
 * ------------------------------------------------------------------------ *)
     
PROCEDURE ashr ( i : LONGINT; shiftFactor : BitIndex ) : LONGINT;

VAR
  pivotalBit : BitIndex;

BEGIN
  IF shiftFactor = 0 THEN
    (* NOP *)
    
  (* shifting by 1 .. Bitwidth-1 *)
  IF shiftFactor < Bitwidth THEN
    (* bit at position Bitwidth - shiftFactor is pivotal *)
    pivotalBit := Bitwidth - shiftFactor;

    IF i >= 0 THEN
      (* shift *)
      i := i DIV powerOf2[pivotalBit]
      
    ELSE (* i < 0 *)
      (* clear high bit *)
      i := i - MIN(LONGINT);
            
      (* shift by 1 and copy high bit to Bitwidth-2 *)
      i := i DIV 2 + powerOf2[Bitwidth-2];
      
      (* shift by index of pivotal bit less the one already shifted *)
      i := i DIV powerOf2[pivotalBit-1];
      
      (* restore high bit *)
      i := i + MIN(LONGINT)
    END (* IF *)
    
  (* shifting by Bitwidth *)
  ELSE (* shiftFactor = Bitwidth *)
    i := 0
  END; (* IF *)
  
  RETURN i
END ashr;


(* ---------------------------------------------------------------------------
 * procedure ShlWCarry( i, carryBits, bitIndex )
 * ---------------------------------------------------------------------------
 * Left shifts i by bitIndex and passes the shifted out bits in carryBits.
 * ------------------------------------------------------------------------ *)

PROCEDURE ShlWCarry ( VAR i, carryBits : LONGINT; bitIndex : BitIndex );

VAR
  pivotalBit : BitIndex;

BEGIN
  (* shifting by 0 *)
  IF shiftFactor = 0 THEN
    carryBits := 0
    
  (* shifting by 1 .. Bitwidth-1 *)
  ELSIF shiftFactor < Bitwidth THEN
    (* bit at position Bitwidth - shiftFactor is pivotal *)
    pivotalBit := Bitwidth - shiftFactor;
    
   (* compute bits that will be shifted out of i *)
    IF i >= 0 THEN
      carryBits := i DIV powerOf2[pivotalBit]

    ELSE (* i < 0 *)
      (* clear high bit *)
      i := i - MIN(LONGINT);
      
      (* shift by 1 to restore high bit into overflow, ... *)
      carryBits := i DIV 2 + powerOf2[Bitwidth-2];
      
      (* then shift by index of pivotal bit less the one already shifted *)
      carryBits := carryBits DIV powerOf2[pivotalBit-1]
    END; (* IF *)

    (* clear bits that will be shifted out to avoid overflow *)
    ClearBitsInclAndAbove(i, pivotalBit);
    
    (* shift safely *)
    i := i * powerOf2[shiftFactor];
    
    (* set high bit if pivotal bit is set *)
    IF ODD(carryBits) THEN
      i := i + MIN(LONGINT)    
    END (* IF *)
    
  (* shifting by Bitwidth *)
  ELSE (* shiftFactor = Bitwidth *)
    carryBits := i; i := 0
  END (* IF *)
END ShlWCarry;


(* ---------------------------------------------------------------------------
 * function bit( i, bitIndex )
 * ---------------------------------------------------------------------------
 * Returns TRUE if the bit at bitIndex of i is set, otherwise FALSE.
 * ------------------------------------------------------------------------ *)

PROCEDURE bit ( i : LONGINT; bitIndex : BitIndex ) : BOOLEAN;

BEGIN
  IF (i < 0) AND (bitIndex = Bitwidth-1) THEN
    RETURN TRUE
  ELSE (* general case *)
    RETURN ODD(i DIV powerOf2[bitIndex])
  END (* IF *)
END bit;


(* ---------------------------------------------------------------------------
 * procedure SetBit( i, bitIndex )
 * ---------------------------------------------------------------------------
 * Sets the bit at bitIndex of i.
 * ------------------------------------------------------------------------ *)

PROCEDURE SetBit ( VAR i : LONGINT; bitIndex : BitIndex );

BEGIN
  IF NOT bit(i, bitIndex) THEN
    i := i + powerOf2[bitIndex]
  END (* IF *)
END SetBit;


(* ---------------------------------------------------------------------------
 * procedure ClearBit( i, bitIndex )
 * ---------------------------------------------------------------------------
 * Clears the bit at bitIndex of i.
 * ------------------------------------------------------------------------ *)

PROCEDURE ClearBit ( VAR i : LONGINT; bitIndex : BitIndex );

BEGIN
  IF bit(i, bitIndex) THEN
    i := i - powerOf2[bitIndex]
  END (* IF *)
END ClearBit;


(* ---------------------------------------------------------------------------
 * Powers of 2 table
 * ------------------------------------------------------------------------ *)

VAR
  powerOf2 : ARRAY [0..MAX(BitIndex)] OF LONGINT;
  

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
  powerOf2[Bitwidth-1] := MIN(LONGINT)
END InitPow2Table;


(* ---------------------------------------------------------------------------
 * Module Initialisation
 * ------------------------------------------------------------------------ *)

BEGIN (* LongIntBitOps *)
  InitPow2Table
END LongIntBitOps.