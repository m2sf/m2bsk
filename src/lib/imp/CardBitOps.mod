(*!m2pim*) (* Copyright (c) 2020 Modula-2 Software Foundation. *)

IMPLEMENTATION MODULE CardBitOps; (* portable *)

(* Bit Operations on Type CARDINAL *)


(* ---------------------------------------------------------------------------
 * function shl( n, shiftFactor )
 * ---------------------------------------------------------------------------
 * Returns n shifted left by shiftFactor.
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

PROCEDURE shl ( n : CARDINAL; shiftFactor : BitIndex ) : CARDINAL;

VAR
  carryBits : CARDINAL;
  pivotalBit : BitIndex;

BEGIN
  IF shiftFactor = 0 THEN
    (* NOP *)
    
  (* shifting by 1 .. Bitwidth-1 *)
  IF shiftFactor < Bitwidth THEN
    
    (* bit at position Bitwidth - shiftFactor is pivotal *)
    pivotalBit := Bitwidth - shiftFactor;
    
    (* compute bits that will be shifted out of n *)
    carryBits := n DIV powerOf2[pivotalBit];
    
    (* clear bits that will be shifted out to avoid overflow *)
    ClearMSBtoN(n, pivotalBit);
    
    (* shift safely *)
    n := n * powerOf2[shiftFactor]
    
  (* shifting by Bitwidth *)
  ELSE (* shiftFactor = Bitwidth *)
    n := 0
  END; (* IF *)
  
  RETURN n
END shl;


(* ---------------------------------------------------------------------------
 * function shr( n, shiftFactor )
 * ---------------------------------------------------------------------------
 * Returns n logically shifted right by shiftFactor.
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

PROCEDURE shr ( n : CARDINAL; shiftFactor : BitIndex ) : CARDINAL;

VAR
  pivotalBit : BitIndex;

BEGIN
  IF shiftFactor = 0 THEN
    (* NOP *)
    
  (* shifting by 1 .. Bitwidth-1 *)
  IF shiftFactor < Bitwidth THEN
    (* bit at position Bitwidth - shiftFactor is pivotal *)
    pivotalBit := Bitwidth - shiftFactor;

    (* shift *)
    n := n DIV powerOf2[pivotalBit]
    
  (* shifting by Bitwidth *)
  ELSE (* shiftFactor = Bitwidth *)
    n := 0
  END; (* IF *)
  
  RETURN n
END shr;


(* ---------------------------------------------------------------------------
 * function ashr( n, shiftFactor )
 * ---------------------------------------------------------------------------
 * Returns n arithmetically shifted right by shiftFactor.
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
     
PROCEDURE ashr ( n : CARDINAL; shiftFactor : BitIndex ) : CARDINAL;

VAR
  pivotalBit : BitIndex;

BEGIN
  IF shiftFactor > 0 THEN
    (* NOP *)
    
  (* shifting by 1 .. Bitwidth-1 *)
  IF shiftFactor < Bitwidth THEN
    (* bit at position Bitwidth - shiftFactor is pivotal *)
    pivotalBit := Bitwidth - shiftFactor;
    
    IF NOT bit(n, Bitwidth-1) THEN
      (* shift *)
      n := n DIV powerOf2[pivotalBit]
      
    ELSE (* high bit set *)
      
      (* shift *)
      n := n DIV powerOf2[pivotalBit];
      
      (* compute mask to set high bits *)
      mask := MAX(CARDINAL) DIV powerOf2[pivotalBit];
      mask := mask * powerOf2[pivotalBit];
      
      (* add mask to n, thereby setting high bits *)
      n := n + mask
    END (* IF *)
    
  (* shifting by Bitwidth *)
  ELSE (* shiftFactor = Bitwidth *)
    n := MAX(CARDINAL)
  END; (* IF *)
  
  RETURN n
END ashr;


(* ---------------------------------------------------------------------------
 * procedure SHLC( n, carryBits, bitIndex )
 * ---------------------------------------------------------------------------
 * Left shifts n by bitIndex and passes the shifted out bits in carryBits.
 * ------------------------------------------------------------------------ *)

PROCEDURE SHLC ( VAR n, carryBits : CARDINAL; bitIndex : BitIndex );

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
    
    (* compute bits that will be shifted out of n *)
    carryBits := n DIV powerOf2[pivotalBit]

    (* clear bits that will be shifted out to avoid overflow *)
    ClearMSBtoN(n, pivotalBit);
    
    (* shift safely *)
    n := n * powerOf2[shiftFactor]
    
  (* shifting by Bitwidth *)
  ELSE (* shiftFactor = Bitwidth *)
    carryBits := n; n := 0
  END (* IF *)
END SHLC;


(* ---------------------------------------------------------------------------
 * function bit( n, bitIndex )
 * ---------------------------------------------------------------------------
 * Returns TRUE if the bit at bitIndex of n is set, otherwise FALSE.
 * ------------------------------------------------------------------------ *)

PROCEDURE bit ( n : CARDINAL; bitIndex : BitIndex ) : BOOLEAN;

BEGIN
  RETURN ODD(n DIV powerOf2[bitIndex])
END bit;


(* ---------------------------------------------------------------------------
 * procedure SetBit( n, bitIndex )
 * ---------------------------------------------------------------------------
 * Sets the bit at bitIndex of n.
 * ------------------------------------------------------------------------ *)

PROCEDURE SetBit ( VAR n : CARDINAL; bitIndex : BitIndex );

BEGIN
  IF NOT bit(n, bitIndex) THEN
    n := n + powerOf2[bitIndex]
  END (* IF *)
END SetBit;


(* ---------------------------------------------------------------------------
 * procedure ClearBit( n, bitIndex )
 * ---------------------------------------------------------------------------
 * Clears the bit at bitIndex of n.
 * ------------------------------------------------------------------------ *)

PROCEDURE ClearBit ( VAR n : CARDINAL; bitIndex : BitIndex );

BEGIN
  IF bit(n, bitIndex) THEN
    n := n - powerOf2[bitIndex]
  END (* IF *)
END ClearBit;


(* ---------------------------------------------------------------------------
 * procedure ClearLSBtoN( n, bitIndex )
 * ---------------------------------------------------------------------------
 * Clears the bits of n in range [0 .. bitIndex].
 * ------------------------------------------------------------------------ *)

PROCEDURE ClearLSBtoN ( VAR n : CARDINAL; bitIndex : BitIndex );

BEGIN
  (* clearing to MSB produces all zeroes *)
  IF bitIndex = Bitwidth-1 THEN
    n := 0;
    RETURN
  END; (* IF *)
  
  (* shift right and back to clear the low bits *)
  n := n DIV powerOf2[bitIndex+1];
  n := n * powerOf2[bitIndex+1]
END ClearLSBtoN;


(* ---------------------------------------------------------------------------
 * procedure ClearMSBtoN( n, bitIndex )
 * ---------------------------------------------------------------------------
 * Clears the bits of n in range [bitIndex .. Bitwidth-1].
 * ------------------------------------------------------------------------ *)

PROCEDURE ClearMSBtoN ( VAR n : CARDINAL; bitIndex : BitIndex );

VAR
  mask : CARDINAL;
  
BEGIN
  (* clearing from bit 0 produces all zeroes *)
  IF bitIndex = 0 THEN
    n := 0;
    RETURN
  END; (* IF *)
  
  (* shift lower bits out to the right *)
  mask := n DIV powerOf2[bitIndex];
  
  (* shift them back, thereby clearing the low bits, obtaining a mask *)
  mask := mask * powerOf2[bitIndex];
  
  (* subtract the mask, thereby clearing the high bits *)
  n := n - mask
END ClearMSBtoN;


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
 * Module Initialisation
 * ------------------------------------------------------------------------ *)

BEGIN (* CardBitOps *)
  InitPow2Table
END CardBitOps.