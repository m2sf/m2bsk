(*!m2pim*) (* Copyright (c) 2024 Modula-2 Software Foundation. *)

IMPLEMENTATION MODULE Unichar; (* portable *)



(* ---------------------------------------------------------------------------
 * function UCHR( value )
 * ---------------------------------------------------------------------------
 * Returns the UNICHAR code point for value.
 * ------------------------------------------------------------------------ *)

PROCEDURE UCHR ( val : UnicharBaseT ) : UNICHAR;

BEGIN
  IF (value > MaxCodePoint) OR (value < 0) THEN
    HALT
  END; (* IF *)
  
  RETURN VAL(UNICHAR, value)
END UCHR;


END Unichar.