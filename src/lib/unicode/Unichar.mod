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


(* ---------------------------------------------------------------------------
 * function fromUTF8( utf8 )
 * ---------------------------------------------------------------------------
 * Returns UNICHAR code point value from UTF8.
 * ------------------------------------------------------------------------ *)

PROCEDURE fromUTF8 ( VAR (* CONST *) utf8 : UTF8 ) : UNICHAR;

VAR cp : UNICHAR;

BEGIN
  CASE utf8.length OF
    1 :
      cp := VAL(UNICHAR, utf8[0])
      
  | 2 :
      cp :=
        256 * (VAL(UNICHAR, utf8[0]) MOD 32)
          + VAL(UNICHAR, utf8[1]) MOD 64
  | 3 :
      cp :=
        65536 * (VAL(UNICHAR, utf8[0]) MOD 16)
          + 256 * (VAL(UNICHAR, utf8[1]) MOD 64)
          + VAL(UNICHAR, utf8[2]) MOD 64
      
  | 4 :
      cp :=
        16777216 * (VAL(UNICHAR, utf8[0]) MOD 8)
          + 65536 * (VAL(UNICHAR, utf8[1]) MOD 64)
          + 256 * (VAL(UNICHAR, utf8[2]) MOD 64)
          + VAL(UNICHAR, utf8[3]) MOD 64
    
  ELSE (* invalid *)
    cp := 0;
    (* TO DO: error handling *)
  END; (* CASE *)
  
  RETURN cp
END fromUTF8;


(* ---------------------------------------------------------------------------
 * procedure ToUTF8( cp, utf8 )
 * ---------------------------------------------------------------------------
 * Converts UNICHAR code point value to UTF8.
 * ------------------------------------------------------------------------ *)

PROCEDURE ToUTF8 ( cp : UNICHAR; VAR utf8 : UTF8 );

VAR index : CARDINAL;

BEGIN
  CASE cp OF
  (* code point in range U+0000 .. U+007F *)
    0 .. 127 :
      utf8.length := 1;
      utf8.octet[0] := VAL(Octet, cp);
      
  (* code point in range U+0080 .. U+07FF *)
  | 128 .. 2047 :
      utf8.length := 2;
      
      (* 1st octet = (cp SHR 6) MOD 64 + prefix 0xC0 *)
      utf8.octet[0] := VAL(Octet, (cp DIV 64) MOD 64) + 192;
      
      (* 2nd octet = cp MOD 64 + prefix 0x80 *)
      utf8.octet[2] := VAL(Octet, cp MOD 64) + 128
      
  (* code point in range U+0800 .. U+FFFF *)
  | 2048 .. 65535 :
      utf8.length := 3;
      
      (* 1st octet = (cp SHR 12) MOD 16 + prefix 0xE0 *)
      utf8.octet[0] := VAL(Octet, (cp DIV 4096) MOD 16) + 224;
      
      (* 2nd octet = (cp SHR 6) MOD 64 + prefix 0x80 *)
      utf8.octet[1] := VAL(Octet, (cp DIV 4096) MOD 64) + 128;
      
      (* 3rd octet = cp MOD 64 + prefix 0x80 *)
      utf8.octet[2] := VAL(Octet, cp MOD 64) + 128
      
  (* code point in range U+010000 .. U+10FFFF *)
  | 65536 .. 1114111 :
      utf8.length := 4;
      
      (* 1st octet = (cp SHR 18) MOD 8 + prefix 0xF0 *)
      utf8.octet[0] := VAL(Octet, (cp DIV 262144) MOD 8) + 240;
      
      (* 2nd octet = (cp SHR 12) MOD 64 + prefix 0x80 *)
      utf8.octet[1] := VAL(Octet, (cp DIV 4096) MOD 64) + 128;
      
      (* 3rd octet = (cp SHR 6) MOD 64 + prefix 0x80 *)
      utf8.octet[2] := VAL(Octet, (cp DIV 64) MOD 64) + 128;
      
      (* 4th octet = cp MOD 64 + prefix 0x80 *)
      utf8.octet[3] := VAL(Octet, cp MOD 64) + 128
  
  (* code point out of valid range for RFC 3629  *)
  ELSE (* overlong *)
    utf8.length := 0
    (* TO DO: error handling *)
  END; (* CASE *)
  
  (* clear unused octets *)
  FOR index := utf8.length TO MaxUTF8Length-1 DO
    utf8.octet[index] := 0
  END (* FOR *)
END ToUTF8;


END Unichar.