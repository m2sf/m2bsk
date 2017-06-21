(*!m2pim*) (* Copyright (c) 2015 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE Char;

(* Character tests and conversions *)

IMPORT ASCII;


(* Tests *)

PROCEDURE isControl ( ch : CHAR ) : BOOLEAN;
(* Returns TRUE if ch is a control code, otherwise FALSE. *)

BEGIN
  RETURN ((ch >= ASCII.NUL) AND (ch <= ASCII.US)) OR (ch = ASCII.DEL)
END isControl;


PROCEDURE isDigit ( ch : CHAR ) : BOOLEAN;
(* Returns TRUE if ch is a digit, otherwise FALSE. *)

BEGIN
  RETURN (ch >= '0') AND (ch <= '9')
END isDigit;


PROCEDURE isAtoF ( ch : CHAR ) : BOOLEAN;
(* Returns TRUE if ch is a base-16 digit, otherwise FALSE. *)

BEGIN
  RETURN (ch >= 'A') AND (ch <= 'F')
END isAtoF;


PROCEDURE isLetter ( ch : CHAR ) : BOOLEAN;
(* Returns TRUE if ch is a letter, otherwise FALSE. *)

BEGIN
  RETURN ((ch >= 'A') AND (ch <= 'Z')) OR ((ch >= 'a') AND (ch <= 'z'))
END isLetter;


PROCEDURE isUpper ( ch : CHAR ) : BOOLEAN;
(* Returns TRUE if ch is an uppercase letter, otherwise FALSE. *)

BEGIN
  RETURN (ch >= 'A') AND (ch <= 'Z')
END isUpper;


PROCEDURE isLower ( ch : CHAR ) : BOOLEAN;
(* Returns TRUE if ch is a lowercase letter, otherwise FALSE. *)

BEGIN
  RETURN (ch >= 'a') AND (ch <= 'z')
END isLower;


PROCEDURE isAlphaNum ( ch : CHAR ) : BOOLEAN;
(* Returns TRUE if ch is alpha-numeric, otherwise FALSE. *)

BEGIN
  RETURN
    ((ch >= '0') AND (ch <= '9')) OR
    ((ch >= 'A') AND (ch <= 'Z')) OR
    ((ch >= 'a') AND (ch <= 'z'))
END isAlphaNum;


PROCEDURE isPrintable ( ch : CHAR ) : BOOLEAN;
(* Returns TRUE if ch is printable, otherwise FALSE. *)

BEGIN
  RETURN (ch >= ASCII.SP) AND (ch <= '~')
END isPrintable;


PROCEDURE isQuotable ( ch : CHAR ) : BOOLEAN;
(* Returns TRUE if ch is quotable, otherwise FALSE. *)

BEGIN
  RETURN
    (ch = ASCII.SP) OR
    ((ch >= '(') AND (ch <= '[')) OR
    ((ch >= ']') AND (ch <= '~')) OR
    ((ch >= 'a') AND (ch <= 'z')) OR
    ((ch >= '#') AND (ch <= '&')) OR (ch = '!')
END isQuotable;


PROCEDURE isEscapable ( ch : CHAR ) : BOOLEAN;
(* Returns TRUE if ch is escapable, otherwise FALSE. *)

BEGIN
  RETURN (ch = ASCII.BACKSLASH) OR (ch = 'n') OR (ch = 't')
END isEscapable;


(* Conversions *)

PROCEDURE toUpper ( ch : CHAR ) : CHAR;
(* Returns the uppercase equivalent of ch if ch is a lowercase letter.
   Otherwise returns ch. *)

BEGIN
  IF (ch >= 'a') AND (ch <= 'z') THEN
    RETURN CHR(ORD(ch) - 32)
  ELSE (* not lowercase *)
    RETURN ch
  END (* IF *)
END toUpper;


PROCEDURE ToUpper ( VAR ch : CHAR );
(* Replaces ch with its uppercase equivalent if ch is a lowercase letter. *)

BEGIN
  IF (ch >= 'a') AND (ch <= 'z') THEN ch := CHR(ORD(ch) - 32) END
END ToUpper;


PROCEDURE toLower ( ch : CHAR ) : CHAR;
(* Returns the lowercase equivalent of ch if ch is an uppercase letter.
   Otherwise returns ch. *)

BEGIN
  IF (ch >= 'A') AND (ch <= 'Z') THEN
    RETURN CHR(ORD(ch) + 32)
  ELSE (* not uppercase *)
    RETURN ch
  END (* IF *)
END toLower;


PROCEDURE ToLower ( VAR ch : CHAR );
(* Replaces ch with its lowercase equivalent if ch is a uppercase letter. *)

BEGIN
  IF (ch >= 'A') AND (ch <= 'Z') THEN ch := CHR(ORD(ch) + 32) END
END ToLower;


END Char.