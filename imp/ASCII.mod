(*!m2pim*) (* Copyright (c) 2015 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE ASCII;


(* Operations *)

PROCEDURE isControl ( ch : CHAR ) : BOOLEAN;
(* Returns TRUE if ch is a control code, otherwise FALSE. *)

BEGIN
  RETURN (ch <= US) OR (ch = DEL)
END isControl;


PROCEDURE isDigit ( ch : CHAR ) : BOOLEAN;
(* Returns TRUE if ch is a digit, otherwise FALSE. *)

BEGIN
  RETURN (ch >= DIGIT_ZERO) AND (ch <= DIGIT_NINE)
END isDigit;


PROCEDURE isLetter ( ch : CHAR ) : BOOLEAN;
(* Returns TRUE if ch is a letter, otherwise FALSE. *)

BEGIN
  RETURN ((ch >= SMALL_LETTER_A) AND (ch <= SMALL_LETTER_Z))
    OR ((ch >= CAPITAL_LETTER_A) AND (ch <= CAPITAL_LETTER_Z))
END isLetter;


PROCEDURE isAlphaNum ( ch : CHAR ) : BOOLEAN;
(* Returns TRUE if ch is alpha-numeric, otherwise FALSE. *)

BEGIN
  RETURN ((ch >= DIGIT_ZERO) AND (ch <= DIGIT_NINE))
    OR ((ch >= SMALL_LETTER_A) AND (ch <= SMALL_LETTER_Z))
    OR ((ch >= CAPITAL_LETTER_A) AND (ch <= CAPITAL_LETTER_Z))
END isAlphaNum;


PROCEDURE isUpper ( ch : CHAR ) : BOOLEAN;
(* Returns TRUE if ch is an uppercase letter, otherwise FALSE. *)

BEGIN
  RETURN (ch >= CAPITAL_LETTER_A) AND (ch <= CAPITAL_LETTER_Z)
END isUpper;


PROCEDURE isLower ( ch : CHAR ) : BOOLEAN;
(* Returns TRUE if ch is a lowercase letter, otherwise FALSE. *)

BEGIN
  RETURN (ch >= SMALL_LETTER_A) AND (ch <= SMALL_LETTER_Z)
END isUpper;


PROCEDURE toUpper ( VAR ch : CHAR );
(* Passes back the uppercase equivalent of ch if ch is a lowercase letter. *)

BEGIN
  IF (ch >= SMALL_LETTER_A) AND (ch <= SMALL_LETTER_Z) THEN
    ch := CHR(ORD(ch) + 32)
  END
END toUpper;


PROCEDURE toLower ( VAR ch : CHAR );
(* Passes back the lowercase equivalent of ch if ch is an uppercase letter. *)

BEGIN
  IF (ch >= CAPITAL_LETTER_A) AND (ch <= CAPITAL_LETTER_Z) THEN
    ch := CHR(ORD(ch) - 32)
  END
END toLower;


END ASCII.