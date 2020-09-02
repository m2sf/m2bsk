(*!m2pim*) (* Copyright (c) 2017 Modula-2 Software Foundation *)

IMPLEMENTATION MODULE MatchLex;

(* Lexer Support Library for Modula-2 R10 Bootstrap Kernel *)

IMPORT ISO646, Char, Capabilities, Infile, Token;

FROM Token IMPORT TokenT;
FROM Infile IMPORT InfileT;


(* Semantic Symbols *)

(* ---------------------------------------------------------------------------
 * procedure StdIdent ( infile )
 *  matches the input in infile to an identifier
 * ---------------------------------------------------------------------------
 * EBNF
 *
 * StdIdent :=
 *   Letter ( Letter | Digit )*  ;
 *
 * pre-conditions:
 *  (1) infile is the current input file and it must not be NIL.
 *  (2) lookahead of infile is the first character of the identifier.
 *
 * post-conditions:
 *  (1) lookahead of infile is the character immediately following the last
 *      character of the identifier whose first character was the
 *      lookahead of infile upon entry into the procedure.
 *
 * error-conditions:
 *  (1) maximum length exceeded
 *      TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE StdIdent ( infile : InfileT );
  
VAR
  next : CHAR;
  
BEGIN
  (* collect all letters and digits *)
  next := Infile.lookaheadChar(infile);
  WHILE (next >= 'a') AND (next <= 'z')
    OR (next >= 'A') AND (next <= 'Z')
    OR (next >= '0') AND (next <= '9') DO
    next := Infile.consumeChar(infile)
  END (* WHILE *)
END StdIdent;


(* ---------------------------------------------------------------------------
 * procedure IdentOrResword ( infile, allcaps )
 *  matches the input in infile to an identifier or reserved word
 * ---------------------------------------------------------------------------
 * EBNF
 *
 * IdentOrResword :=
 *   Letter ( Letter | Digit )*
 *   ;
 *
 *
 * pre-conditions:
 *  (1) infile is the current input file and it must not be NIL.
 *  (2) lookahead of infile is the first character of the identifier or RW.
 *
 * post-conditions:
 *  (1) lookahead of infile is the character immediately following the last
 *      character of the identifier or RW whose first character was the
 *      lookahead of infile upon entry into the procedure.
 *  (2) if the input consists exclusively of uppercase letters then
 *      TRUE is passed back in parameter allcaps otherwise FALSE is passed
 *
 * error-conditions:
 *  (1) maximum length exceeded
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE IdentOrResword ( infile : InfileT; VAR allcaps : BOOLEAN );

VAR
  next : CHAR;
 
BEGIN
  (* collect all uppercase letters *)
  next := Infile.lookaheadChar(infile);
  WHILE (next >= 'A') AND (next <= 'Z') DO
    next := Infile.consumeChar(infile)
  END; (* WHILE *)
  
  (* check if followed by lowercase letter or digit *)
  IF (next >= 'a') AND (next <= 'z') OR (next >= '0') AND (next <= '9') THEN
    allcaps := FALSE;
    next := Infile.consumeChar(infile);
    
    (* collect any remaining letters and digits *)
    WHILE (next >= 'a') AND (next <= 'z')
      OR (next >= 'A') AND (next <= 'Z')
      OR (next >= '0') AND (next <= '9') DO
      next := Infile.consumeChar(infile)
    END (* WHILE *)

  ELSE (* only uppercase letters found *)
    allcaps := TRUE
  END (* IF *)
 
END IdentOrResword;


(* ---------------------------------------------------------------------------
 * procedure NumericLiteral ( infile, token )
 *  matches the input in infile to a numeric literal
 * ---------------------------------------------------------------------------
 * EBNF
 *
 * NumericLiteral :=
 *   '0' ( RealNumberTail | NonDecimalNumberTail )? |
 *   ( '1' .. '9' ) DecimalNumberTail?
 *   ;
 *
 * NonDecimalNumberTail :=
 *   'b' Base2DigitSeq | ( 'u' | 'x' ) Base16DigitSeq
 *   ;
 *
 * pre-conditions:
 *  (1) infile is the current input file and it must not be NIL.
 *  (2) lookahead of infile is the first digit of the literal.
 *
 * post-conditions:
 *  (1) lookahead of infile is the character immediately following the last
 *      digit of the literal whose first digit was the lookahead of infile
 *      upon entry into the procedure.
 *  (2) if the numeric literal represents a whole number,
 *       token value WholeNumber is passed back in token.
 *      if the numeric literal represents a character code,
 *       token value QuotedChar is passed back in token.
 *      if the numeric literal represents a real number,
 *       token value RealNumber is passed back in token.
 *
 * error-conditions:
 *  (1) missing digit after prefix
 *       TO DO
 *  (2) missing fractional part after decimal point
 *       TO DO
 *  (3) missing exponent part after exponent prefix
 *       TO DO
 *  (4) maximum length exceeded
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE NumericLiteral ( infile : InfileT; VAR token : TokenT );

VAR
  ch, next : CHAR;

BEGIN
  
  ch := Infile.lookaheadChar(infile);
  next := Infile.consumeChar(infile);
  
  IF ch = '0' THEN
        
    CASE next OF
      '.' : (* sole '0' or real number *)
      IF Infile.la2Char(infile) # '.' THEN
        (* real number found *)
        next := matchRealNumberTail(infile)        
      END (* IF *)
      
    | 'b' : (* base-2 integer *)
      next := matchBase2DigitSeq(infile)
      
    | 'u' : (* character code *)
      next := matchBase16DigitSeq(infile)
      
    | 'x' : (* base-16 integer *)
      next := matchBase16DigitSeq(infile)
      
    END (* CASE *)
       
  ELSIF ch >= '1' AND ch <= '9' THEN
    (* decimal integer or real number *)
    next := matchDecimalNumberTail(infile)    
  END (* IF *)
  
END NumericLiteral;


(* ---------------------------------------------------------------------------
 * procedure QuotedLiteral ( infile, token )
 *  matches the input in infile to a quoted literal
 * ---------------------------------------------------------------------------
 * EBNF
 *
 * QuotedLiteral :=
 *   SingleQuotedLiteral | DoubleQuotedLiteral
 *   ;
 *
 * SingleQuotedLiteral :=
 *   "'" ( QuotableCharacter | '"' )* "'"
 *   ;
 *
 * DoubleQuotedLiteral :=
 *   '"' ( QuotableCharacter | "'" )* '"'
 *   ;
 *
 * pre-conditions:
 *  (1) infile is the current input file and it must not be NIL.
 *  (2) lookahead of infile is the opening quotation mark of the literal.
 *
 * post-conditions:
 *  (1) lookahead of infile is the character immediately following the closing
 *      quotation mark that closes the literal whose opening quotation mark
 *      was the lookahead of infile upon entry into the procedure.
 *  (2) if the quoted literal represents the empty string or a single
 *      character, token value quotedChar is passed back in token.
 *      Otherwise, token value quotedString is passed back in token.
 *
 * error-conditions:
 *  (1) eof reached
 *       TO DO
 *  (2) illegal character encountered
 *       TO DO
 *  (3) unescaped backslash encountered
 *       TO DO
 *  (4) maximum length exceeded
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE QuotedLiteral ( infile : InfileT; token : TokenT );

VAR
  next, delimiter : CHAR;

BEGIN
  
  (* consume string delimiter *)
  delimiter := Infile.lookaheadChar(infile);
  next := Infile.consumeChar(infile);
 
  
  WHILE next # delimiter DO
    
    (* check for control characters *)
    IF Char.isControl(next) THEN
      
      IF next = ISO646.NEWLINE THEN
        
        (* error: new line in string literal *)
        
      ELSIF Infile.eof(infile) THEN
        
        (* error: EOF in string literal *)
        
      ELSE (* any other control character *)
        
        (* error: illegal character in string literal *)
        
      END (* IF *)
    END (* IF *)
    
    (* check for escape sequence *)
    IF next = ISO646.BACKSLASH THEN
      
      next := Infile.consumeChar(infile);
      
      IF (next # 'n') AND (next # = 't') AND (next # ISO646.BACKSLASH) THEN
        
        (* error: invalid escape sequence *)
        
      END (* IF *)
    END (* IF *)
    
    next := Infile.consumeChar(infile)
  END (* WHILE *)
  
  (* consume closing delimiter *)
  IF next = delimiter THEN
    next := Infile.consumeChar(infile)
  END (* IF *)
  
END QuotedLiteral;


(* Non-Semantic Symbols *)

(* ---------------------------------------------------------------------------
 * procedure Pragma ( infile )
 *  matches the input in infile to a pragma
 * ---------------------------------------------------------------------------
 * EBNF
 *
 * Pragma :=
 *   '<*' ( QuotableCharacter | QuotedLiteral )* '*>'
 *   ;
 *
 * pre-conditions:
 *  (1) infile is the current input file and it must not be NIL.
 *  (2) lookahead of infile is the first character of the opening pragma
 *      delimiter.
 *
 * post-conditions:
 *  (1) lookahead of infile is the character immediately following the last
 *      character of the closing delimiter that closes the pragma whose
 *      opening delimiter was the lookahead of infile upon entry into the
 *      procedure.
 *
 * error-conditions:
 *  (1) eof reached
 *       TO DO
 *  (2) illegal character encountered
 *       TO DO
 *  (3) maximum length exceeded
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE Pragma ( infile : InfileT );

VAR
  next : CHAR;
  delimiterFound : BOOLEAN;
  
BEGIN
  
  delimiterFound := FALSE;
  
  (* consume opening '<' and '*' *)
  next := Infile.consumeChar(infile);
  next := Infile.consumeChar(infile);
  
  WHILE NOT delimiterFound DO
  
    IF next = '*' AND Infile.la2Char(infile) = '>' THEN
      delimiterFound := TRUE;
      
      (* consume closing '*' and '>' *)
      next := Infile.consumeChar(infile);
      next := Infile.consumeChar(infile);
    ELSE (* not closing delimiter *)
    
      (* consume this character *)
      next := Infile.consumeChar(infile)
      
      (* TO DO check for eof, illegal chars, report diagnostics *)
      
    END (* IF *)
  END (* WHILE *)
  
 END Pragma;


(* ---------------------------------------------------------------------------
 * procedure LineComment ( infile )
 *  matches the input in infile to a line comment
 * ---------------------------------------------------------------------------
 * EBNF
 *
 * LineComment :=
 *   '!' CommentCharacter* EndOfLine
 *   ;
 *
 * pre-conditions:
 *  (1) infile is the current input file and it must not be NIL.
 *  (2) lookahead of infile is the opening exclamation point of the comment.
 *
 * post-conditions:
 *  (1) if the comment is terminated by end-of-line:
 *       lookahead of infile is the new-line character that closes the
 *       comment whose opening exclamation point was the lookahead of infile
 *       upon entry into the procedure, or
 *      if the comment is terminated by end-of-file:
 *       the last character in the input file has been consumed.
 *
 * error-conditions:
 *  (1) illegal character encountered
 *       TO DO
 *  (2) maximum comment length exceeded
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE LineComment ( infile : InfileT );

VAR
  next : CHAR;
  
BEGIN

  REPEAT
    next := Infile.consumeChar(infile);
  UNTIL (next = ISO646.NEWLINE) OR Infile.eof(infile)
    
END LineComment;


(* ---------------------------------------------------------------------------
 * procedure BlockComment ( infile )
 *  matches the input in infile to a block comment
 * ---------------------------------------------------------------------------
 * EBNF
 *
 * BlockComment :=
 *   '(' '*' ( CommentCharacter | BlockComment | EndOfLine )* '*' ')'
 *   ;
 *
 * pre-conditions:
 *  (1) infile is the current input file and it must not be NIL.
 *  (2) lookahead of infile is the opening parenthesis of a block comment.
 *
 * post-conditions:
 *  (1) lookahead of infile is the character immediately following the closing
 *      parenthesis that closes the block comment whose opening parenthesis
 *      was the lookahead of infile upon entry into the procedure.
 *
 * error-conditions:
 *  (1) eof reached
 *       TO DO
 *  (2) illegal character encountered
 *       TO DO
 *  (3) maximum comment length exceeded
 *       TO DO
 *  (4) maximum nesting level exceeded
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE BlockComment ( infile : InfileT );

VAR
  ch, next : CHAR;
  nestLevel : CARDINAL;
  
BEGIN
  
  nestLevel := 1;
  ch := Infile.lookaheadChar(infile);
  
  WHILE (nestLevel > 0) AND NOT Infile.eof(infile) DO
    next := Infile.consumeChar(infile);
     
    IF (ch = '*') AND (next = ')') THEN
      ch := next; next := Infile.consumeChar(infile);
      nestLevel := nextLevel - 1
    
    ELSIF (ch = '(') AND (next = '*') THEN
      ch := next; next := Infile.consumeChar(infile);
      nestLevel := nestLevel + 1
      
    END (* IF *)
    
  END (* WHILE *)
  
  (* TO DO : diagnostics *)

END BlockComment;


(* Disabled Code Sections *)

(* ---------------------------------------------------------------------------
 * procedure DisabledCode ( infile )
 *  matches the input in infile to a disabled code block
 * ---------------------------------------------------------------------------
 * EBNF
 *
 * DisabledCode :=
 *   ( StartOfSourceFile | EndOfLine ) '?' '<'
 *   ( PrintableCharacter | Tabulator | EndOfLine )*
 *   EndOfLine '>' '?'
 *   ;
 *
 * pre-conditions:
 *  (1) infile is the current input source and it must not be NIL.
 *  (2) lookahead of infile is the opening '?' of a disabled code block.
 *
 * post-conditions:
 *  (1) lookahead of inflie is the character immediately following the
 *      closing '?' that closes the disabled code block whose opening '?'
 *      was the lookahead of infile upon entry into the procedure.
 *
 * error-conditions:
 *  (1) illegal character encountered
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE DisabledCode ( infile : InfileT );

VAR
  next : CHAR;
  delimiterFound : BOOLEAN;

BEGIN

  delimiterFound := FALSE;
  
  (* consume opening '?' and '<' *)
  next := Infile.consumeChar(infile);
  next := Infile.consumeChar(infile);
    
  WHILE NOT delimiterFound AND NOT Infile.eof(infile) DO
    
    (* check for closing delimiter *)
    IF next = '>' AND
      (Infile.column(infile) = 1) AND (Infile.la2Char(infile) = '?') THEN
      delimiterFound := TRUE;
      
      (* consume closing '>' and '?' *)
      next := Infile.consumeChar(infile);
      next := Infile.consumeChar(infile);
      
    ELSE (* not closing delimiter *)
      (* consume this character *)
      next := Infile.consumeChar(infile)
      
      (* TO DO check for illegal chars, report diagnostics *)
      
    END (* IF *)
    
  END (* WHILE *)
    
END DisabledCode;


(* Private Procedures *)

(* ---------------------------------------------------------------------------
 * function matchDecimalNumberTail ( infile )
 *  matches the input in infile to a decimal number tail, returns lookahead
 * ---------------------------------------------------------------------------
 * EBNF
 *
 * DecimalNumberTail :=
 *   DigitSep? DigitSeq RealNumberTail?
 *   ;
 *
 * alias DigitSep = "'" ;
 *
 * pre-conditions:
 *  (1) infile is the current input file and it must not be NIL.
 *  (2) lookahead of infile is a digit between 1 and 9 or a decimal point.
 *
 * post-conditions:
 *  (1) lookahead of infile is the character immediately following the last
 *      digit of the literal whose first digit was the lookahead of infile
 *      upon entry into the procedure.
 *
 * error-conditions:
 *  (1) illegal character encountered
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE matchDecimalNumberTail ( infile : InfileT ) : CHAR;

VAR
  next : CHAR;
  
BEGIN
  
  next := Infile.consumeChar(infile);
  
  (* DigitSep? *)
  IF next = DigitSeparator THEN
    next := Infile.consumeChar(infile);
  END; (* IF *)
  
  (* DigitSep *)
  IF Char.isDigit(next) THEN
    next := DigitSeq(infile)
    
  ELSE (* error: lookahead is not a decimal digit *)
    
    (* TO DO *)
    
  END; (* IF *)
  
  (* RealNumberTail? *)
  IF next = DecimalPoint THEN
    next := RealNumberTail(infile)
  END; (* IF *)
  
  RETURN next
END matchDecimalNumberTail;


(* ---------------------------------------------------------------------------
 * function matchRealNumberTail ( infile )
 *  matches the input in infile to a real number tail, returns lookahead
 * ---------------------------------------------------------------------------
 * EBNF
 *
 * RealNumberTail :=
 *   '.' DigitSeq ( 'e' ( '+' | '-' )? DigitSeq )?
 *   ;
 *
 * pre-conditions:
 *  (1) infile is the current input file and it must not be NIL.
 *  (2) lookahead of infile is a decimal point.
 *
 * post-conditions:
 *  (1) lookahead of infile is the character immediately following the last
 *      digit of the literal whose decimal point was the lookahead of infile
 *      upon entry into the procedure.
 *
 * error-conditions:
 *  (1) illegal character encountered
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE matchRealNumberTail ( infile : InfileT ) : CHAR;

VAR
  next : CHAR;
  
BEGIN
  
  (* '.' *)
  next := Infile.consumeChar(infile);
  
  (* DigitSeq *)
  IF Char.isDigit(next) THEN
    next := DigitSeq(infile)
    
  ELSE (* error: lookahead is not a decimal digit *)
    
    (* TO DO *)
    
  END; (* IF *)
  
  (* exponent? *)
  IF next = 'e' THEN
    (* consume 'e' *)
    next := Infile.consumeChar(infile);
    
    (* ( '+' | '-' )?  *)
    IF (next = '+') OR (next = '-') THEN
      (* consume sign *)
      next := Infile.consumeChar(infile)
    END; (* IF *)
    
    (* DigitSeq *)
    IF Char.isDigit(next) THEN
      next := DigitSeq(infile)
      
    ELSE (* error: lookahead is not a decimal digit *)
      
      (* TO DO *)

    END (* IF *)
  END; (* IF *)
  
  RETURN next
END matchRealNumberTail;


(* ---------------------------------------------------------------------------
 * function matchDigitSeq ( infile )
 *  matches the input in infile to a base-2 digit sequence, returns lookahead
 * ---------------------------------------------------------------------------
 * EBNF
 *
 * matchDigitSeq :=
 *   Digit+ ( DigitSep Digit+ )*
 *   ;
 *
 * pre-conditions:
 *  (1) s is the current input source and it must not be NIL.
 *  (2) lookahead of s is a base-2 digit.
 *
 * post-conditions:
 *  (1) lookahead of s is the character immediately following the last digit
 *      of the literal whose first digit was the lookahead of s upon entry
 *      into the procedure.
 *
 * error-conditions:
 *  (1) illegal character encountered
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE matchDigitSeq ( infile : InfileT );

VAR
  next : CHAR;
  
BEGIN
  
  (* TO DO *)
  
  RETURN next
END matchDigitSeq;


(* ---------------------------------------------------------------------------
 * function matchBase2DigitSeq ( infile )
 *  matches the input in infile to a base-2 digit sequence, returns lookahead
 * ---------------------------------------------------------------------------
 * EBNF
 *
 * Base2DigitSeq :=
 *   Base2Digit+ ( DigitSep Base2Digit+ )*
 *   ;
 *
 * pre-conditions:
 *  (1) infile is the current input file and it must not be NIL.
 *  (2) lookahead of infile is a base-2 digit.
 *
 * post-conditions:
 *  (1) lookahead of infile is the character immediately following the last
 *      digit of the literal whose first digit was the lookahead of infile
 *      upon entry into the procedure.
 *
 * error-conditions:
 *  (1) illegal character encountered
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE matchBase2DigitSeq ( infile : InfileT ) : CHAR;

VAR
  next : CHAR;
  
BEGIN
  
  (* TO DO *)
  
  RETURN next
END matchBase2DigitSeq;


(* ---------------------------------------------------------------------------
 * function matchBase16DigitSeq ( infile )
 *  matches the input in infile to a base-16 digit sequence, returns lookahead
 * ---------------------------------------------------------------------------
 * EBNF
 *
 * Base16DigitSeq :=
 *   Base16Digit+ ( DigitSep Base16Digit+ )*
 *   ;
 *
 * pre-conditions:
 *  (1) infile is the current input file and it must not be NIL.
 *  (2) lookahead of infile is a base-16 digit.
 *
 * post-conditions:
 *  (1) lookahead of infule is the character immediately following the last
 *      digit of the literal whose first digit was the lookahead of infile
 *      upon entry into the procedure.
 *
 * error-conditions:
 *  (1) illegal character encountered
 *       TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE matchBase16DigitSeq ( infile : InfileT ) : CHAR;

VAR
  next : CHAR;
  
BEGIN
  
  (* TO DO *)
  
  RETURN next
END matchBase16DigitSeq;


END MatchLex.
