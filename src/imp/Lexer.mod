(*!m2pim*) (* Copyright (c) 2017 Modula-2 Software Foundation. *)

IMPLEMENTATION MODULE Lexer;

(* Lexer for Modula-2 R10 Bootstrap Kernel *)

IMPORT ISO646, Capabilities, Infile, Storage, String, Token, Symbol, MatchLex;

FROM Infile IMPORT InfileT;
FROM String IMPORT StringT;
FROM Token IMPORT TokenT;
FROM Symbol IMPORT SymbolT;


(* Lexer Type *)

TYPE Lexer = POINTER TO LexerDescriptor;

TYPE LexerDescriptor = RECORD
  infile     : InfileT;
  nextSymbol : SymbolT;
  warnings,
  errors     : CARDINAL;
  lastStatus : Status
END; (* LexerDescriptor *)


(* Operations *)

(* ---------------------------------------------------------------------------
 * procedure New ( newLexer, filename, status )
 *  creates a new lexer instance, associated with filename
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE New
  ( VAR newLexer : Lexer; filename : StringT; VAR status : Status );

VAR
  infile : InfileT;
  fstat : BasicFileIO.Status;

BEGIN
 
  (* lexer must not have been initialised *)
  IF newLexer # NIL THEN
    status := Status.AlreadyInitialised;
    RETURN
  END; (* IF *)
  
  (* allocate and initialise source *)
  Infile.New(infile, filename, fstat);
  IF fstat # BasicFileIO.Success THEN
    status := Status.UnableToAllocate;
    RETURN
  END; (* IF *)
  
  (* allocate a lexer instance *)
  Storage.ALLOCATE(newLexer, TSIZE(LexerDescriptor));
  IF newLexer = NIL THEN
    status := Status.UnableToAllocate;
    RELEASE source;
    RETURN
  END; (* IF *)
  
  (* initialise lexer *)
  newLexer^.infile := infile;
  newLexer^.warnings := 0;
  newLexer^.errors := 0;
  newLexer^.lastStatus := Status.Success;
  
  (* read the first symbol to be returned *)
  newLexer^.nextSymbol := Lexer.consumeSym(newLexer);
  
  status := Status.Success;
  RETURN
END New;


(* ---------------------------------------------------------------------------
 * procedure GetSym ( lexer, symbol, lookaheadSymbol )
 *  passes and consumes current lookahead symbol, passes new lookahead symbol
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE GetSym ( lexer : Lexer; VAR sym, next : SymbolT );

BEGIN
  
  (* nextSymbol holds current lookahead, pass it back in sym *)
  sym := lexer^.nextSymbol;
  
  (* consume the current lookahead,
     read the new lookahead symbol, pass it back in next *)
  next := Lexer.consumeSym(lexer);
  
  RETURN
END GetSym;


(* ---------------------------------------------------------------------------
 * procedure consumeSym ( lexer )
 *  consumes current lookahead symbol and returns new lookahead symbol
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE consumeSym ( lexer : Lexer ) : SymbolT;

VAR
  ch, next, la2 : CHAR;
  allcaps : BOOLEAN;
  infile : InfileT;
  sym : SymbolT;

BEGIN
  (* ensure source is valid *)
  IF lexer = NIL THEN
  (* TO DO: report and handle error *)
    RETURN Symbol.NilSymbol
  END; (* IF *)
  
  (* shorthand *)
  infile := lexer^.infile;
  
  (* all decisions are based on lookahead *)
  next := Infile.lookaheadChar(infile);
  
  (* skip any whitespace, tab and new line *)
  WHILE NOT Infile.eof(infile) AND
    (next = ISO646.SPACE) OR (next = ISO646.TAB) OR (next = ISO646.NEWLINE) DO
    next := Infile.consumeChar(infile)
  END; (* WHILE *)
  
  (* skip comments unless comments are to be preserved *)
  IF Capabilities.isDisabled(Capabilities.PreserveComments) THEN
    
    (* skip any line comment *)
    WHILE next = '!' DO
      MatchLex.LineComment(infile);
      next := Infile.lookahead(infile)
    END; (* WHILE *)
    
    (* skip any block comment *)
    WHILE (next = '(') AND (Infile.la2Char(infile) = '*') DO
      MatchLex.BlockComment(infile);
      next := Infile.lookahead(infile)
    END (* WHILE *)
    
  END; (* IF *)
    
  (* skip any disabled code section *)
  WHILE (sym.column = 1) AND
   (next = '?') AND (Infile.la2Char(infile) = '<') DO
    MatchLex.DisabledCodeBlock(infile);
    next := Infile.lookahead(infile);
    sym.column := Infile.column(infile);
 
  END; (* WHILE *)

  (* get current position *)
  sym.line := Infile.line(infile);
  sym.column := Infile.column(infile);
  
  (* check for end-of-file *)
  IF Infile.eof(infile) THEN
    sym.token := Token.EOF;
    sym.lexeme := String.Nil
      
  (* check for any other symbol *)
  ELSE
    CASE next OF
    (* next symbol is line comment *)
      '!' :
        Infile.MarkLexeme(infile);
        MatchLex.LineComment(infile);
        sym.token := Token.LineComment;
        sym.lexeme := Infile.lexeme(infile)
    
    (* next symbol is quoted literal *)
    | ISO646.SINGLEQUOTE,
      ISO646.DOUBLEQUOTE :
        Infile.MarkLexeme(infile);
        MatchLex.QuotedLiteral(infile);
        sym.token := Token.QuotedString;
        sym.lexeme := Infile.lexeme(infile)
    
    (* next symbol is '#' *)
    | '#' :
        (* consume '#' *)
        next := Infile.consumeChar(infile);
        sym.token := Token.NotEqual;
        sym.lexeme := Token.lexemeForToken(Token.NotEqual)
       
    (* next symbol is '&' *)
    | '&' :
        (* consume '&' *)
        next := Infile.consumeChar(infile);
        sym.token := Token.Concat;
        sym.lexeme := Token.lexemeForToken(Token.Concat)
    
    (* next symbol is '(' or block comment *)
    | '(' :
        IF Infile.la2Char(infile) = '*' THEN (* found block comment *)
          Infile.MarkLexeme(source);
          MatchLex.BlockComment(source);
          sym.token := Token.LineComment;
          sym.lexeme := Infile.lexeme(infile)
 
        
        ELSE (* found sole '(' *)
          (* consume '(' *)
          next := Infile.consumeChar(infile);
          sym.token := Token.LParen;
          sym.lexeme := Token.lexemeForToken(Token.LParen)  
        END (* '(' and block comment *)
    
    (* next symbol is ')' *)
    | ')' :
        (* consume ')' *)
        next := Infile.consumeChar(infile);
        sym.token := Token.RParen;
        sym.lexeme := Token.lexemeForToken(Token.RParen)
    
    (* next symbol is '*' *)
    | '*' :
        (* consume '*' *)
        next := Infile.consumeChar(infile);
        sym.token := Token.Asterisk;
        sym.lexeme := Token.lexemeForToken(Token.Asterisk)
    
    (* next symbol is '+' or '++' *)
    | '+' :
        (* consume '+' *)
        next := Source.consumeChar(source);
         
        IF next = '+' THEN (* found '++' *)
          (* consume second '+' *)
          next := Infile.consumeChar(infile);
          sym.token := Token.PlusPlus;
          sym.lexeme := Token.lexemeForToken(Token.PlusPlus)
        
        ELSE (* found sole '+' *)
          (* first '+' already consumed *)
          sym.token := Token.Plus;
          sym.lexeme := Token.lexemeForToken(Token.Plus)
        
        END (* '+' and '++' *)
      
    (* next symbol is ',' *)
    | ',' :
        (* consume ',' *)
        next := Infile.consumeChar(infile);
        sym.token := Token.Comma;
        sym.lexeme := Token.lexemeForToken(Token.Comma)
    
    (* next symbol is '-' or '--' *)
    | '-' :
        (* consume '-' *)
        next := Infile.consumeChar(infile);
         
        IF next = '-' THEN (* found '--' *)
          (* consume second '-' *)
          next := Infile.consumeChar(infile);
          sym.token := Token.MinusMinus;
          sym.lexeme := Token.lexemeForToken(Token.MinusMinus)
        
        ELSE (* found sole '-' *)
          (* first '-' already consumed *)
          sym.token := Token.Minus;
          sym.lexeme := Token.lexemeForToken(Token.Minus)
        
        END (* '-' or '--' *)
    
    (* next symbol is '.', '..' or '.*' *)
    | '.' :
        (* consume '.' *)
        next := Infile.consumeChar(infile);
         
        IF next = '.' THEN (* found '..' *)
          (* consume second '.' *)
          next := Infile.consumeChar(infile);
          sym.token := Token.DotDot;
          sym.lexeme := Token.lexemeForToken(Token.DotDot)
        
        ELSIF next = '*' THEN (* found '.*' *)
          (* consume '*' *)
          next := Infile.consumeChar(infile);
          sym.token := Token.DotStar;
          sym.lexeme := Token.lexemeForToken(Token.DotStar)
        
        ELSE (* found sole '.' *)
          (* first '.' already consumed *)
          sym.token := Token.Dot;
          sym.lexeme := Token.lexemeForToken(Token.Dot)
        
        END (* '.', '..' and '.*' *)
      
    (* next symbol is '/' *)
    | '/' :
        (* consume '/' *)
        next := Infile.consumeChar(infile);
        sym.token := Token.RealDiv;
        sym.lexeme := Token.lexemeForToken(Token.RealDiv)
    
    (* next symbol is numeric literal *)
    | '0' .. '9' :
        Source.MarkLexeme(source);
        MatchLex.NumericLiteral(source, sym.token);
        sym.lexeme := Infile.lexeme(infile)
            
    (* next symbol is ':', ':=' or '::' *)
    | ':' :
        (* consume ':' *)
        next := Source.consumeChar(source);
         
        IF next = '=' THEN (* found ':=' *)
          (* consume '=' *)
          next := Infile.consumeChar(infile);
          sym.token := Token.Assign;
          sym.lexeme := Token.lexemeForToken(Token.Assign)
        
        ELSIF next = ':' THEN (* found '::' *)
          (* consume second ':' *)
          next := Infile.consumeChar(infile);
          sym.token := Token.TypeConv;
          sym.lexeme := Token.lexemeForToken(Token.TypeConv)
        
        ELSE (* found sole ':' *)
          (* first ':' already consumed *)
          sym.token := Token.Colon;
          sym.lexeme := Token.lexemeForToken(Token.Colon)
        
        END (* ':', ':=' and '::' *)
    
    (* next symbol is ';' *)
    | ';' :
        (* consume ';' *)
        next := Infile.consumeChar(infile);
        sym.token := Token.Semicolon;
        sym.lexeme := Token.lexemeForToken(Token.Semicolon)
    
    (* next symbol is '<', '<=' or pragma *)
    | '<' :
        (* peek at second lookahead character *)
        la2 := Infile.la2Char(infile);
        
        IF la2 = '*' THEN (* found '<*' *)
          Infile.MarkLexeme(infile);
          MatchLex.Pragma(source);
          sym.token := Token.Pragma;
          sym.lexeme := Infile.lexeme(infile)
           
        ELSE (* '<' or '<=' *)
          (* consume common '<' *)
          next := Infile.consumeChar(infile);
           
          IF next = '=' THEN (* found '<=' *)
            (* consume '=' *)
            next := Infile.consumeChar(infile);
            sym.token := Token.LessEq;
            sym.lexeme := Token.lexemeForToken(Token.LessEq)
            
          ELSE (* found sole '<' *)
            (* '<' already consumed *)
            sym.token := Token.Less;
            sym.lexeme := Token.lexemeForToken(Token.Less)
          
          END (* '<' or '<=' *)
          
        END (* '<' or '<=' or pragma *)
    
    (* next symbol is '=' *)
    | '=' :
        (* consume '=' *)
        next := Infile.consumeChar(infile);
        sym.token := Token.Equal;
        sym.lexeme := Token.lexemeForToken(Token.Equal)
    
    (* next symbol is '>' or '>=' *)
    | '>' :
        (* consume common '>' *)
        next := Infile.consumeChar(infile);
         
        IF next = '=' THEN (* found '>=' *)
          (* consume '=' *)
          next := Infile.consumeChar(infile);
          sym.token := Token.GreaterEq;
          sym.lexeme := Token.lexemeForToken(Token.GreaterEq)
                  
        ELSE (* found sole '>' *)
          (* '<' already consumed *)
          sym.token := Token.Greater;
          sym.lexeme := Token.lexemeForToken(Token.Greater)
        
        END (* '>' or '>=' *)
    
    (* next symbol is '@' *)
    | '@' :
        (* consume '@' *)
        next := Infile.consumeChar(infile);
        sym.token := Token.AtSign;
        sym.lexeme := Token.lexemeForToken(Token.AtSign)

    (* next symbol is identifier or reserved word *)
    | 'A' .. 'Z' :
        Infile.MarkLexeme(infile);
        MatchLex.IdentOrResword(infile, allcaps);
        sym.lexeme := Infile.lexeme(infile);
        
        IF allcaps THEN (* possibly reserved word *)
          token := Resword.tokenForLexeme(sym.lexeme, Token.StdIdent)

        ELSE (* not reserved word *)
          token := Token.StdIdent

        END (* IF *)
    
    (* next symbol is '[' *)
    | '[' :
        (* consume '[' *)
        next := Infile.consumeChar(infile);
        sym.token := Token.LBracket;
        sym.lexeme := Token.lexemeForToken(Token.LBracket)
    
    (* next symbol is backslash *)
    | ISO646.BACKSLASH :
        (* consume backslash *)
        next := Infile.consumeChar(infile);
        sym.token := Token.SetDiff;
        sym.lexeme := Token.lexemeForToken(Token.SetDiff)
    
    (* next symbol is ']' *)
    | ']' :
        (* consume ']' *)
        next := Infile.consumeChar(infile);
        sym.token := Token.RBracket;
        sym.lexeme := Token.lexemeForToken(Token.RBracket)
    
    (* next symbol is '^' *)
    | '^' :
        (* consume '^' *)
        next := Infile.consumeChar(infile);
        sym.token := Token.Deref;
        sym.lexeme := Token.lexemeForToken(Token.Deref)
    
    (* next symbol is primitive *)
    | '_' :
        Infile.MarkLexeme(infile);
        MatchLex.Primitive(infile);
        sym.token := Token.Primitive;
        sym.lexeme := Infile.lexeme(infile)
    
    (* next symbol is identifier *)
    | 'a' .. 'z' :
        Infile.MarkLexeme(infile);
        MatchLex.StdIdent(infile);
        sym.token := Token.StdIdent;
        sym.lexeme := Infile.lexeme(infile)
    
    (* next symbol is '{' *)
    | '{' :
        (* consume '{' *)
        next := Infile.consumeChar(infile);
        sym.token := Token.LBrace;
        sym.lexeme := Token.lexemeForToken(Token.LBrace)
    
    (* next symbol is '|' *)
    | '|' :
        (* consume '|' *)
        next := Infile.consumeChar(infile);
        sym.token := Token.VerticalBar;
        sym.lexeme := Token.lexemeForToken(Token.VerticalBar)
    
    (* next symbol is '}' *)
    | '}' :
        (* consume '}' *)
        next := Infile.consumeChar(infile);
        sym.token := Token.RBrace;
        sym.lexeme := Token.lexemeForToken(Token.RBrace)
    
    (* next symbol is invalid *)
    ELSE
      Infile.MarkLexeme(infile);
      next := Infile.consumeChar(infile);
      sym.token := Token.Invalid;
      sym.lexeme := Infile.lexeme(infile);
      lexer^.errors++
      
    END (* CASE *)
  END (* IF *)
  
  (* store symbol for use by lookaheadSym *)
  lexer^.nextSymbol := sym;
  
  RETURN sym
END consumeSym;


(* ---------------------------------------------------------------------------
 * procedure lookaheadSym ( lexer ) : M2Symbol
 *  returns current lookahead symbol
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE lookaheadSym ( lexer : Lexer ) : M2Symbol;

BEGIN
  RETURN lexer^.nextSymbol
END lookaheadSym;


(* ---------------------------------------------------------------------------
 * procedure GetStatus ( lexer, status )
 *  returns status of last operation
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE status ( lexer : Lexer ) : Status;

BEGIN

  IF lexer = NIL THEN
    RETURN Status.NotInitialised
  ELSE
    RETURN lexer^.lastStatus
  END

END status;


(* ---------------------------------------------------------------------------
 * procedure warnCount ( lexer ) : CARDINAL
 *  returns current lexical warning count
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE warnCount ( lexer : Lexer ) : CARDINAL;
 (* Returns the lexer's accumulated warning count. *)

BEGIN
  RETURN lexer^.warnings
END warnCount;


(* ---------------------------------------------------------------------------
 * procedure errorCount ( lexer ) : CARDINAL
 *  returns current lexical error count
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE errorCount ( lexer : Lexer ) : CARDINAL;
 (* Returns the lexer's accumulated error count. *)

BEGIN
  RETURN lexer^.errors
END errorCount;


(* ---------------------------------------------------------------------------
 * procedure release ( lexer )
 *  releases lexer instance
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  (1) lexer must not be NIL
 *
 * post-conditions:
 *  (1) lexer is deallocated
 *  (2) NIL is passed back in lexer
 *
 * error-conditions:
 *  (1) reference to lexer remains unmodified
 * ---------------------------------------------------------------------------
 *)
PROCEDURE Release ( VAR lexer : Lexer );
  
BEGIN

  (* lexer must not be NIL *)
  IF lexer = NIL THEN
    RETURN
  END;
  
  (* release source and lexer *)
  Infile.Close(lexer^.infile);
  Storage.DEALLOCATE(lexer, TSIZE(LexerDescriptor));
END Release;


END Lexer.
