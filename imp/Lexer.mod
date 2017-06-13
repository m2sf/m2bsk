(*!m2pim*) (* Copyright (c) 2015 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE Lexer;

(* Lexer for Modula-2 R10 Bootstrap Kernel *)

IMPORT ASCII, Capabilities, String, Source, Token, Symbol, MatchLex;

FROM String IMPORT StringT;
FROM Source IMPORT SourceT;
FROM Token IMPORT TokenT;
FROM Symbol IMPORT SymbolT;


(* Lexer Type *)

TYPE Lexer = POINTER TO LexerDescriptor;

TYPE LexerDescriptor = RECORD
  source     : SourceT;
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
PROCEDURE New ( VAR newLexer : Lexer; filename : StringT; VAR s : Status );

VAR
  source : SourceT;
  sourceStatus : Source.Status;

BEGIN
 
  (* lexer must not have been initialised *)
  IF newLexer # NIL THEN
    status := Status.AlreadyInitialised;
    RETURN
  END;
  
  (* allocate and initialise source *)
  Source.New(source, filename, sourceStatus);
  IF sourceStatus # Source.Status.Success THEN
    s := Status.UnableToAllocate;
    RETURN
  END;
  
  (* allocate a lexer instance *)
  NEW newLexer;
  IF newLexer = NIL THEN
    s := Status.UnableToAllocate;
    RELEASE source;
    RETURN
  END;
  
  (* initialise lexer *)
  newLexer^.source := source;
  newLexer^.warnings := 0;
  newLexer^.errors := 0;
  newLexer^.lastStatus := Status.Success;
  
  (* read the first symbol to be returned *)
  newLexer^.nextSymbol := Lexer.consumeSym(newLexer);
  
  s := Status.Success;
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
  source : Source;
  sym : SymbolT;

BEGIN
  (* ensure source is valid *)
  IF lexer = NIL THEN
  (* TO DO: report and handle error *)
    RETURN Symbol.NilSymbol
  END;
  
  (* shorthand *)
  source := lexer^.source;
  
  (* all decisions are based on lookahead *)
  next := source.lookaheadChar();
  
  (* skip any whitespace, tab and new line *)
  WHILE NOT Source.eof(source) AND
    (next = ASCII.SPACE OR next = ASCII.TAB OR next = ASCII.NEWLINE) DO
    next := Source.consumeChar(source)
  END; (* WHILE *)
  
  (* skip comments unless comments are to be preserved *)
  IF Capabilities.isDisabled(Capabilities.PreserveComments) THEN
    
    (* skip any line comment *)
    WHILE next = "!" DO
      MatchLex.LineComment(source, sym.token);
      next := Source.lookahead(source)
    END; (* WHILE *)
    
    (* skip any block comment *)
    WHILE next = "(" AND Source.la2Char(source) = "*" DO
      MatchLex.BlockComment(source, sym.token);
      next := Source.lookahead(source)
    END (* WHILE *)
    
  END; (* IF *)
  
  (* get current position *)
  Source.GetLineAndColumn(source, sym.line, sym.column);
  
  (* skip any disabled code section *)
  WHILE next = "?" AND sym.column = 1 DO
    MatchLex.DisabledCodeBlock(source);
    next := Source.lookahead(source);
    Source.GetLineAndColumn(source, sym.line, sym.column)
  END; (* WHILE *)
  
  (* check for end-of-file *)
  IF Source.eof(source) THEN
    sym.token := TokenT.EOF;
    sym.lexeme := 0
      
  (* check for any other symbol *)
  ELSE
    CASE next OF
    
    (* next symbol is line comment *)
    | "!" :
        Source.MarkLexeme(source, sym.line, sym.column);
        MatchLex.LineComment(source, sym.token);
        Source.CopyLexeme(source, lexer^.dict, sym.lexeme)
    
    (* next symbol is quoted literal *)
    | '"', "'" :
        Source.MarkLexeme(source, sym.line, sym.column);
        MatchLex.QuotedLiteral(source, sym.token);
        Source.CopyLexeme(source, lexer^.dict, sym.lexeme)
    
    (* next symbol is "#" *)
    | "#" :
        (* consume "#" *)
        next := Source.consumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.token := TokenT.NotEqual;
        sym.lexeme := Token.lexemeForToken(TokenT.NotEqual)
    
    (* next symbol is foreign identifier *)
    | "$" :
        Source.MarkLexeme(source, sym.line, sym.column);
        MatchLex.ForeignIdent(source, sym.token);
        Source.CopyLexeme(source, lexer^.dict, sym.lexeme)
    
    (* next symbol is "&" *)
    | "&" :
        (* consume "&" *)
        next := Source.consumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.token := TokenT.Concat;
        sym.lexeme := Token.lexemeForToken(TokenT.Concat)
    
    (* next symbol is "(" or block comment *)
    | "(" :
        IF Source.la2Char(source) = "*" THEN (* found block comment *)
          Source.MarkLexeme(source, sym.line, sym.column);
          MatchLex.BlockComment(source, sym.token);
          Source.CopyLexeme(source, lexer^.dict, sym.lexeme)
        
        ELSE (* found "(" *)
          (* consume "(" *)
          next := Source.consumeChar(source);
          Source.GetLineAndColumn(source, sym.line, sym.column);
          sym.token := TokenT.LParen;
          sym.lexeme := Token.lexemeForToken(TokenT.LParen)
          
        END (* "(" and block comment *)
    
    (* next symbol is ")" *)
    | ")" :
        (* consume ")" *)
        next := Source.consumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.value := TokenT.RParen;
        sym.lexeme := Token.lexemeForToken(TokenT.RParen)
    
    (* next symbol is "*" *)
    | "*" :
        (* consume "*" *)
        next := Source.consumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.token := TokenT.Asterisk;
        sym.lexeme := Token.lexemeForToken(TokenT.Asterisk)
    
    (* next symbol is "+" or "++" *)
    | "+" :
        (* consume "+" *)
        next := Source.consumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        
        IF next = "+" THEN (* found "++" *)
          (* consume second "+" *)
          next := Source.consumeChar(source);
          sym.token := TokenT.PlusPlus;
          sym.lexeme := Token.lexemeForToken(TokenT.PlusPlus)
        
        ELSE (* found sole "+" *)
          (* first "+" already consumed *)
          sym.token := TokenT.Plus;
          sym.lexeme := Token.lexemeForToken(TokenT.Plus)
        
        END (* "+" and "++" *)
      
    (* next symbol is "," *)
    | "," :
        (* consume "," *)
        next := Source.consumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.token := TokenT.Comma;
        sym.lexeme := Token.lexemeForToken(TokenT.Comma)
    
    (* next symbol is "-" or "--" *)
    | "-" :
        (* consume "-" *)
        next := Source.consumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        
        IF next = "-" THEN (* found "--" *)
          (* consume second "-" *)
          next := Source.consumeChar(source);
          sym.token := TokenT.MinusMinus;
          sym.lexeme := Token.lexemeForToken(TokenT.MinusMinus)
        
        ELSE (* found sole "-" *)
          (* first "-" already consumed *)
          sym.token := TokenT.Minus;
          sym.lexeme := Token.lexemeForToken(TokenT.Minus)
        
        END (* "-" or "--" *)
    
    (* next symbol is ".", ".." or ".*" *)
    | "." :
        (* consume "." *)
        next := Source.consumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        
        IF next = "." THEN (* found ".." *)
          (* consume second "." *)
          next := Source.consumeChar(source);
          sym.token := TokenT.DotDot;
          sym.lexeme := Token.lexemeForToken(TokenT.DotDot)
        
        ELSIF next = "*" THEN (* found ".*" *)
          (* consume "*" *)
          next := Source.consumeChar(source);
          sym.token := TokenT.DotStar;
          sym.lexeme := Token.lexemeForToken(TokenT.DotStar)
        
        ELSE (* found sole "." *)
          (* first "." already consumed *)
          sym.token := TokenT.Dot;
          sym.lexeme := Token.lexemeForToken(TokenT.Dot)
        
        END (* ".", ".." and ".*" *)
      
    (* next symbol is "/" *)
    | "/" :
        (* consume "/" *)
        next := Source.consumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.token := TokenT.RealDiv;
        sym.lexeme := Token.lexemeForToken(TokenT.RealDiv)
    
    (* next symbol is numeric literal *)
    | "0" .. "9" :
        Source.MarkLexeme(source, sym.line, sym.column);
        MatchLex.NumericLiteral(source, sym.token);
        Source.CopyLexeme(source, lexer^.dict, sym.lexeme)
    
    (* next symbol is ":", ":=" or "::" *)
    | ":" :
        (* consume ":" *)
        next := Source.consumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        
        IF next = "=" THEN (* found ":=" *)
          (* consume "=" *)
          next := Source.consumeChar(source);
          sym.token := TokenT.Assign;
          sym.lexeme := Token.lexemeForToken(TokenT.Assign)
        
        ELSIF next = ":" THEN (* found "::" *)
          (* consume second ":" *)
          next := Source.consumeChar(source);
          sym.token := TokenT.TypeConv;
          sym.lexeme := Token.lexemeForToken(TokenT.TypeConv)
        
        ELSE (* found sole ":" *)
          (* first ":" already consumed *)
          sym.token := TokenT.Colon;
          sym.lexeme := Token.lexemeForToken(TokenT.Colon)
        
        END (* ":", ":=" and "::" *)
    
    (* next symbol is ";" *)
    | ";" :
        (* consume ";" *)
        next := Source.consumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.token := TokenT.Semicolon;
        sym.lexeme := Token.lexemeForToken(TokenT.Semicolon)
    
    (* next symbol is "<", "<=" or pragma *)
    | "<" :
        (* peek at second lookahead character *)
        la2 := Source.la2Char(source);
        
        IF la2 = "*" THEN (* found "<*" *)
          Source.MarkLexeme(source, sym.line, sym.column);
          MatchLex.Pragma(source, sym.token);
          Source.CopyLexeme(source, lexer^.dict, sym.lexeme)
          
        ELSE (* "<" or "<=" *)
          (* consume common "<" *)
          next := Source.consumeChar(source);
          Source.GetLineAndColumn(source, sym.line, sym.column);
                            
          IF next = "=" THEN (* found "<=" *)
            (* consume "=" *)
            next := Source.consumeChar(source);
            sym.token := TokenT.LessEq;
            sym.lexeme := Token.lexemeForToken(TokenT.LessEq)
            
          ELSE (* found sole "<" *)
            (* "<" already consumed *)
            sym.token := TokenT.Less;
            sym.lexeme := Token.lexemeForToken(TokenT.Less)
          
          END (* "<" or "<=" *)
          
        END (* chevron text or pragma *)
    
    (* next symbol is "=" *)
    | "=" :
        (* consume "=" *)
        next := Source.consumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.token := TokenT.Equal;
        sym.lexeme := Token.lexemeForToken(TokenT.Equal)
    
    (* next symbol is ">" or ">=" *)
    | ">" :
        (* consume common ">" *)
        next := Source.consumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        
        IF next = "=" THEN (* found ">=" *)
          (* consume "=" *)
          next := Source.consumeChar(source);
          sym.token := TokenT.GreaterEq;
          sym.lexeme := Token.lexemeForToken(TokenT.GreaterEq)
                  
        ELSE (* found sole ">" *)
          (* "<" already consumed *)
          sym.token := TokenT.Greater;
          sym.lexeme := Token.lexemeForToken(TokenT.Greater)
        
        END (* ">" or ">=" *)
    
    (* next symbol is identifier or reserved word *)
    | "A" .. "Z" :
        Source.MarkLexeme(source, sym.line, sym.column);
        MatchLex.IdentOrResword(source, sym.token);
        Source.CopyLexeme(source, lexer^.dict, sym.lexeme)
    
    (* next symbol is "[" *)
    | "[" :
        (* consume "[" *)
        next := Source.consumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.token := TokenT.LBracket;
        sym.lexeme := Token.lexemeForToken(TokenT.LBracket)
    
    (* next symbol is backslash *)
    | ASCII.BACKSLASH :
        (* consume backslash *)
        next := Source.consumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.token := TokenT.SetDiff;
        sym.lexeme := Token.lexemeForToken(TokenT.SetDiff)
    
    (* next symbol is "]" *)
    | "]" :
        (* consume "]" *)
        next := Source.consumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.token := TokenT.RBracket;
        sym.lexeme := Token.lexemeForToken(TokenT.RBracket)
    
    (* next symbol is "^" *)
    | "^" :
        (* consume "^" *)
        next := Source.consumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.token := TokenT.Deref;
        sym.lexeme := Token.lexemeForToken(TokenT.Deref)
    
    (* next symbol is identifier *)
    | "a" .. "z" :
        Source.MarkLexeme(source, sym.line, sym.column);
        MatchLex.Ident(source, sym.token);
        Source.CopyLexeme(source, lexer^.dict, sym.lexeme)
    
    (* next symbol is "{" *)
    | "{" :
        (* consume "{" *)
        next := Source.consumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.token := TokenT.LBrace;
        sym.lexeme := Token.lexemeForToken(TokenT.LBrace)
    
    (* next symbol is "|" *)
    | "|" :
        (* consume "|" *)
        next := Source.consumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.token := TokenT.VerticalBar;
        sym.lexeme := Token.lexemeForToken(TokenT.VerticalBar)
    
    (* next symbol is "}" *)
    | "}" :
        (* consume "}" *)
        next := Source.consumeChar(source);
        Source.GetLineAndColumn(source, sym.line, sym.column);
        sym.token := TokenT.RBrace;
        sym.lexeme := Token.lexemeForToken(TokenT.RBrace)
    
    (* next symbol is invalid *)
    ELSE
      Source.MarkLexeme(source, sym.line, sym.column);
      next := Source.consumeChar(source);
      sym.token := TokenT.Invalid;
      Source.CopyLexeme(source, lexer^.dict, sym.lexeme);
      lexer^.errors++
      
    END; (* CASE *)
  
  END (* IF *);
  
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
  Source.Release(lexer^.source);
  RELEASE lexer
END Release;


END Lexer.