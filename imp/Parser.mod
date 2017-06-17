(*!m2pim*) (* Copyright (c) 2015 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE Parser;

(* Parser for Modula-2 R10 Bootstrap Kernel *)

IMPORT
  AST, AstNodeType, AstQueue, NonTerminals,
  Lexer, LexQueue, Symbol, Token, TokenSet, String;

FROM AST IMPORT AstT; (* alias for AST.AST *)
FROM AstNodeType IMPORT AstNodeTypeT; (* alias for AstNodeType.AstNodeType *)
FROM AstQueue IMPORT AstQueueT; (* alias for AstQueue.AstQueue *)
FROM Lexer IMPORT LexerT; (* alias for Lexer.Lexer *)
FROM LexQueue IMPORT LexQueueT; (* alias for LexQueue.LexQueue *)
FROM Symbol IMPORT SymbolT; (* alias for Symbol.Symbol *)
FROM Token IMPORT TokenT; (* alias for Token.Token *)
FROM TokenSet IMPORT TokenSetT; (* alias for TokenSet.TokenSet *)
FROM String IMPORT StringT; (* alias for String.String *)

FROM NonTerminals IMPORT FIRST, FOLLOW, inFIRST;


(* Parse Procedure Type *)

TYPE ParseProc = PROCEDURE ( VAR AstT ) : SymbolT;


(* Parser context *)

VAR
  lexer : LexerT;
  stats : Statistics;


(* Operations *)

PROCEDURE parseDef
  ( source : StringT; VAR stats : Statistics; VAR status : Status ) : AstT;
(* Parses .def source file, returns AST on success, NIL on failure. *)

BEGIN
  (* TO DO *)
END parseDef;


PROCEDURE parseMod
  ( source : StringT; VAR stats : Statistics; VAR status : Status ) : AstT;
(* Parses .mod source file, returns AST on success, NIL on failure. *)

BEGIN
  (* TO DO *)
END parseMod;


(* Private Operations *)

(* --------------------------------------------------------------------------
 * private function matchToken(expectedToken)
 * --------------------------------------------------------------------------
 * Matches the lookahead symbol to expectedToken and returns TRUE if they
 * match.  If they don't match, a syntax error is reported, the error count
 * is incremented and FALSE is returned.
 * --------------------------------------------------------------------------
 *)
PROCEDURE matchToken ( expectedToken : TokenT ) : BOOLEAN;

VAR
  lookahead : SymbolT;

BEGIN
  lookahead := Lexer.nextSym(lexer);
  
  IF expectedToken = lookahead.token THEN
    RETURN  TRUE
  ELSE (* no match *)
    (* report error *)
    EmitSyntaxErrorWToken(expectedToken, lookahead);
        
    (* print source line *)
    Source.PrintLineAndMarkColumn(source, lookahead.line, lookahead.col);
    
    (* update error count *)
    stats.syntaxErrors := stats.syntaxErrors + 1;
    
    RETURN FALSE
  END (* IF *)
END matchToken;


(* --------------------------------------------------------------------------
 * private function matchSet(expectedSet)
 * --------------------------------------------------------------------------
 * Matches the lookahead symbol to set expectedSet and returns TRUE if it
 * matches any of the tokens in the set.  If there is no match, a syntax
 * error is reported, the error count is incremented and FALSE is returned.
 * --------------------------------------------------------------------------
 *)
PROCEDURE matchSet ( expectedSet : TokenSetT ) : BOOLEAN;

VAR
  lookahead : SymbolT;

BEGIN
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* check if lookahead matches any token in expectedSet *)
  IF TokenSet.isElement(expectedSet, lookahead.token) THEN
    RETURN TRUE
  ELSE (* no match *)
    (* report error *)
    EmitSyntaxErrorWSet(expectedSet, lookahead);
    
    (* print source line *)
    Source.PrintLineAndMarkColumn(source, lookahead.line, lookahead.col);
        
    (* update error count *)
    stats.syntaxErrors := stats.syntaxErrors + 1;
    
    RETURN FALSE
  END (* IF *)
END matchSet;


(* --------------------------------------------------------------------------
 * private function matchTokenOrSet(expectedToken, expectedSet)
 * --------------------------------------------------------------------------
 * Matches the lookahead symbol to token expectedToken or set expectedSet and
 * returns TRUE if there is a match.  If there is no match, a syntax error is
 * reported, the error count is incremented and FALSE is returned.
 * --------------------------------------------------------------------------
 *)
PROCEDURE matchTokenOrSet
  ( expectedToken : TokenT; expectedSet : TokenSetT ) : BOOLEAN;

VAR
  lookahead : SymbolT;

BEGIN
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* check if lookahead matches any token in expected_set *)
  IF expectedToken = lookahead.token OR 
    TokenSet.isElement(expectedSet, lookahead.token) THEN
    RETURN TRUE
  ELSE (* no match *)
    (* report error *)
    EmitSyntaxErrorWTokenAndSet(expectedToken, expectedSet, lookahead);
    
    (* print source line *)
    Source.PrintLineAndMarkColumn(source, lookahead.line, lookahead.col);
        
    (* update error count *)
    stats.syntaxErrors := stats.syntaxErrors + 1;
    
    RETURN FALSE
  END (* IF *)
END matchTokenOrSet;


(* --------------------------------------------------------------------------
 * private function matchSetOrSet(expectedSet1, expectedSet2)
 * --------------------------------------------------------------------------
 * Matches the lookahead symbol to set expectedSet1 or set expectedSet2 and
 * returns TRUE if there is a match.  If there is no match, a syntax error
 * is reported, the error count is incremented and FALSE is returned.
 * --------------------------------------------------------------------------
 *)
PROCEDURE matchSetOrSet ( expectedSet1, expectedSet2 : TokenSetT ) : BOOLEAN;

VAR
  lookahead : SymbolT;

BEGIN
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* check if lookahead matches any token in expected_set *)
  IF TokenSet.isElement(expectedSet1, lookahead.token) OR 
    TokenSet.isElement(expectedSet2, lookahead.token) THEN
    RETURN TRUE
  ELSE (* no match *)
    (* report error *)
    EmitSyntaxErrorWSetAndSet(expectedSet1, expectedSet2, lookahead);
    
    (* print source line *)
    Source.PrintLineAndMarkColumn(source, lookahead.line, lookahead.col);
        
    (* update error count *)
    stats.syntaxErrors := stats.syntaxErrors + 1;
    
    RETURN FALSE
  END (* IF *)
END matchSetOrSet;


(* --------------------------------------------------------------------------
 * private function skipToMatchToken(resyncToken)
 * --------------------------------------------------------------------------
 * Consumes symbols until the lookahead symbol's token matches the given
 * token.  Returns the new lookahead symbol.
 * --------------------------------------------------------------------------
 *)
PROCEDURE skipToMatchToken ( resyncToken : TokenT ) : SymbolT;

VAR
  lookahead : SymbolT;

BEGIN
  lookahead := Lexer.lookaheadSym(lexer);

  (* skip symbols until lookahead token matches resync token *)
  WHILE lookahead.token # resyncToken DO
    lookahead = Lexer.consumeSym(lexer)
  END; (* WHILE *)
  
  RETURN lookahead
END skipToMatchToken;


(* --------------------------------------------------------------------------
 * private function skipToMatchTokenOrToken(token1, token2)
 * --------------------------------------------------------------------------
 * Consumes symbols until the lookahead symbol's token matches one of the
 * a given tokens.  Returns the new lookahead symbol.
 * --------------------------------------------------------------------------
 *)
PROCEDURE skipToMatchTokenOrToken ( token1, token2 : TokenT ) : SymbolT;

VAR
  lookahead : SymbolT;

BEGIN
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* skip symbols until lookahead token matches token1 or token2 *)
  WHILE (lookahead.token # token1) AND (lookahead.token # token2) DO
    lookahead = Lexer.consumeSym(lexer)
  END; (* WHILE *)
    
  RETURN lookahead
END skipToMatchTokenOrToken;


(* --------------------------------------------------------------------------
 * private function skipToMatchSet(resyncSet)
 * --------------------------------------------------------------------------
 * Consumes symbols until the lookahead symbol's token matches one of the
 * tokens in the given token set.  Returns the new lookahead symbol.
 * --------------------------------------------------------------------------
 *)
PROCEDURE skipToMatchSet ( resyncSet : TokenSetT ) : SymbolT;

VAR
  lookahead : SymbolT;
  
BEGIN
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* skip symbols until lookahead matches a token in resyncSet *)
  WHILE NOT TokenSet.isElement(resyncSet, lookahead.token) DO
    lookahead = Lexer.consumeSym(lexer)
  END; (* WHILE *)
  
  RETURN lookahead
END skipToMatchSet;


(* --------------------------------------------------------------------------
 * private function skipToMatchSetOrSet(tokenSet1, tokenSet2)
 * --------------------------------------------------------------------------
 * Consumes symbols until the lookahead symbol's token matches one of the
 * tokens in any of the given token sets.  Returns the new lookahead symbol.
 * --------------------------------------------------------------------------
 *)
PROCEDURE skipToMatchSetOrSet ( tokenSet1, tokenSet2 : TokenSetT ) : SymbolT;

VAR
  lookahead : SymbolT;
  
BEGIN
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* skip symbols until lookahead matches a token in either token set *)
  WHILE NOT TokenSet.isElement(tokenSet1, lookahead.token) AND
    NOT TokenSet.isElement(tokenSet2, lookahead.token) DO
    lookahead = Lexer.consumeSym(lexer)
  END; (* WHILE *)
  
  RETURN lookahead
END skipToMatchSetOrSet;


(* --------------------------------------------------------------------------
 * private function skipToMatchTokenOrSet(resyncToken, resyncSet)
 * --------------------------------------------------------------------------
 * Consumes symbols until the lookahead symbol's token matches the given
 * token or one of the tokens in the given token set.  Returns the new
 * lookahead symbol.
 * --------------------------------------------------------------------------
 *)
PROCEDURE skipToMatchTokenOrSet
  ( resyncToken : TokenT; resyncSet : TokenSetT ) : SymbolT;

VAR
  lookahead : SymbolT;
  
BEGIN
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* skip symbols until lookahead matches resyncToken or resyncSet *)
  WHILE (lookahead.token # resyncToken) AND
    NOT TokenSet.isElement(resyncSet, lookahead.token) DO
    lookahead = Lexer.consumeSym(lexer)
  END; (* WHILE *)
  
  RETURN lookahead
END skipToMatchTokenOrSet;


(* --------------------------------------------------------------------------
 * private function skipToMatchTokenOrTokenOrSet(token1, token2, resyncSet)
 * --------------------------------------------------------------------------
 * Consumes symbols until the lookahead symbol's token matches one of the
 * given tokens or a token in the given token set.  Returns the new
 * lookahead symbol.
 * --------------------------------------------------------------------------
 *)
PROCEDURE skipToMatchTokenOrTokenOrSet
  ( token1, token2 : TokenT; tokenSet : TokenSetT ) : SymbolT;

VAR
  lookahead : SymbolT;
  
BEGIN
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* skip symbols until lookahead token matches token1, token2 or tokenSet *)
  WHILE (lookahead.token # token1) AND (lookahead.token # token2) AND
    NOT TokenSet.isElement(tokenSet, lookahead.token) DO
    lookahead = Lexer.consumeSym(lexer)
  END; (* WHILE *)
  
  RETURN lookahead
END skipToMatchTokenOrTokenOrSet;



(* ************************************************************************ *
 * Syntax Analysis                                                          *
 * ************************************************************************ *)

(* --------------------------------------------------------------------------
 * Generic Parsing Functions
 * ------------------------------------------------------------------------ *)

(* --------------------------------------------------------------------------
 * private function parseListWSeparator(p, sep, first, follow, nodeType, ast)
 * --------------------------------------------------------------------------
 * Parses a generic list rule, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * genericListWSeparator :=
 *   <production> ( <separator> <production> )*
 *   ;
 *
 * where <production> and <separator> are passed in as parameters.
 *
 * astNode: (<nodeType> <production-1> <production-2> ... <production-N>)
 *
 * Usage:
 *
 * To parse an expression list with the syntax
 *
 *   expression ( ',' expression )*
 *
 * call the generic list rule as follows
 *
 *  lookahead :=
 *    parseListWSeparator(expression, Token.Comma,
 *      FIRST(Expression), FOLLOW(Expression),
 *      AstNodeType.ExprList, astNode);
 * --------------------------------------------------------------------------
 *)
(* production ( separator production )* *)
PROCEDURE parseListWSeparator
  ( production  : ParseProc;
    separator   : TokenT;
    firstSet,
    followSet   : TokenSetT;
    nodeType    : AstNodeType;
    VAR astNode : AstT ) : SymbolT;

VAR
  node : AstT;
  tmplist : AstQueueT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("parseListWSeparator");
  
  (* production *)
  lookahead := production(node);
  AstQueue.Enqueue(tmplist, node);
  
  (* ( separator production )* *)
  WHILE lookahead.token = separator DO
    (* separator *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* production *)
    IF matchSet(firstSet) THEN
      lookahead := production(node);
      AstQueue.Enqueue(tmplist, node)
    
    ELSE (* resync *)
      lookahead := matchTokenOrSet(separator, followSet)
    END (* IF *)
  END; (* WHILE *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewListNode(nodeType, tmplist);
  
  RETURN lookahead
END parseListWSeparator;


(* --------------------------------------------------------------------------
 * private function parseListWTerminator(p, t, first, follow, nodeType, ast)
 * --------------------------------------------------------------------------
 * Parses a generic list rule, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * genericlistWTerminator :=
 *   ( <production> <terminator> )+
 *   ;
 *
 * where <production> and <terminator> are passed in as parameters.
 *
 * astNode: (<nodeType> <production-1> <production-2> ... <production-N>)
 *
 * Usage:
 *
 * To parse a definition list with the syntax
 *
 *   ( constDefinition ';' )+
 *
 * call the generic list rule as follows
 *
 *  lookahead :=
 *    parseListWTerminator(constDefinition, Token.Semicolon,
 *      FIRST(ConstDefinition), FOLLOW(ConstDefinition),
 *      AstNodeType.DefList, astNode);
 * --------------------------------------------------------------------------
 *)
PROCEDURE parseListWTerminator
  ( production  : ParseProc;
    terminator  : TokenT;
    firstSet,
    followSet   : TokenSetT;
    nodeType    : AstNodeType;
    VAR astNode : AstT ) : SymbolT;

VAR
  node : AstT;
  tmplist : AstQueueT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("parseListWTerminator");
  
  (* production *)
  lookahead := production(node);
  AstQueue.Enqueue(tmplist, node);
  
  (* ( production terminator )+ *)
  REPEAT
    (* production *)
    IF matchSet(firstSet) THEN
      lookahead := production(node);
      AstQueue.Enqueue(tmplist, node)
    ELSE (* resync *)
      lookahead := skipToMatchTokenOrSet(terminator, firstSet)
    END;
      
    (* terminator *)
    IF matchToken(terminator) THEN
      lookahead := Lexer.consumeSym(lexer)
    ELSE (* resync *)
      lookahead := matchSet(followSet)
    END (* IF *)
  UNTIL NOT inFIRST(firstSet, lookahead.token);
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewListNode(nodeType, tmplist);
  
  RETURN lookahead
END parseList;


(* --------------------------------------------------------------------------
 * Specific Parsing Functions
 * ------------------------------------------------------------------------ *)

(* --------------------------------------------------------------------------
 * private function compilationUnit()
 * --------------------------------------------------------------------------
 * Parses rule compilationUnit, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * compilationUnit :=
 *   definitionModule | implOrPrgmModule
 *   ;
 *
 * astnode: defModuleNode | impModuleNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE compilationUnit ( VAR astNode : AstT ) : SymbolT;

VAR
  lookahead : SymbolT;

BEGIN
  PARSER_DEBUG_INFO("compilationUnit");
  
  lookahead := Lexer.lookaheadSym(lexer);
  
  CASE lookahead.token OF
    Token.Definition : lookahead := definitionModule(astNode)
      
  | Token.Implementation,
    Token.Module : lookahead := implOrPrgmModule(astNode)
  END; (* CASE *)
  
  RETURN lookahead
END compilationUnit;


(* --------------------------------------------------------------------------
 * private function definitionModule(astNode)
 * --------------------------------------------------------------------------
 * Parses rule definitionModule, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * definitionModule :=
 *   DEFINITION MODULE moduleIdent ';'
 *   import* definition* END moduleIdent '.'
 *   ;
 *
 * moduleIdent := Ident ;
 *
 * astNode: (DEFMOD moduleIdent implist deflist)
 * --------------------------------------------------------------------------
 *)
PROCEDURE definitionModule ( VAR astNode : AstT ) : SymbolT;

VAR
  moduleIdent, implist, deflist : AstT;
  ident1, ident2 : StringT;
  tmplist : AstQueueT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("definitionModule");
  
  (* DEFINITION *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* MODULE *)
  IF matchToken(Token.Module) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrToken(Token.StdIdent, Token.Semicolon)
  END; (* IF *)

  (* moduleIdent *)
  IF matchToken(Token.StdIdent) THEN
    ident1 = lookahead.lexeme;
    lookahead := Lexer.consumeSym(lexer)
    
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrSet(Token.Semicolon, FIRST(Import))
  END; (* IF *)
  
  (* ';' *)
  IF matchToken(Token.Semicolon) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSetOrSet(First(Import), FOLLOW(Import))
  END; (* IF *)
  
  AstQueue.New(templist);

  (* import* *)
  WHILE lookahead.token = Token.Import DO
    lookahead := import(implist);
    AstQueue.Enqueue(tmplist, implist)
  END (* WHILE *)
  
  implist := AST.NewListNode(AstNodeType.ImpList, tmplist);
  tmplist:= AstQueue.ResetQueue(tmplist);
  
  (* definition* *)
  WHILE inFIRST(Definition) DO
    lookahead := definition(deflist);
    AstQueue.Enqueue(tmplist, deflist)
  END (* WHILE *)
  
  deflist := AST.NewListNode(AstNodeType.DefList, tmplist);
  tmplist := AstQueue.ResetQueue(tmplist);
  
  (* END *)
  IF matchToken(Token.End) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrTokenOrSet
      (Token.StdIdent, Token.Dot, FOLLOW(DefinitionModule))
  END; (* IF *)
  
  (* moduleIdent *)
  IF matchToken(Token.StdIdent) THEN
    ident2 := lookahead.lexeme;
    lookahead := Lexer.ConsumeSym(lexer);
    
    IF ident1 # ident2 THEN
      (* TO DO: report error -- module identifiers don't match *) 
    END (* IF *)
    
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrSet(Token.Dot, FOLLOW(DefinitionModule))
  END; (* IF *)
  
  (* '.' *)
  IF matchToken(Token.Period, FOLLOW(DefinitionModule)) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(DefinitionModule))
  END (* IF *)
  
  (* build AST node and pass it back in astNode *)
  moduleIdent := AST.NewTerminalNode(AstNodeType.Ident, ident1);
  astNode := AST.NewNode(AstNodeType.DefMod, moduleIdent, implist, deflist);
  
  RETURN lookahead
END definitionModule;


(* --------------------------------------------------------------------------
 * private function import(astNode)
 * --------------------------------------------------------------------------
 * Parses rule import, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * import :=
 *   IMPORT libIdent ( ',' libIdent )* ';'
 *   ;
 *
 * alias libIdent := StdIdent ;
 *
 * astNode: (IMPORT implist)
 * --------------------------------------------------------------------------
 *)
PROCEDURE import ( VAR astNode : AstT ) : SymbolT;

VAR
  idlist : AstT;
  tmplist : LexQueueT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("import");

  (* IMPORT *)
  lookahead := Lexer.consumeSym(lexer);
  
  LexQueue.New(tmplist);
  
  (* libIdent *)
  IF matchToken(Token.StdIdent) THEN
    LexQueue.Enqueue(tmplist, lookahead.lexeme);
    lookahead := Lexer.consumeSym(lexer)
    
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrSet(Token.Comma, FOLLOW(Import))
  END; (* IF *)
  
  (* ( ',' libIdent )* *)
  WHILE lookahead.token = Token.Comma DO
    lookahead := Lexer.consumeSym(lexer);
    
    (* libIdent *)
    IF matchToken(Token.StdIdent) THEN
      LexQueue.Enqueue(tmplist, lookahead.lexeme);
      lookahead := Lexer.ConsumeSym(lexer)
      
    ELSE (* resync *)
      lookahead := skipToMatchTokenOrSet(Token.Comma, FOLLOW(Import))
    END (* IF *)
  END; (* WHILE *)
    
  (* build AST node and pass it back in astNode *)
  idlist := NewTerminalListNode(AstNodeType.IdentList, tmplist);
  astNode := AST.NewListNode(AstNodeType.Import, idlist);
  LexQueue.Release(tmplist);
  
  RETURN lookahead
END import;


(* --------------------------------------------------------------------------
 * private function ident(astNode)
 * --------------------------------------------------------------------------
 * Parses rule ident, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * ident :=
 *   StdIdent | ForeignIdent
 *   ;
 *
 * astNode: (IDENT "lexeme")
 * --------------------------------------------------------------------------
 *)
PROCEDURE ident ( VAR astNode : AstT ) : SymbolT;

VAR
  lexeme : LexemeT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("ident");
  
  lookahead := Lexer.lookaheadSym(lexer);
  
  lexeme := lookahead.lexeme;

  (* StdIdent | ForeignIdent *)
  lookahead := Lexer.consumeSym(lexer)
    
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewTerminalNode(AstNodeType.Ident, lexeme);
  
  RETURN lookahead
END ident;


(* --------------------------------------------------------------------------
 * private function qualident(astNode)
 * --------------------------------------------------------------------------
 * Parses rule qualident, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * qualident :=
 *   ident ( '.' ident )*
 *   ;
 *
 * astNode: (QUALIDENT "lexeme1" "lexeme2" ... "lexemeN" )
 * --------------------------------------------------------------------------
 *)
PROCEDURE qualident ( VAR astNode : AstT ) : SymbolT;

VAR
  tmplist : LexQueue;
  lookahead := SymbolT;

BEGIN
  PARSER_DEBUG_INFO("qualident");
  
  lookahead := Lexer.lookaheadSym(lexer);
  
  LexQueue.New(tmplist);
  LexQueue.Enqueue(tmplist, lookahead.lexeme);
  
  (* ident *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* ( '.' ident )* *)
  WHILE lookahead.token = Token.Dot DO
    lookahead := Lexer.consumeSym(lexer);
    
    (* ident *)
    IF matchSet(FIRST(Ident)) THEN
      LexQueue.Enqueue(tmplist, lookahead.lexeme);
      lookahead := Lexer.consumeSym(lexer)
      
    ELSE (* resync *)
      lookahead := skipToMatchTokenOrSet(Token.Dot, FOLLOW(Qualident))
    END (* IF *)
  END; (* WHILE *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewTerminalListNode(AstNodeType.Qualident, tmplist);
  LexQueue.Release(tmplist);
  
  RETURN lookahead
END qualident;


(* --------------------------------------------------------------------------
 * private function identList(astNode)
 * --------------------------------------------------------------------------
 * Parses rule identList, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * identList :=
 *   ident ( ',' ident )*
 *   ;
 *
 * astNode: (IDENTLIST "lexeme1" "lexeme2" ... "lexemeN" )
 * --------------------------------------------------------------------------
 *)
PROCEDURE identList ( VAR astNode : AstT ) : SymbolT;

VAR
  tmplist : LexQueue;
  lookahead := SymbolT;

BEGIN
  PARSER_DEBUG_INFO("identList");

  lookahead := Lexer.lookaheadSym(lexer);
  
  LexQueue.New(tmplist);
  LexQueue.Enqueue(tmplist, lookahead.lexeme);
  
  (* ident *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* ( ',' ident )* *)
  WHILE lookahead.token = Token.Comma DO
    lookahead := Lexer.consumeSym(lexer);
    
    (* ident *)
    IF matchSet(FIRST(Ident)) THEN
      LexQueue.EnqueueUnique(tmplist, lookahead.lexeme);
      lookahead := Lexer.consumeSym(lexer)
      
    ELSE (* resync *)
      lookahead := skipToMatchTokenOrSet(Token.Comma, FOLLOW(IdentList))
    END (* IF *)
  END; (* WHILE *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewTerminalListNode(AstNodeType.IdentList, tmplist);
  LexQueue.Release(tmplist);
  
  RETURN lookahead
END identList;


(* --------------------------------------------------------------------------
 * private function definition(astNode)
 * --------------------------------------------------------------------------
 * Parses rule definition, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * definition :=
 *   CONST ( constDefinition ';' )+ |
 *   TYPE ( typeDefinition ';' )+ |
 *   VAR ( identList ':' typeIdent ';' )+ |
 *   procedureHeader ';' |
 *   toDoList ';'
 *   ;
 *
 * astNode: constDefNode | typeDefNode | varDeclNode | procDefNode | toDoNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE definition ( VAR astNode : AstT ) : SymbolT;

VAR
  lookahead := SymbolT;

BEGIN
  PARSER_DEBUG_INFO("definition");

  lookahead := Lexer.lookaheadSym(lexer);
  
  CASE lookahead.token OF
  (* CONST *)
    Token.Const :
      lookahead := Lexer.consumeSym(lexer);
      
      (* ( constDefinition ';' )+ *)
      lookahead :=
        parseListWTerminator(constDefinition, Token.Semicolon,
          FIRST(ConstDefinition), FOLLOW(ConstDefinition),
          AstNodeType.DefList, astNode)
            
  (* TYPE *)
  | Token.Type :
      lookahead := Lexer.consumeSym(lexer);
      
      (* ( typeDefinition ';' )+ *)
      lookahead :=
        parseListWTerminator(typeDefinition, Token.Semicolon,
          FIRST(TypeDefinition), FOLLOW(TypeDefinition),
          AstNodeType.DefList, astNode)
  
  (* VAR *)
  | Token.Var :
      lookahead := Lexer.consumeSym(lexer);
      
      (* ( varDefinition ';' )+ *)
      lookahead :=
        parseListWTerminator(varDefinition, Token.Semicolon,
          FIRST(VarDefinition), FOLLOW(VarDefinition),
          AstNodeType.DefList, astNode)
  
  (* PROCEDURE *)
  | Token.Procedure :
      (* procedureHeader *)
      lookahead := procedureHeader(astNode);
      
      (* ';' *)
      IF matchToken(Token.Semicolon) THEN
        lookahead := Lexer.consumeSym(lexer)
      ELSE (* resync *)
        lookahead := skipToMatchSet(FOLLOW(Definition))
      END (* IF *)
      
  (* TO *)
  | Token.To :
      (* toDoList *)
      lookahead := toDoList(astNode);
      
      (* ';' *)
      IF matchToken(Token.Semicolon) THEN
        lookahead := Lexer.consumeSym(lexer)
      ELSE (* resync *)
        lookahead := skipToMatchSet(FOLLOW(Definition))
      END (* IF *)
  END; (* CASE *)
    
  RETURN lookahead
END definition;


(* --------------------------------------------------------------------------
 * private function constDefinition(astNode)
 * --------------------------------------------------------------------------
 * Parses rule constDefinition, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * constDefinition :=
 *   ident '=' constExpression
 *   ;
 *
 * alias constExpression = expression ;
 *
 * astNode: (CONSTDEF constId expr)
 * --------------------------------------------------------------------------
 *)
PROCEDURE constDefinition ( VAR astNode : AstT ) : SymbolT;

VAR
  constId, expr : AstT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("constDefinition");
  
  (* ident *)
  lookahead := ident(constId);
  
  (* '=' *)
  IF matchToken(Token.Equal) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead :=
      skipToMatchSetOrSet(FIRST(Expression), FOLLOW(ConstDefinition)))
  END; (* IF *)
  
  (* constExpression *)
  IF matchSet(FIRST(Expression)) THEN
    lookahead := expression(expr)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(ConstDefinition))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.ConstDef, constId, expr);
  
  RETURN lookahead
END constDefinition;


(* --------------------------------------------------------------------------
 * private function typeDefinition(astNode)
 * --------------------------------------------------------------------------
 * Parses rule typeDefinition, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * typeDefinition :=
 *   ident '=' typeDefinitionTail
 *   ;
 *
 * typeDefinitionTail :=
 *   ( OPAQUE | type )
 *   ;
 *
 * astNode: (TYPEDEF identNode typeNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE typeDefinition ( VAR astNode : AstT ) : SymbolT;

VAR
  typeId, typeDef : AstT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("typeDefinition");
  
  (* ident *)
  lookahead := ident(typeId);
  
  (* '=' *)
  IF matchToken(Token.Equal) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrSet(Token.Opaque, FIRST(type))
  END; (* IF *)
  
  (* OPAQUE | type *)
  IF matchTokenOrSet(Token.Opaque ,FIRST(Type)) THEN
    (* OPAQUE | *)
    IF lookahead.token = Token.Opaque THEN
      lookahead := Lexer.consumeSym(lexer)
      typeDef := AST.emptyNode()
      
    (* type *)
    ELSE
      lookahead := type(typeDef)
    END (* IF *)
    
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(TypeDefinition))
  END; (* IF *)
    
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.TypeDef, typeId, typeDef);
  
  RETURN lookahead
END typeDefinition;


(* --------------------------------------------------------------------------
 * private function type(astNode)
 * --------------------------------------------------------------------------
 * Parses rule type, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * type :=
 *   aliasType | derivedType | subrangeType | enumType | setType |
 *   arrayType | recordType | pointerType | procedureType
 *   ;
 *
 * alias derivedType = typeIdent ;
 *
 * alias typeIdent = qualident ;
 *
 * astNode: typeNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE type ( VAR astNode : AstT ) : SymbolT;

VAR
  lookahead := SymbolT;

BEGIN
  PARSER_DEBUG_INFO("type");
  
  lookahead := Lexer.lookaheadSym(lexer);
  
  CASE lookahead.token OF
  (* aliasType | *)
    Token.Alias : lookahead := aliasType(astNode)
      
  (* derivedType | *)
  | Token.StdIdent,
    Token.ForeignIdent : lookahead := qualident(astNode)
    
  (* subrangeType | *)
  | Token.LeftBracket : lookahead := subrangeType(astNode)
  
  (* enumType | *)
  | Token.LeftParen : lookahead := enumType(astNode)
  
  (* setType | *)
  | Token.Set : lookahead := setType(astNode)
  
  (* arrayType | *)
  | Token.Array : lookahead := arrayType(astNode)
  
  (* recordType | *)
  | Token.Record : lookahead := recordType(astNode)
  
  (* pointerType | *)
  | Token.Pointer : lookahead := pointerType(astNode)
  
  (* procedureType *)
  | Token.Procedure : lookahead := procedureType(astNode)
  END; (* CASE *)
  
  RETURN lookahead
END type;


(* --------------------------------------------------------------------------
 * private function aliasType(astNode)
 * --------------------------------------------------------------------------
 * Parses rule aliasType, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * aliasType :=
 *   ALIAS OF typeIdent
 *   ;
 *
 * alias typeIdent = qualident ;
 *
 * astNode: (ALIAS baseType)
 * --------------------------------------------------------------------------
 *)
PROCEDURE aliasType ( VAR astNode : AstT ) : SymbolT;

VAR
  baseType : AstT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("aliasType");
  
  (* ALIAS *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* OF *)
  IF matchToken(Token.Of) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FIRST(Qualident))
  END; (* IF *)
  
  (* typeIdent *)
  IF matchSet(FIRST(Qualident)) THEN
    lookahead := qualident(baseType)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(AliasType))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.AliasType, baseType);
  
  RETURN lookahead
END aliasType;


(* --------------------------------------------------------------------------
 * private function rangeOfOrdinalType(astNode)
 * --------------------------------------------------------------------------
 * Parses rule rangeOfOrdinalType, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * rangeOfOrdinalType :=
 *   '[' lowerBound '..' upperBound ']' OF ordinalType
 *   ;
 *
 * alias lowerBound = ordinalExpression ;
 *
 * alias upperBound = ordinalExpression ;
 *
 * alias ordinalExpression = expression ;
 *
 * alias ordinalType = typeIdent ;
 *
 * astNode: (SUBR lowerBound upperBound typeId)
 * --------------------------------------------------------------------------
 *)
PROCEDURE rangeOfOrdinalType ( VAR astNode : AstT ) : SymbolT;

VAR
  lowerBound, upperBound, typeId : AstT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("rangeOfOrdinalType");
  
  (* '[' *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* lowerBound *)
  IF matchSet(FIRST(Expression)) THEN
    lookahead := expression(lowerBound)
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrSet(Token.DotDot, FIRST(Expression))
  END; (* IF *)
  
  (* '..' *)
  IF matchToken(Token.DotDot) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FIRST(Expression))
  END; (* IF *)
  
  (* upperBound *)
  IF matchSet(FIRST(Expression)) THEN
    lookahead := expression(upperBound)
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrToken(Token.RightBracket, Token.Of)
  END; (* IF *)
  
  (* ']' *)
  IF matchToken(Token.RightBracket) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrSet(Token.Of, FIRST(Qualident))
  END; (* IF *)
  
  (* OF *)
  IF matchToken(Token.Of) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead :=
      skipToMatchSetOrSet(FIRST(Qualident), FOLLOW(rangeOfOrdinalType))
  END; (* IF *)
    
  (* ordinalType *)
  IF matchSet(FIRST(Qualident)) THEN
    lookahead := qualident(typeId)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(rangeOfOrdinalType))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.Subr, lowerBound, upperBound, typeId);
  
  RETURN lookahead
END rangeOfOrdinalType;



(* --------------------------------------------------------------------------
 * private function subrangeType(astNode)
 * --------------------------------------------------------------------------
 * Parses rule subrangeType, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * alias subrangeType = rangeOfOrdinalType;
 *
 * astNode: (SUBR lowerBound upperBound baseType)
 * --------------------------------------------------------------------------
 *)
PROCEDURE subrangeType ( VAR astNode : AstT ) : SymbolT;

BEGIN
  PARSER_DEBUG_INFO("subrangeType");
  
  RETURN rangeOfOrdinalType(astNode)
END subrangeType;


(* --------------------------------------------------------------------------
 * private function enumType(astNode)
 * --------------------------------------------------------------------------
 * Parses rule enumType, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * enumType :=
 *   '(' ( '+' enumTypeToExtend ',' )? identList ')'
 *   ;
 *
 * alias enumTypeToExtend = enumTypeIdent ;
 *
 * alias enumTypeIdent = typeIdent ;
 *
 * astNode: (ENUM baseType valueList)
 * --------------------------------------------------------------------------
 *)
PROCEDURE enumType ( VAR astNode : AstT ) : SymbolT;

VAR
  baseType, valueList : AstT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("enumType");
  
  (* '(' *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* ( '+' enumTypeToExtend ',' )? *)
  IF lookahead.token = Token.Plus THEN
    (* '+' *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* enumTypeToExtend *)
    IF matchSet(FIRST(Qualident)) THEN
      lookahead := qualident(baseType);
      
      (* ',' *)
      IF matchToken() THEN
        lookahead := Lexer.consumeSym(lexer)
      ELSE (* resync *)
        lookahead := skipToMatchSet(FIRST(IdentList))
      END (* IF *)
      
    ELSE (* resync *)
      lookahead := skipToMatchSetOrSet(FIRST(IdentList), FOLLOW(EnumType))
    END (* IF *)
  END; (* IF *)
  
  (* identList *)
  IF matchSet(FIRST(IdentList)) THEN
    lookahead := identList(valueList);
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrSet(Token.RightParen, FOLLOW(EnumType))
  END; (* IF *)
  
  (* ')' *)
  IF matchToken(Token.RightParen) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(EnumType))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.EnumType, baseType, valueList);
  
  RETURN lookahead
END enumType;


(* --------------------------------------------------------------------------
 * private function setType(astNode)
 * --------------------------------------------------------------------------
 * Parses rule setType, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * setType :=
 *   SET OF enumTypeIdent
 *   ;
 *
 * alias enumTypeIdent = typeIdent ;
 *
 * astNode: (SET elemType)
 * --------------------------------------------------------------------------
 *)
PROCEDURE setType ( VAR astNode : AstT ) : SymbolT;

VAR
  elemType : AstT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("setType");
  
  (* SET *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* OF *)
  IF matchToken(Token.Of) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSetOrSet(FIRST(Qualident), FOLLOW(SetType))
  END; (* IF *)
  
  (* enumTypeIdent *)
  IF matchSet(FIRST(Qualident)) THEN
    lookahead := qualident(elemType)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(SetType))
  END; (* IF *)
    
  (* build AST node and pass it back in ast *)
  astNode := AST.NewNode(AstNodeType.SetType, elemType);
  
  RETURN lookahead
END setType;


(* --------------------------------------------------------------------------
 * private function arrayType(astNode)
 * --------------------------------------------------------------------------
 * Parses rule arrayType, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * arrayType :=
 *   ARRAY valueCount OF typeIdent
 *   ;
 *
 * alias valueCount = constExpression ;
 *
 * astNode: (ARRAY valueCount baseType)
 * --------------------------------------------------------------------------
 *)
PROCEDURE arrayType ( VAR astNode : AstT ) : SymbolT;

VAR
  valueCount, baseType : AstT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("arrayType");
  
  (* ARRAY *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* valueCount *)
  IF matchSet(FIRST(Expression)) THEN
    (* alias valueCount = constExpression *)
    lookahead := expression(valueCount);
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrSet(Token.Of, FIRST(TypeIdent))
  END; (* IF *)
  
  (* OF *)
  IF matchToken(Token.Of) THEN
    lookahead := Lexer.consumeSym(lexer);
  ELSE (* resync *)
    lookahead := skipToMatchSetOrSet(FIRST(Qualident), FOLLOW(ArrayType))
  END (* IF *)
  
  (* typeIdent *)
  IF matchSet(FIRST(Qualident)) THEN
    lookahead := qualident(baseType)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(ArrayType))
  END (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.ArrayType, valueCount, baseType);
  
  RETURN lookahead
END arrayType;


(* --------------------------------------------------------------------------
 * private function recordType(astNode)
 * --------------------------------------------------------------------------
 * Parses rule recordType, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * recordType :=
 *   RECORD ( '(' recTypeToExtend ')' )?
 *   fieldList ( ';' fieldList )* END
 *   ;
 *
 * recTypeToExtend = typeIdent | NIL ;
 *
 * alias fieldList = varOrFieldDeclaration ;
 *
 * astNode: (RECORD baseType fieldListSeq)
 * --------------------------------------------------------------------------
 *)
PROCEDURE recordType ( VAR astNode : AstT ) : SymbolT;

VAR
  baseType, fieldListSeq : AstT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("recordType");
  
  (* RECORD *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* ( '(' recTypeToExtend ')' )? *)
  IF lookahead.token = Token.LeftParen THEN
    (* '(' *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* typeIdent | NIL *)
    IF matchSet(FIRST(RecTypeToExtend)) THEN
      lookahead := qualident(baseType)
    ELSE (* resync *)
      lookahead := skipToMatchTokenOrSet(Token.RightParen, FIRST(FieldList))
    END (* IF *)
    
    (* ')' *)
    IF matchToken(Token.RightParen) THEN
      lookahead := Lexer.consumeSym(lexer)
    ELSE (* resync *)
      lookahead := skipToMatchSet(FIRST(FieldList))
    END (* IF *)
    
  ELSE (* non-extensible record *)
    baseType := AST.emptyNode()
  END; (* IF *)
  
  (* fieldList ( ';' fieldList )* *)
  lookahead :=
    parseListWSeparator(fieldList, Token.Semicolon,
      FIRST(FieldList), FOLLOW(FieldList),
      AstNodeType.FieldListSeq, fieldListSeq);
  
  (* END *)
  IF matchToken(Token.End) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(RecordType))
  END; (* IF *)
    
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.RecordType, baseType, fieldListSeq);
  
  RETURN lookahead
END recordType;


(* --------------------------------------------------------------------------
 * private function pointerType(astNode)
 * --------------------------------------------------------------------------
 * Parses rule pointerType, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * pointerType :=
 *   POINTER TO typeIdent
 *   ;
 *
 * astNode: (POINTER qualidentNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE pointerType ( VAR astNode : AstT ) : SymbolT;

VAR
  baseType : AstT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("pointerType");
  
  (* POINTER *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* TO *)
  IF matchToken(Token.To) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSetOrSet(FIRST(Qualident), FOLLOW(PointerType))
  END; (* IF *)
  
  (* typeIdent *)
  IF matchSet(FIRST(Qualident)) THEN
    lookahead := qualident(baseType)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(PointerType))
  END; (* IF *)
    
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.PointerType, baseType);
  
  RETURN lookahead
END pointerType;


(* --------------------------------------------------------------------------
 * private function procedureType(astNode)
 * --------------------------------------------------------------------------
 * Parses rule procedureType, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * procedureType :=
 *   PROCEDURE
 *   ( '(' formalType ( ',' formalType )* ')' )? ( ':' returnedType )?
 *   ;
 *
 * alias returnedType = typeIdent ;
 *
 * astNode: (PROCTYPE formalTypeListNode qualidentNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE procedureType ( VAR astNode : AstT ) : SymbolT;

VAR
  formalTypeList, retType : AstT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("procedureType");
    
  (* PROCEDURE *)
  lookahead := Lexer.consumeSym(lexer);  
  
  (* ( '(' formalType ( ',' formalType )* ')' )? *)
  IF lookahead.token = Token.LeftParen THEN
    (* '(' *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* formalType ( ',' formalType )* *)
    lookahead :=
      parseListWSeparator(formalType, Token.Comma,
        FIRST(FormalType), FOLLOW(FormalType),
        AstNodeType.FormalTypeList, formalTypeList);
          
    (* ')' *)
    IF matchToken(Token.RightParen) THEN
      lookahead := Lexer.consumeSym(lexer)
    ELSE (* resync *)
      lookahead := skipToMatchTokenOrSet(Token.Colon, FOLLOW(ProcedureType))
    END (* IF *)
  END; (* IF *)
  
  (* ( ':' returnedType )? *)
  IF lookahead.token = Token.Colon THEN
    (* ':' *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* returnedType *)
    IF matchSet(FIRST(Qualident)) THEN
      lookahead := qualident(retType)
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(procedureType))
    END (* IF *)
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.ProcType, formalTypeList, retType);
  
  RETURN lookahead
END procedureType;


(* --------------------------------------------------------------------------
 * private function formalType(astNode)
 * --------------------------------------------------------------------------
 * Parses rule formalType, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * formalType :=
 *   nonAttrFormalType | attributedFormalType
 *   ;
 *
 * astNode: formalTypeNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE formalType ( VAR astNode : AstT ) : SymbolT;

VAR
  lookahead := SymbolT;

BEGIN
  PARSER_DEBUG_INFO("formalType");
  
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* nonAttrFormalType | *)
  IF inFIRST(NonAttrFormalType, lookahead.token) THEN
    lookahead := nonAttrFormalType(astNode)
  ELSE (* attributedFormalType *)
    lookahead := attributedFormalType(astNode)
  END; (* IF *) 
  
  RETURN lookahead
END formalType;


(* --------------------------------------------------------------------------
 * private function nonAttrFormalType(astNode)
 * --------------------------------------------------------------------------
 * Parses rule nonAttrFormalType, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * nonAttrFormalType :=
 *   ( ARRAY OF )? typeIdent | castingFormalType
 *   ;
 *
 * astNode:
 *  (FTYPE qualidentNode) |
 *  (ARRAYP identNode qualidentNode) |
 *  (OPENARRAYP qualidentNode) |
 *  castingFormalTypeNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE nonAttrFormalType ( VAR astNode : AstT ) : SymbolT;

VAR
  ftypeId, ftype : AstT;
  seenArray : BOOLEAN;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("procedureType");
  
  seenArray := FALSE;
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* castingFormalType | *)
  IF lookahead.token = Token.Cast THEN
    lookahead := castingFormalType(astNode)
  
  ELSE (* ( ARRAY OF )? typeIdent *)
    (* ( ARRAY OF )? *)
    IF lookahead.token = Token.Array THEN
      (* ARRAY *)
      lookahead := Lexer.consumeSym(lexer);
      seenArray := TRUE;
      
      (* OF *)
      IF matchToken(Token.Of) THEN
        lookahead := Lexer.consumeSym(lexer);
      ELSE (* resync *)
        lookahead :=
          skipToMatchSetOrSet(FIRST(Qualident), FOLLOW(NonAttrFormalType))
      END (* IF *)
    END; (* IF *)
    
    (* typeIdent *)
    IF matchSet(FIRST(Qualident)) THEN
      lookahead := qualident(ftypeId);
      
      (* build AST node and pass it back in astNode *)
      ftype := AST.NewNode(AstNodeType.FormalType, ftypeId);
      IF seenArray THEN
        astNode := AST.NewNode(AstNodeType.OpenArray, ftype)
      ELSE
        astNode := ftype
      END (* IF *)
      
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(NonAttrFormalType))
    END (* IF *)
  END; (* IF *)
  
  RETURN lookahead
END nonAttrFormalType;


(* --------------------------------------------------------------------------
 * private function castingFormalType(astNode)
 * --------------------------------------------------------------------------
 * Parses rule castingFormalType, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * castingFormalType :=
 *   CAST ( BARE ARRAY OF OCTET | addressTypeIdent )
 *   ;
 *
 * astNode: (CASTP formalTypeNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE castingFormalType ( VAR astNode : AstT ) : SymbolT;

VAR
  lexeme : LexemeT;
  ftypeId, ftype : AstT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("castingFormalType");
  
  (* CAST *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* BARE ARRAY OF OCTET | addressTypeIdent *)
  IF matchTokenOrSet(Token.Bare, FIRST(AddressType)) THEN
    (* BARE ARRAY OF OCTET | *)
    IF lookahead.token = Token.Bare THEN
      (* BARE *)
      lookahead := Lexer.consumeSym(lexer);
      
      (* ARRAY *)
      IF matchToken(Token.Array) THEN
        lookahead := Lexer.consumeSym(lexer)
      ELSE (* resync *)
        lookahead := skipToMatchTokenOrToken(Token.Of, Token.Octet)
      END; (* IF *)
      
      (* OF *)
      IF matchToken(Token.Of) THEN
        lookahead := Lexer.consumeSym(lexer)
      ELSE (* resync *)
        lookahead :=
          skipToMatchTokenOrSet(Token.Octet, FOLLOW(CastingFormalType))
      END; (* IF *)
      
      (* OCTET *)
      IF matchToken(Token.Octet) THEN
        lexeme := lookahead.lexeme;
        lookahead := Lexer.consumeSym(lexer);
        
        (* build AST node and pass it back in astNode *)
        ftypeId := AST.NewTerminalNode(AstNodeType.Qualident, lexeme);
        ftype := AST.NewNode(AstNodeType.OpenArray, ftypeId);
        astNode := AST.NewNode(AstNodeType.CastP, ftype)
        
      ELSE (* resync *)
        lookahead := skipToMatchSet(FOLLOW(CastingFormalType))
      END; (* IF *)
      
    ELSE (* addressType *)
      lookahead := addressTypeIdent(astNode)
    END (* IF *)
  END; (* IF *)
  
  RETURN lookahead
END castingFormalType;


(* --------------------------------------------------------------------------
 * private function addressTypeIdent(astNode)
 * --------------------------------------------------------------------------
 * Parses rule addressTypeIdent, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * addressTypeIdent :=
 *   ( UNSAFE '.' )? ADDRESS
 *   ;
 *
 * astNode: qualidentNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE addressTypeIdent ( VAR astNode : AstT ) : SymbolT;

VAR
 tmplist : LexQueueT;
  lookahead := SymbolT;
 
BEGIN
  PARSER_DEBUG_INFO("addressTypeIdent");
  
  LexQueue.New(tmplist);
  
  lookahead = Lexer.lookaheadSym(lexer);
  
  (* ( UNSAFE '.' )?  *)
  IF lookahead.token = Token.Address THEN
    (* UNSAFE *)
    LexQueue.Enqueue(tmplist, lookahead.lexeme);
    lookahead := Lexer.consumeSym(lexer);
    
    (* '.' *)
    IF matchToken(Token.Dot) THEN
      lookahead := Lexer.consumeSym(lexer)
    ELSE (* resync *)
      lookahead :=
        skipToMatchTokenOrSet(Token.Address, FOLLOW(AddressTypeIdent))
    END (* IF *)
  END; (* IF *)
  
  (* ADDRESS *)
  IF matchToken(Token.Address) THEN
    LexQueue.Enqueue(tmplist, lookahead.lexeme);
    lookahead := Lexer.consumeSym(lexer)
    
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(AddressTypeIdent))
  END (* IF *)
    
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewTerminalListNode(AstNodeType.Qualident, tmplist);
  LexQueue.Release(tmplist);
  
  RETURN lookahead
END addressTypeIdent;


(* --------------------------------------------------------------------------
 * private function attributedFormalType(astNode)
 * --------------------------------------------------------------------------
 * Parses rule attributedFormalType, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * attributedFormalType :=
 *   ( CONST | VAR ) ( nonAttrFormalType | simpleVariadicFormalType )
 *   ;
 *
 * astNode: formalTypeNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE attributedFormalType ( VAR astNode : AstT ) : SymbolT;

VAR
  ftype : AstT;
  nodeType : AstNodeTypeT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("attributedFormalType");
  
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* CONST | VAR *)
  IF lookahead.token = Token.Const THEN
    (* CONST *)
    nodeType := AstNodeType.ConstP
  ELSE
    (* VAR *)
    nodeType := AstNodeType.VarP
  END; (* IF *)
  
  lookahead := Lexer.consumeSym(lexer);
  
  (* nonAttrFormalType | simpleVariadicFormalType *)
  IF matchTokenOrSet(Token.ArgList, FIRST(NonAttrFormalType)) THEN
    (* simpleVariadicFormalType | *)
    IF lookahead.token = Token.ArgList THEN
      lookahead := simpleVariadicFormalType(ftype)
    ELSE (* nonAttrFormalType *)
      lookahead := nonAttrFormalType(ftype)
    END (* IF *)
    
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(attributedFormalType))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(nodeType, ftype);
  
  RETURN lookahead
END attributedFormalType;


(* --------------------------------------------------------------------------
 * private function simpleVariadicFormalType(astNode)
 * --------------------------------------------------------------------------
 * Parses rule simpleVariadicFormalType, constructs its AST node, passes the
 * node back in out-parameter astNode and returns the new lookahead symbol.
 *
 * simpleVariadicFormalType :=
 *   ARGLIST OF nonAttrFormalType
 *   ;
 *
 * astNode: (ARGLIST formalTypeNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE simpleVariadicFormalType ( VAR astNode : AstT ) : SymbolT;

VAR
  ftype : AstT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("simpleVariadicFormalType");
  
  (* ARGLIST *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* OF *)
  IF matchToken(Token.Of) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FIRST(NonAttrFormalType))
  END; (* IF *)
  
  (* nonAttrFormalType *)
  IF matchSet(FIRST(NonAttrFormalType)) THEN
    lookahead := nonAttrFormalType(ftype)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(SimpleVariadicFormalType))
  END (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.ArgList, ftype);
  
  RETURN lookahead
END simpleVariadicFormalType;


(* --------------------------------------------------------------------------
 * private function procedureHeader(astNode)
 * --------------------------------------------------------------------------
 * Parses rule procedureHeader, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * procedureHeader :=
 *   PROCEDURE procedureSignature
 *   ;
 *
 * procedureSignature :=
 *   ident ( '(' formalParams ( ';' formalParams )* ')' )?
 *   ( ':' returnedType )?
 *   ;
 *
 * astNode: (PROC procId fplist retType)
 * --------------------------------------------------------------------------
 *)
PROCEDURE procedureHeader ( VAR astNode : AstT ) : SymbolT;

VAR
  procId, fpList, retType : AstT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("procedureHeader");
    
  (* PROCEDURE *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* procedureSignature *)
  IF matchSet(FIRST(ProcedureSignature)) THEN
    (* ident *)
    IF matchSet(FIRST(Ident)) THEN
      lookahead := ident(procId)
    ELSE (* resync *)
      lookahead := skipToMatchTokenOrTokenOrSet
        (Token.LeftParen, Token.Colon, FOLLOW(procedureHeader))
    END; (* IF *)
    
    (* ( '(' formalParams ( ';' formalParams )* ')' )? *)
    IF matchToken(Token.LeftParen) THEN
      (* '(' *)
      lookahead := Lexer.consumeSym(lexer);
      
      (* formalParams ( ';' formalParams )* *)
      lookahead :=
        parseListWSeparator(formalParams, Token.Semicolon,
          FIRST(FormalParams), FOLLOW(FormalParams),
          AstNodeType.FPList, fpList);
      
      (* ')' *)
      IF matchToken(Token.RightParen) THEN
        lookahead := Lexer.consumeSym(lexer)
      ELSE (* resync *)
        lookahead :=
          skipToMatchTokenOrSet(Token.Colon, FOLLOW(procedureHeader))
      END (* IF *)
      
    ELSE (* no formal parameter list *)
      fplist := AST.emptyNode()
    END; (* IF *)
    
    (* ( ':' returnedType )? *)
    IF matchToken(Token.Colon) THEN
      (* ':' *)
      lookahead := Lexer.consumeSym(lexer);
      
      (* returnedType *)
      IF matchSet(FIRST(Qualident)) THEN
        lookahead := qualident(retType)
      ELSE (* resync *)
        lookahead := skipToMatchSet(FOLLOW(procedureHeader))
      END (* IF *)
      
    ELSE (* no return type *)
      retType := AST.emptyNode()
    END (* IF *)
    
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(procedureHeader))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.Proc, procId, fplist, retType);
  
  RETURN lookahead
END procedureHeader;


(* --------------------------------------------------------------------------
 * private function formalParams(astNode)
 * --------------------------------------------------------------------------
 * Parses rule formalParams, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * formalParams :=
 *   identList ':' ( nonAttrFormalType | simpleVariadicFormalType ) |
 *   attributedFormalParams
 *   ;
 *
 * astNode: (FPARAMS identListNode formalTypeNode) | attrFParamsNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE formalParams ( VAR astNode : AstT ) : SymbolT;

VAR
  idlist, ftype : AstT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("formalParams");
  
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* nonAttrFormalParams | *)
  IF inFIRST(IdentList, lookahead.token) THEN
    lookahead := nonAttrFormalParams(idlist);
    (* ':' *)
    IF matchToken(Token.Colon) THEN
      lookahead := Lexer.consumeSym(lexer)
    ELSE
      lookahead :=
        skipToMatchTokenOrSet(Token.ArgList, FIRST(nonAttrFormalType))
    END; (* IF *)
    
    (* nonAttrFormalType | simpleVariadicFormalType *)
    IF matchTokenOrSet(Token.ArgList, FIRST(NonAttrFormalType)) THEN
      (* simpleVariadicFormalType | *)
      IF lookahead.token = Token.ArgList THEN
        lookahead := simpleVariadicFormalType(ftype)
      ELSE (* nonAttrFormalType *)
        lookahead := nonAttrFormalType(ftype)
      END (* IF *)
      
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(FormalParams))
    END (* IF *)
    
    (* build AST node and pass it back in astNode *)
    astNode := AST.NewNode(AstNodeType.FParams, idlist, ftype)
    
  ELSE (* attributedFormalParams *)
    lookahead := attributedFormalParams(astNode)
  END;
  
  RETURN lookahead
END formalParams;


(* --------------------------------------------------------------------------
 * private function attributedFormalParams(astNode)
 * --------------------------------------------------------------------------
 * Parses rule attributedFormalParams, constructs its AST node, passes the
 * node back in out-parameter astNode and returns the new lookahead symbol.
 *
 * attributedFormalParams :=
 *   ( CONST | VAR ) identList ':'
 *   ( nonAttrFormalType | simpleVariadicFormalType )
 *   ;
 *
 * astNode:
 *  (CONSTP identListNode formalTypeNode) |
 *  (VARP identListNode formalTypeNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE attributedFormalParams ( VAR astNode : AstT ) : SymbolT;

VAR
  idlist, ftype : AstT;
  nodeType : AstNodeTypeT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("attributedFormalParams");
  
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* CONST | *)
  IF lookahead.token = Token.Const THEN
    nodeType := AstNodeType.ConstP
  ELSE (* VAR *)
    nodeType := AstNodeType.VarP
  END; (* IF *)
  
  (* identList *)
  IF matchSet(FIRST(IdentList) THEN
    lookahead := identList(idlist);
    (* ':' *)
    IF matchToken(Token.Colon) THEN
      lookahead := Lexer.consumeSym(lexer)
    ELSE (* resync *)
      lookahead :=
        skipToMatchTokenOrSet(Token.ArgList, FIRST(NonAttrFormalType))
    END (* IF *)
    
  ELSE (* resync *)
    lookahead :=
      skipToMatchTokenOrSet(Token.ArgList, FIRST(NonAttrFormalType))
  END; (* IF *)
  
  (* nonAttrFormalType | simpleVariadicFormalType *)
  IF matchTokenOrSet(Token.ArgList, FIRST(NonAttrFormalType)) THEN
    (* simpleVariadicFormalType | *)
    IF lookahead.token = Token.ArgList THEN
      lookahead := simpleVariadicFormalType(ftype)
    ELSE (* nonAttrFormalType *)
      lookahead := nonAttrFormalType(ftype)
    END (* IF *)
    
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(AttributedFormalParams))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  ftype := AST.NewNode(nodeType, ftype);
  astNode := AST.NewNode(AstNodeType.FParams, idlist, ftype);
  
  RETURN lookahead
END attributedFormalParams;


(* --------------------------------------------------------------------------
 * private function implOrPrgmModule(astNode)
 * --------------------------------------------------------------------------
 * Parses rule implOrPrgmModule, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * implOrPrgmModule :=
 *   IMPLEMENTATION? MODULE moduleIdent ';'
 *   privateImport* block moduleIdent '.'
 *   ;
 *
 * alias privateImport = import ;
 *
 * astNode: (IMPMOD moduleIdent implist blockNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE implOrPrgmModule ( VAR astNode : AstT ) : SymbolT;

VAR
  moduleIdent, implist, blockNode : AstT;
  ident1, ident2 : StringT;
  tmplist : AstQueueT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("implOrPrgmModule");
  
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* IMPLEMENTATION? *)
  IF lookahead.token = Token.Implementation THEN
    lookahead := Lexer.consumeSym(lexer);
    
    (* TO DO: implement imp flag *)
    
  END; (* IF *)
  
  (* MODULE *)
  IF matchToken(Token.Module) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrToken(Token.StdIdent, Token.Semicolon)
  END; (* IF *)
  
  (* moduleIdent *)
  IF matchToken(Token.StdIdent) THEN
    ident1 = lookahead.lexeme;
    lookahead := Lexer.consumeSym(lexer)
    
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrSet()
  END; (* IF *)
  
  (* ';' *)
  IF matchToken(Token.Semicolon) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSetOrSet(FIRST(Import), FIRST(Block))
  END; (* IF *)
  
  tmplist := AstQueue.New();

  (* privateImport* *)
  WHILE lookahead.token = Token.Import DO
    (* alias privateImport = import *)
    lookahead := import(implist);
    AstQueue.Enqueue(tmplist, implist)
  END (* WHILE *)
  
  implist := AST.NewListNode(AstNodeType.ImpList, tmplist);
  tmplist:= AstQueue.ResetQueue(tmplist);
    
  (* block *)
  IF matchSet(FIRST(Block)) THEN
    lookahead := block(blockNode)
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrSet(Token.StdIdent, FIRST(Block));
    (* retry *)
    IF inFIRST(Block, lookahead.token) THEN
      lookahead := block(blockNode)
    END (* IF *)
  END; (* IF *)
  
  (* moduleIdent *)
  IF matchToken(Token.StdIdent) THEN
    ident2 := lookahead.lexeme;
    lookahead := Lexer.ConsumeSym(lexer);
    
    IF ident1 # ident2 THEN
      (* TO DO: report error -- module identifiers don't match *) 
    END (* IF *)
    
  ELSE (* resync *)
    lookahead :=
      skipToMatchTokenOrSet(Token.Dot, FOLLOW(ImplementationModule))
  END; (* IF *)
  
  (* '.' *)
  IF matchToken(Token.Period) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(ImplementationModule))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  moduleIdent := AST.NewTerminalNode(AstNodeType.Ident, ident1);
  astNode := AST.NewNode(AstNodeType.ImpMod, moduleIdent, implist, blockNode);
  
  RETURN lookahead
END implOrPrgmModule;


(* --------------------------------------------------------------------------
 * private function block(astNode)
 * --------------------------------------------------------------------------
 * Parses rule block, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * block :=
 *   declaration*
 *   ( BEGIN statementSequence )? END
 *   ;
 *
 * astNode: (BLOCK declListNode stmtSeqNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE block ( VAR astNode : AstT ) : SymbolT;

VAR
  decllist, stmtSeq : AstT;
  tmplist : AstQueueT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("block");
  
  AstQueue.New(tmplist);
  
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* declaration* *)
  WHILE inFIRST(Declaration, lookahead.token) DO
    lookahead := definition(decllist);
    AstQueue.Enqueue(tmplist, decllist)
  END (* WHILE *)
  
  decllist := AST.NewListNode(AstNodeType.DeclList, tmplist);
  tmplist := AstQueue.ResetQueue(tmplist);
  
  (* ( BEGIN statementSequence )? *)
  IF lookahead.token = Token.Begin THEN
    (* BEGIN *)
    lookahead := Lexer.consumeSym(lexer)
    
    (* statementSequence *)
    IF matchSet(FIRST(StatementSequence)) THEN
      lookahead := statementSequence(stmtSeq)
    ELSE (* resync *)
      lookahead := skipToMatchTokenOrSet(Token.End, FOLLOW(Block))
    END (* IF *)
    
  ELSE (* no statements *)
    stmtSeq := AST.emptyNode()
  END; (* IF *)
  
  (* END *)
  IF matchToken(Token.End) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(Block))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.Block, decllist, stmtSeq);
  AstQueue.Release(tmplist);
  
  return lookahead
END block;


(* --------------------------------------------------------------------------
 * private function declaration(astNode)
 * --------------------------------------------------------------------------
 * Parses rule declaration, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * declaration :=
 *   ALIAS ( aliasDeclaration ';' )+ |
 *   CONST ( constDeclaration ';' )+ |
 *   TYPE ( typeDeclaration ';' )+ |
 *   VAR ( varOrFieldDeclaration ';' )+ |
 *   procedureHeader ';' block ident ';' |
 *   toDoList ';'
 *   ;
 *
 * alias constDeclaration = constDefinition ;
 *
 * astNode: (DECLLIST declNode1 declNode2 ... declNodeN)
 * --------------------------------------------------------------------------
 *)
PROCEDURE declaration ( VAR astNode : AstT ) : SymbolT;

VAR
  lookahead := SymbolT;

BEGIN
  PARSER_DEBUG_INFO("declaration");

  lookahead := Lexer.lookaheadSym(lexer);
  
  CASE lookahead.token OF
    (* ALIAS *)
    Token.Alias :
      lookahead := Lexer.consumeSym(lexer);
      
      (* ( aliasDeclaration ';' )+ *)
      lookahead :=
        parseListWTerminator(aliasDeclaration, Token.Semicolon,
          FIRST(AliasDeclaration), FOLLOW(AliasDeclaration),
          AstNodeType.DeclList, astNode)
      
  (* CONST *)
  | Token.Const :
      lookahead := Lexer.consumeSym(lexer);
      
      (* ( constDefinition ';' )+ *)
      lookahead :=
        parseListWTerminator(constDefinition, Token.Semicolon,
          FIRST(ConstDefinition), FOLLOW(ConstDefinition),
          AstNodeType.DeclList, astNode)
            
  (* TYPE *)
  | Token.Type :
      lookahead := Lexer.consumeSym(lexer);
      
      (* ( typeDeclaration ';' )+ *)
      lookahead :=
        parseListWTerminator(typeDeclaration, Token.Semicolon,
          FIRST(TypeDeclaration), FOLLOW(TypeDeclaration),
          AstNodeType.DeclList, astNode)
   
  (* VAR ( varOrFieldDeclaration ';' )+ | *)
  | Token.Var :
      lookahead := Lexer.consumeSym(lexer);
      
      (* ( varOrFieldDeclaration ';' )+ *)
      lookahead :=
        parseListWTerminator(varOrFieldDeclaration, Token.Semicolon,
          FIRST(VarOrFieldDeclaration), FOLLOW(VarOrFieldDeclaration),
          AstNodeType.DeclList, astNode)
  
  (* procedureHeader ';' block ident ';' | *)
  | Token.Procedure :
      (* procedureHeader *)
      lookahead := procDeclaration(astNode)
            
  (* toDoList ';' *)
  | Token.To :  
      (* toDoList *)
      lookahead := toDoList(astNode);
      
      (* ';' *)
      IF matchToken(Token.Semicolon) THEN
        lookahead := Lexer.consumeSym(lexer)
      ELSE (* resync *)
        lookahead := skipToMatchSet(FOLLOW(Declaration))
      END (* IF *)
      
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(Declaration))
  END; (* CASE *)
    
  RETURN lookahead
END declaration;


(* --------------------------------------------------------------------------
 * private function aliasDeclaration(astNode)
 * --------------------------------------------------------------------------
 * Parses rule aliasDeclaration, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * aliasDeclaration :=
 *   namedAliasDecl | wildcardAliasDecl
 *   ;
 *
 * astNode: aliasDeclNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE aliasDeclaration ( VAR astNode : AstT ) : SymbolT;

VAR
  lookahead := SymbolT;

BEGIN
  PARSER_DEBUG_INFO("aliasDeclSection");
  
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* namedAliasDecl | wildcardAliasDecl *)
  IF lookahead.token = Token.Asterisk THEN
    lookahead := wildcardAliasDecl(astNode)
  ELSE
    lookahead := namedAliasDecl(astNode)
  END; (* IF *)
  
  RETURN lookahead
END aliasDeclaration;


(* --------------------------------------------------------------------------
 * private function namedAliasDecl(astNode)
 * --------------------------------------------------------------------------
 * Parses rule namedAliasDecl, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * namedAliasDecl :=
 *   aliasName
 *     ( '=' qualifiedName | ( ',' aliasName )+ '=' qualifiedWildcard )
 *   ;
 *
 * alias aliasName = ident ;
 *
 * alias qualifiedName = qualident ;
 *
 * qualifiedWildcard :=
 *   qualident '.*'
 *   ;
 *
 * astNode:
 *  (ALIASDECL identNode qualidentNode) |
 *  (ALIASDECLLIST aliasDeclNode1 aliasDeclNode2 ... aliasDeclNodeN)
 * --------------------------------------------------------------------------
 *)
PROCEDURE namedAliasDecl ( VAR astNode : AstT ) : SymbolT;

VAR
  aliasId, translation: AstT;
  tmplist : AstQueueT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("aliasDeclSection");
  
  AstQueue.New(tmplist);
  
  (* aliasName *)
  lookahead := ident(aliasId);
  
  (* '=' qualifiedName | ( ',' aliasName )+ '=' qualifiedWildcard *)
  IF matchSet(FIRST(NamedAliasDeclTail)) THEN
    (* '=' qualifiedName | *)
    IF lookahead.token = Token.Equal THEN
      (* '=' *)
      lookahead := Lexer.consumeSym(lexer);
      
      (* qualifiedName *)
      IF matchSet(FIRST(Qualident)) THEN
        lookahead := qualident(translation)
      ELSE (* resync *)
        lookahead := skipToMatchSet(FOLLOW(namedAliasDecl))
      END; (* IF *)
      
      astNode := AST.NewNode(AstNodeType.AliasDecl, aliasId, translation)
      
    ELSE (* ( ',' aliasName )+ '=' qualifiedWildcard *)
      AstQueue.Enqueue(tmplist, aliasId);
      
      (* ( ',' aliasName )+ *)
      REPEAT
        IF matchToken(Token.Comma) THEN
          (* ',' *)
          lookahead := Lexer.consumeSym(lexer);
          
          (* aliasName *)
          IF matchSet(FIRST(Ident)) THEN
            lookahead := ident(aliasId);
            AstQueue.Enqueue(tmplist, aliasId)
            
          ELSE (* resync *)
            lookahead := skipToMatchSet(FOLLOW(Ident))
          END (* IF *)
          
        ELSE (* resync *)
          lookahead := skipToMatchTokenOrTokenOrSet
            (Token.Comma, Token.Equal, FOLLOW(namedAliasDecl));
        END (* IF *)
      UNTIL lookahead.token # Token.Comma;
      
      (* '=' qualifiedWildcard *)
      IF matchToken(Token.Equal) THEN
        (* '=' *)
        lookahead := Lexer.consumeSym(lexer);
      ELSE (* resync *)
        lookahead := skipToMatchSet(FIRST(QualifiedWildcard))
      END; (* IF *)
      
      (* qualifiedWildcard *)
      IF matchSet(FIRST(QualifiedWildcard)) THEN
        lookahead := qualifiedWildcard(translation)
      ELSE (* resync *)
        lookahead := skipToMatchSet(FOLLOW(namedAliasDecl))
      END; (* IF *)
      
      (* TO DO : resolve aliases in tmplist using wildcard *)
      
      astNode := AST.NewListNode(AstNodeType.AliasDeclList, tmplist)
    END (* IF *)
    
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(namedAliasDecl))
  END; (* IF *)
  
  RETURN lookahead
END namedAliasDecl;


(* --------------------------------------------------------------------------
 * private function wildcardAliasDecl(astNode)
 * --------------------------------------------------------------------------
 * Parses rule wildcardAliasDecl, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * wildcardAliasDecl :=
 *   '*' '=' qualifiedWildcard
 *   ;
 *
 * astNode: (ALIASDECL (IDENT "*") qualidentNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE wildcardAliasDecl ( VAR astNode : AstT ) : SymbolT;

VAR
  lookahead := SymbolT;

BEGIN
  PARSER_DEBUG_INFO("aliasDeclSection");
  
  (* '*' *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* '=' *)
  IF matchToken(Token.Equal) THEN
    lookahead := Lexer.consumeSym(lexer);
  ELSE (* resync *)
    lookahead := skipToMatchSet(FIRST(QualifiedWildcard))
  END; (* IF *)
  
  (* qualifiedWildcard *)
  IF matchSet(FIRST(QualifiedWildcard)) THEN
    lookahead := qualifiedWildcard(translation);
    
    (* build AST node and pass it back in astNode *)
    astNode := AST.NewNode(AstNodeType.WcAlias, translation);
    
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(WildcardAliasDecl))
  END (* IF *)
  
  RETURN lookahead
END wildcardAliasDecl;


(* --------------------------------------------------------------------------
 * private function qualifiedWildcard(astNode)
 * --------------------------------------------------------------------------
 * Parses rule qualifiedWildcard, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * qualifiedWildcard :=
 *   qualident '.*'
 *   ;
 *
 * astNode: qualidentNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE qualifiedWildcard ( VAR astNode : AstT ) : SymbolT;

VAR
  lookahead := SymbolT;

BEGIN
  PARSER_DEBUG_INFO("qualifiedWildcard");
  
  (* qualident *)
  lookahead := qualident(astnode);
  
  (* '.*' *)
  IF matchToken() THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(QualifiedWildcard))
  END; (* IF *)
  
  RETURN lookahead
END qualifiedWildcard;


(* --------------------------------------------------------------------------
 * private function typeDeclaration(astNode)
 * --------------------------------------------------------------------------
 * Parses rule typeDeclaration, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * typeDeclaration :=
 *   ident '=' ( indeterminateType |  type )
 *   ;
 *
 * astNode: (TYPEDECL identNode typeNode) | indeterminateTypeNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE typeDeclaration ( VAR astNode : AstT ) : SymbolT;

VAR
  typeId, typeDecl : AstT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("typeDeclaration");
  
  (* ident *)
  lookahead := ident(typeId);
  
  (* '=' *)
  IF matchToken(Token.Equal) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead :=
      skipToMatchTokenOrSet(Token.In, FIRST(Type))
  END; (* IF *)
  
  (* indeterminateType | type *)
  IF matchTokenOrSet(Token.In, FIRST(type)) THEN
    (* indeterminateType | *)
    IF lookahead.token = Token.In THEN
      lookahead := indeterminateType(typeDecl)
    ELSE (* type *)
      lookahead := type(typeDecl)
    END (* IF *)
    
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(typeDeclaration))
  END; (* IF *)
    
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.TypeDecl, typeId, typeDecl);
  
  RETURN lookahead
END typeDeclaration;


(* --------------------------------------------------------------------------
 * private function indeterminateType(astNode)
 * --------------------------------------------------------------------------
 * Parses rule indeterminateType, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * indeterminateType :=
 *   IN RECORD ( fieldDeclaration ';' )+ indeterminateField END
 *   ;
 *
 * alias fieldDeclaration = varOrFieldDeclaration ;
 *
 * astNode: (INREC fieldListNode identNode identNode qualidentNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE indeterminateType ( VAR astNode : AstT ) : SymbolT;

VAR
  fieldListSeq, inField : AstT;
  lookahead := SymbolT;

BEGIN
  PARSER_DEBUG_INFO("typeDeclaration");
    
  (* IN *)
  lookahead := Lexer.consumeSym(lexer);
      
  (* RECORD *)
  IF matchToken(Token.Record) THEN
    lookahead := Lexer.consumeSym(lexer);
  ELSE (* resync *)
    lookahead := skipToMatchSet(FIRST(varOrFieldDeclaration))
  END; (* IF *)
  
  (* ( varOrFieldDeclaration ';' )+ *)
  lookahead :=
    parseListWTerminator(varOrFieldDeclaration, Token.Semicolon,
      FIRST(VarOrFieldDeclaration), FIRST(indeterminateField),
      AstNodeType.FieldListSeq, fieldListSeq)
      
  (* indeterminateField *)
  IF matchSet(FIRST(indeterminateField)) THEN
    lookahead := indeterminateField(inField)
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrSet(Token.End, FOLLOW(IndeterminateType))
  END; (* IF *)
  
  (* END *)
  IF matchToken(Token.End) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE
    lookahead := skipToMatchSet(FOLLOW(IndeterminateType))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.InRec, fieldListSeq, inField);
  
  RETURN lookahead
END indeterminateType;


(* --------------------------------------------------------------------------
 * private function indeterminateField(astNode)
 * --------------------------------------------------------------------------
 * Parses rule indeterminateField, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * indeterminateField :=
 *   '+' ident ':' BARE ARRAY discriminantFieldIdent OF typeIdent
 *   ;
 *
 * alias discriminantFieldIdent = ident ;
 *
 * astNode: (TERMLIST identNode identNode qualidentNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE indeterminateField ( VAR astNode : AstT ) : SymbolT;

VAR
  fieldId, discrId, typeId : AstT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("typeDeclaration");
  
  (* '+' *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* ident *)
  IF matchSet(FIRST(Ident)) THEN
    lookahead := ident(fieldId)
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrTokenOrSet
      (Token.Colon, Token.Bare, FOLLOW(indeterminateField))
  END; (* IF *)

  (* ':' *)
  IF matchToken(Token.Colon) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead :=
      skipToMatchTokenOrTokenOrSet(Token.Bare, Token.Array, FIRST(Ident))
  END; (* IF *)
  
  (* BARE *)
  IF matchToken(Token.Bare) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrSet(Token.Array, FIRST(Ident))
  END; (* IF *)
  
  (* ARRAY *)
  IF matchToken(Token.Bare) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead :=
      skipToMatchTokenOrSet(Token.Of, FIRST(Ident))
  END; (* IF *)
  
  (* discriminantFieldIdent *)
  IF matchSet(FIRST(Ident)) THEN
    lookahead := ident(discrId)
  ELSE (* resync *)
    lookahead :=
      skipToMatchTokenOrSet(Token.Of, FIRST(Qualident))
  END; (* IF *)
  
  (* OF *)
  IF matchToken(Token.Of) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead :=
      skipToMatchSetOrSet(FIRST(Qualident), FOLLOW(IndeterminateField))
  END; (* IF *)
  
  (* typeIdent *)
  IF matchSet(FIRST(Qualident)) THEN
    lookahead := qualident(typeId)
  ELSE (* resync *)
    lookahead :=
      skipToMatchTokenOrSet(Token.Of, FOLLOW(indeterminateField))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.InField, fieldId, discrId, typeId);
  
  RETURN lookahead
END indeterminateField;


(* --------------------------------------------------------------------------
 * private function varOrFieldDeclaration(astNode)
 * --------------------------------------------------------------------------
 * Parses rule varOrFieldDeclaration, constructs its AST node, passes the
 * node back in out-parameter astNode and returns the new lookahead symbol.
 *
 * varOrFieldDeclaration :=
 *   identList ':' ( typeIdent | anonType )
 *   ;
 *
 * astNode: (VARDECL identListNode typeNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE varOrFieldDeclaration ( VAR astNode : AstT ) : SymbolT;

VAR
  idlist, typeNode : AstT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("varOrFieldDeclaration");
  
  (* identList *)
  lookahead := identList(idlist);
  
  (* ':' *)
  IF matchToken(Token.Colon) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSetOrSet(FIRST(Qualident), FIRST(AnonType))
  END; (* IF *)
  
  (* typeIdent | anonType *)
  IF matchSetOrSet(FIRST(Qualident), FIRST(AnonType)) THEN
    (* typeIdent | *)
    IF inFIRST(Qualident, lookahead.token) THEN
      lookahead := qualident(typeNode)
    ELSE (* anonType *)
      lookahead := anonType(typeNode)
    END (* IF *)
    
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(VarOrFieldDeclaration))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.VarDecl, idlist, typeNode);
  
  RETURN lookahead
END varOrFieldDeclaration;


(* --------------------------------------------------------------------------
 * private function anonType(astNode)
 * --------------------------------------------------------------------------
 * Parses rule anonType, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * anonType :=
 *   ARRAY valueCount OF typeIdent |
 *   subrangeType |
 *   procedureType
 *   ;
 *
 * astNode: arrayTypeNode | subrangeTypeNode | procedureTypeNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE anonType ( VAR astNode : AstT ) : SymbolT;

VAR
  valueCount, baseType : AstT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("anonType");
  
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* ARRAY valueCount OF typeIdent | *)
  IF lookahead.token = Token.Array THEN
    lookahead := Lexer.consumeSym(lexer);
    (* valueCount *)
    IF matchSet(FIRST(Expression) THEN
      lookahead := expression(valueCount)
    ELSE (* resync *)
      lookahead := skipToMatchTokenOrSet(Token.Of, FIRST(Qualident))
    END; (* IF *)
    
    (* OF *)
    IF matchToken(Token.Of) THEN
      lookahead := Lexer.consumeSym(lexer)
    ELSE (* resync *)
      lookahead := skipToMatchSet(FIRST(Qualident))
    END; (* IF *)
    
    (* typeIdent *)
    IF matchSet(FIRST(Qualident) THEN
      lookahead := qualident(baseType)
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(AnonType))
    END; (* IF *)
    
    (* build AST node and pass it back in astNode *)
    astNode := AST.NewNode(AstNodeType.ArrayType, valueCount, baseType)
  
  (* subrangeType | *)
  ELSIF inFIRST(SubrangeType) THEN
    lookahead := subrangeType(astNode)
    
  (* procedureType *)
  ELSE
    lookahead := procedureType(astNode)
  END; (* IF *)
  
  RETURN lookahead
END anonType;


(* --------------------------------------------------------------------------
 * private function procDeclaration(astNode)
 * --------------------------------------------------------------------------
 * Parses rule procDeclaration, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * procDeclaration :=
 *   procedureHeader ';' block ident ';'
 *   ;
 *
 * astNode: aliasDeclNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE procDeclaration ( VAR astNode : AstT ) : SymbolT;

VAR
  procId, procHeader, blockNode : AstT;
  ident1, ident2 : StringT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("procDeclaration");
    
  (* procedureHeader *)
  lookahead := procedureHeader(procHeader);
  procId := AST.subnodeForIndex(procHeader, 0);
  ident1 := AST.valueForIndex(procId, 0);
  
  (* ';' *)
  IF matchToken(Token.Semicolon) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FIRST(Block))
  END; (* IF *)
  
  (* block *)
  IF matchSet(FIRST(Block)) THEN
    lookahead := block(blockNode)
  ELSE (* resync *)
    lookahead := skipToMatchSetOrSet(FIRST(Block), FIRST(Ident));
    (* retry *)
    IF inFIRST(Block) THEN
      lookahead := block(blockNode)
    END (* IF *)
  END (* IF *)
  
  (* ident *)
  IF matchSet(FIRST(Ident)) THEN
    ident2 := lookahead.lexeme;
    lookahead := Lexer.ConsumeSym(lexer);
    
    IF ident1 # ident2 THEN
      (* TO DO: report error -- procedure identifiers don't match *) 
    END (* IF *)
    
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrSet(Token.Semicolon, FOLLOW(Declaration))
  END (* IF *)
  
  (* ';' *)
  IF matchToken(Token.Semicolon, FOLLOW(Declaration)) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(Declaration))
  END (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.ProcDecl, procHeader, blockNode);
  
  RETURN lookahead
END procDeclaration;


(* --------------------------------------------------------------------------
 * private function statementSequence(astNode)
 * --------------------------------------------------------------------------
 * Parses rule statementSequence, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * statementSequence :=
 *   statement ( ';' statement )*
 *   ;
 *
 * astNode: (STMTSEQ stmtNode1 stmtNode2 ... stmtNodeN)
 * --------------------------------------------------------------------------
 *)
PROCEDURE statementSequence ( VAR astNode : AstT ) : SymbolT;

VAR
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("statementSequence");
  
  (* statement ( ';' statement )* *)
  lookahead :=
    parseListWSeparator(statement, Token.Semicolon,
      FIRST(Statement), FOLLOW(Statement),
      AstTypeNode.StmtSeq, astNode);
  
  RETURN lookahead
END statementSequence;


(* --------------------------------------------------------------------------
 * private function statement(astNode)
 * --------------------------------------------------------------------------
 * Parses rule statement, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * statement :=
 *   emptyStatement | memMgtOperation | updateOrProcCall | returnStatement |
 *   ifStatement | caseStatement | loopStatement | whileStatement |
 *   repeatStatement | forStatement | EXIT
 *   ;
 *
 * alias emptyStatement = toDoList ;
 *
 * astNode: statementNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE statement ( VAR astNode : AstT ) : SymbolT;

VAR
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("statement");
  
  lookahead := Lexer.lookaheadSym(lexer);
    
  CASE lookahead.token OF
  (* emptyStatement | *)
    Token.To : lookahead := toDoList(astNode)
      
  (* memMgtOperation | *)
  | Token.New,
    Token.Release : lookahead := memMgtOperation(astNode)
      
  (* returnStatement | *)
  | Token.Return : lookahead := returnStatement(astNode)
    
  (* ifStatement | *)
  | Token.If : lookahead := ifStatement(astNode)
    
   (* caseStatement | *)
  | Token.Case : lookahead := caseStatement(astNode)
    
  (* loopStatement | *)
  | Token.Loop : lookahead := loopStatement(astNode)
    
  (* whileStatement | *)
  | Token.While : lookahead := whileStatement(astNode)
    
  (* repeatStatement | *)
  | Token.Repeat : lookahead := repeatStatement(astNode)
    
  (* forStatement | *)
  | Token.For : lookahead := forStatement(astNode)
    
  (* EXIT | *)
  | Token.Exit :
      lookahead := Lexer.consumeSym(lexer);
      astNode := AST.NewNode(AstNodeType.Exit)
      
  ELSE (* updateOrProcCall *)
    lookahead := updateOrProcCall(astNode)    
  END; (* CASE *)
  
  RETURN lookahead
END statement;


(* --------------------------------------------------------------------------
 * private function toDoList(astNode)
 * --------------------------------------------------------------------------
 * Parses rule toDoList, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * toDoList :=
 *   TO DO trackingRef? taskToDo ( ';' taskToDo )* END
 *   ;
 *
 * trackingRef :=
 *   '(' issueId ',' weight ')'
 *   ;
 *
 * alias issueId = wholeNumber ;
 *
 * alias weight = constExpression ;
 *
 * astNode: (TODO issueId weight taskList)
 * --------------------------------------------------------------------------
 *)
PROCEDURE toDoList ( VAR astNode : AstT ) : SymbolT;

VAR
  issueId, weight, taskList : AstT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("toDoList");
  
  (* TO *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* DO *)
  IF matchToken(Token.Do) THEN
    lookahead := Lexer.consumeSym(lexer);
  ELSE (* resync *)
    lookahead :=
      skipToMatchSetOrSet(FIRST(TrackingRef), FIRST(TaskToDo))
  END; (* IF *)
  
  (* trackingRef? *)
  IF inFIRST(TrackingRef, lookahead.token) THEN
    (* '(' *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* issueId *)
    IF matchToken(Token.NumberLiteral) THEN
      issueId := AST.NewTerminalNode(AstNodeTye.IntVal, lookahead.lexeme);
      lookahead := Lexer.consumeSym(lexer);
    ELSE (* resync *)
      lookahead := skipToMatchTokenOrSet(Token.Comma, FIRST(Expression));
      issueId := AST.emptyNode()
    END; (* IF *)
    
    (* ',' *)
    IF matchToken(Token.Comma) THEN
      lookahead := Lexer.consumeSym(lexer);
    ELSE (* resync *)
      lookahead := skipToMatchTokenOrSet(Token.RightParen, FIRST(Expression))
    END; (* IF *)
    
    (* weight *)
    IF matchSet(FIRST(Expression)) THEN
      lookahead := expression(weight)
    ELSE (* resync *)
      lookahead :=
        skipToMatchTokenOrSet(Token.RightParen, FOLLOW(TrackingRef));
      weight := AST.emptyNode()
    END; (* IF *)
    
    (* ')' *)
    IF matchToken(Token.RightParen) THEN
      lookahead := Lexer.consumeSym(lexer);
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(TrackingRef))
    END (* IF *)
    
  ELSE (* no tracking reference *)
    issueId := AST.emptyNode();
    weight := AST.emptyNode()
  END;
  
  (* taskToDo ( ';' taskToDo )* *)
  lookahead :=
    parseListWithSeparator(taskToDo, Token.Semicolon,
    FIRST(TaskToDo), FOLLOW(TaskToDo, AstNodeType.TaskToDo, taskList);
    
  (* END *)
  IF matchToken(Token.End) THEN
    lookahead := Lexer.consumeSym(lexer);
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(ToDoList))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.ToDo, issueId, weight, taskList);
  
  RETURN lookahead
END toDoList;


(* --------------------------------------------------------------------------
 * private function taskToDo(astNode)
 * --------------------------------------------------------------------------
 * Parses rule taskToDo, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * taskToDo :=
 *   description ( ',' estimatedHours )?
 *   ;
 *
 * alias description = StringLiteral ;
 *
 * alias estimatedHours = constExpression ;
 *
 * astNode: (TASK description estimatedHours)
 * --------------------------------------------------------------------------
 *)
PROCEDURE taskToDo ( VAR astNode : AstT ) : SymbolT;

VAR
  description, estimatedHours : AstT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("taskToDo");
  
  (* description *)
  description := AST.NewTerminalNode(AstNodeType.QuotedVal, lookahead.lexeme);
  
  (* ( ',' estimatedHours )? *)
  IF lookahead.token = Token.Comma THEN
    (* ',' *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* estimatedHours *)
    IF matchSet(FIRST(Expression)) THEN
      lookahead := expression(estimatedHours)
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(TaskToDo));
    END (* IF *)
    
  ELSE (* no estimate given *)
    estimatedHours := AST.emptyNode()
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.TaskToDo, description, estimatedHours);
    
  RETURN lookahead
END taskToDo;


(* --------------------------------------------------------------------------
 * private function memMgtOperation(astNode)
 * --------------------------------------------------------------------------
 * Parses rule memMgtOperation, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * memMgtOperation :=
 *   NEW designator ( OF initSize )? |
 *   RELEASE designator
 *   ;
 *
 * alias initSize = expression ;
 *
 * astNode: (NEW desigNode exprNode) | (RELEASE desigNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE memMgtOperation ( VAR astNode : AstT ) : SymbolT;

VAR
  desig, initSize : AstT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("memMgtOperation");
  
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* NEW designator ( OF initSize )? | *)
  IF lookahead.token = Token.New THEN
    (* NEW *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* designator *)
    IF matchSet(FIRST(Designator)) THEN
      lookahead := designator(desig)
    ELSE (* resync *)
      lookahead := skipToMatchTokenOrSet(Token.Of, FOLLOW(MemMgtOperation))
    END; (* IF *)
    
    (* ( OF initSize )? *)
    IF lookahead.token = Token.Of THEN
      
      (* OF *)
      lookahead := Lexer.consumeSym(lexer);
      
      (* initSize *)
      IF matchSet(FIRST(Expression)) THEN
        lookahead := expression(initSize)
      ELSE (* resync *)
        lookahead := skipToMatchSet(FOLLOW(MemMgtOperation))
      END (* IF *)
      
    ELSE (* no size *)
      initSize := AST.emptyNode()
    END; (* IF *)
    
    (* build AST node and pass it back in astNode *)
    astNode := AST.NewNode(AstNodeType.New, desig, initSize)
    
  (* RELEASE designator *)
  ELSE
    (* RELEASE *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* designator *)
    IF matchSet(FIRST(Designator)) THEN
      lookahead := designator(desig)
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(MemMgtOperation))
    END; (* IF *)
    
    (* build AST node and pass it back in astNode *)
    astNode := AST.NewNode(AstNodeType.Release, desig)
  END; (* IF *)
  
  RETURN lookahead
END memMgtOperation;


(* --------------------------------------------------------------------------
 * private function updateOrProcCall(astNode)
 * --------------------------------------------------------------------------
 * Parses rule updateOrProcCall, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * updateOrProcCall :=
 *   designator
 *     ( IncOrDecSuffix | ':=' expression | '(' expressionList ')' )?
 *   ;
 *
 * IncOrDecSuffix := '++' | '--' ;
 *
 * astNode: (ASSIGN desigNode exprNode) | (PCALL exprListNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE updateOrProcCall ( VAR astNode : AstT ) : SymbolT;

VAR
  desig, expr, exprList : AstT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("updateOrProcCall");
  
  (* designator *)
  lookahead := designator(desig);
  
  (* ( IncOrDecSuffix | ':=' expression | '(' expressionList ')' )? *)
  CASE lookahead.token OF
  (* IncOrDecSuffix | *)
    Token.PlusPlus :
      (* '++' *)
      lookahead := Lexer.consumeSym(lexer);
      
      (* build AST node and pass it back in astNode *)
      astNode := AST.NewNode(AstNodeType.Incr, desig)
    
  | Token.MinusMinus :
      (* '--' *)
      lookahead := Lexer.consumeSym(lexer);
      
      (* build AST node and pass it back in astNode *)
      astNode := AST.NewNode(AstNodeType.Decr, desig)
      
  (* ':=' expression | *)
  | Token.Assign :
      (* ':=' *)
      lookahead := Lexer.consumeSym(lexer);
      
      (* expression *)
      IF matchSet(FIRST(Expression)) THEN
        lookahead := expression(expr)
      ELSE (* resync *)
        lookahead := skipToMatchSet(FOLLOW(UpdateOrProcCall))
      END; (* IF *)
      
      (* build AST node and pass it back in astNode *)
      astNode := AST.NewNode(AstNodeType.Assign, desig, expr)
  
  (* '(' expressionList ')' *)
  | Token.LeftParen :
      (* '(' *)
      lookahead := Lexer.consumeSym(lexer);
      
      (* expressionList *)
      IF matchSet(FIRST(ExpressionList)) THEN
        lookahead := expressionList(exprList)
      ELSE (* resync *)
        lookahead :=
          skipToMatchTokenOrSet(Token.RightParen, FOLLOW(UpdateOrProcCall))
      END; (* IF *)
      
      (* ')' *)
      IF matchToken(Token.RightParen) THEN
        lookahead := Lexer.consumeSym(lexer)
      ELSE (* resync *)
        lookahead := skipToMatchSet(FOLLOW(UpdateOrProcCall))
      END; (* IF *)
      
      (* build AST node and pass it back in astNode *)
      astNode := AST.NewNode(AstNodeType.PCall, desig, exprList)
  
  ELSE (* sole designator *)
    astNode := desig
  END; (* CASE *)
  
  RETURN lookahead
END updateOrProcCall;


(* --------------------------------------------------------------------------
 * private function returnStatement(astNode)
 * --------------------------------------------------------------------------
 * Parses rule returnStatement, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * returnStatement :=
 *   RETURN expression?
 *   ;
 *
 * astNode: (RETURN exprNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE returnStatement ( VAR astNode : AstT ) : SymbolT;

VAR
  expr : AstT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("returnStatement");
  
  (* RETURN *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* expression? *)
  IF inFIRST(Expression, lookahead.token) THEN
    lookahead := expression(expr)
  ELSE
    expr := AST.emptyNode()
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.Return, expr);
  
  RETURN lookahead
END returnStatement;


(* --------------------------------------------------------------------------
 * private function ifStatement(astNode)
 * --------------------------------------------------------------------------
 * Parses rule ifStatement, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * ifStatement :=
 *   IF boolExpression THEN statementSequence
 *   ( ELSIF boolExpression THEN statementSequence )*
 *   ( ELSE statementSequence )?
 *   END
 *   ;
 *
 * alias boolExpression = expression ;
 *
 * astNode: (IF exprNode stmtSeqNode elsifSeqNode stmtSeqNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE ifStatement ( VAR astNode : AstT ) : SymbolT;

VAR
  ifExpr, ifStmtSeq, elif, elifSeq, expr, stmtSeq : AstT;
  tmplist : AstQueueT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("ifStatement");
  
  AstQueue.New(tmplist);
  
  (* IF *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* boolExpression *)
  IF matchSet(FIRST(Expression)) THEN
    lookahead := expression(expr)
  ELSE (* resync *)
    lookahead :=
      skipToMatchTokenOrSet(Token.Then, FIRST(StatementSequence))
  END; (* IF *)
  
  (* THEN *)
  IF matchToken(Token.Then) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FIRST(StatementSequence))
  END; (*IF *)
  
  (* statementSequence *)
  IF matchSet(FIRST(StatementSequence)) THEN
    lookahead := statementSequence(stmtSeq)
  ELSE (* resync *)
    lookahead :=
      skipToMatchTokenOrSet(Token.Elsif, FIRST(Expression))
  END; (* IF *)
  
  (* ( ELSIF boolExpression THEN statementSequence )* *)
  WHILE lookahead.token = Token.Elsif DO
    
    (* ELSIF *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* boolExpression *)
    IF matchSet(FIRST(Expression)) THEN
      lookahead := expression(expr)
    ELSE (* resync *)
      lookahead :=
        skipToMatchTokenOrSet(Token.Then, FIRST(StatementSequence))
    END; (* IF *)
    
    (* THEN *)
    IF matchToken(Token.Then) THEN
      lookahead := Lexer.consumeSym(lexer)
    ELSE (* resync *)
      lookahead := skipToMatchSet(FIRST(StatementSequence))
    END; (*IF *)
    
    (* statementSequence *)
    IF matchSet(FIRST(StatementSequence)) THEN
      lookahead := statementSequence(stmtSeq)
    ELSE (* resync *)
      lookahead := skipToMatchTokenOrTokenOrSet
        (Token.Elsif, Token.End, FOLLOW(IfStatement))
    END; (* IF *)
    
    elif := AST.NewNode(AstNodeType.Elif, expr, stmtSeq)
    AstQueue.Enqueue(tmplist, elif)
  END; (* WHILE *)
  
  elifSeq := AST.NewListNode(AstNodeType.ElifSeq, tmplist);
  
  (* ( ELSE statementSequence )? *)
  IF lookahead.token = Token.Else THEN
    (* ELSE *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* statementSequence *)
    IF matchSet(FIRST(StatementSequence)) THEN
      lookahead := statementSequence(stmtSeq)
    ELSE (* resync *)
      lookahead := skipToMatchTokenOrSet(Token.End, FOLLOW(IfStatement))
    END (* IF *)
    
  ELSE (* no ELSE branch *)
    stmtSeq := AST.emptyNode()
  END; (* IF *)
  
  (* END *)
  IF matchToken(Token.End) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(IfStatement))
  END; (*IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode :=
    AST.NewNode(AstNodeType.IfStmt, ifExpr, ifStmtSeq, elifSeq, stmtSeq);
  AstQueue.Release(tmplist);
  
  RETURN lookahead
END ifStatement;


(* --------------------------------------------------------------------------
 * private function caseStatement(astNode)
 * --------------------------------------------------------------------------
 * Parses rule caseStatement, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * caseStatement :=
 *   CASE expression OF ( '|' case )+ ( ELSE statementSequece )? END
 *   ;
 *
 * astNode: (SWITCH exprNode caseListNode stmtSeqNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE caseStatement ( VAR astNode : AstT ) : SymbolT;

VAR
  expr, caseNode, caseList, stmtSeq : AstT;
  tmplist : AstQueueT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("caseStatement");
  
  AstQueue.New(tmplist);
  
  (* CASE *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* expression *)
  IF matchSet(FIRST(Expression)) THEN
    lookahead := expression(expr)
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrToken(Token.Of, Token.VerticalBar)
  END; (* IF *)
  
  (* OF *)
  IF matchToken(Token.Of) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrSet(Token.VerticalBar, FIRST(Case))
  END; (*IF *)
  
  (* ( '|' case )+ *)
  REPEAT
    (* '|' *)
    IF matchToken(Token.VerticalBar) THEN
      lookahead := Lexer.consumeSym(lexer);
    ELSE (* resync *)
      lookahead := skipToMatchSet(FIRST(Case))
    END; (* IF *)
    
    (* case *)
    IF matchSet(FIRST(Case)) THEN
      lookahead := case(caseNode);
      AstQueue.Enqueue(tmplist, caseNode)
      
    ELSE (* resync *)
      lookahead := skipToMatchTokenOrTokenOrSet
        (Token.VerticalBar, Token.Else, FOLLOW(CaseStatement))
    END (* IF *)
  UNTIL lookahead.token # Token.VerticalBar;
  
  (* ( ELSE statementSequence )? *)
  IF lookahead.token = Token.Else THEN
    (* ELSE *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* statementSequence *)
    IF matchSet(FIRST(StatementSequence)) THEN
      lookahead := statementSequence(stmtSeq)
    ELSE (* resync *)
      lookahead := skipToMatchTokenOrSet(Token.End, FOLLOW(CaseStatement))
    END (* IF *)
    
  ELSE (* no ELSE branch *)
    stmtSeq := AST.emptyNode()
  END; (* IF *)
  
  (* END *)
  IF matchToken(Token.End) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(CaseStatement))
  END; (*IF *)
  
  (* build AST node and pass it back in astNode *)
  caseList := AST.NewListNode(AstNodeType.CList, tmplist);
  astNode := AST.NewNode(AstNodeType.Switch, expr, caseList, stmtSeq);
  AstQueue.Release(tmplist);
  
  RETURN lookahead
END caseStatement;


(* --------------------------------------------------------------------------
 * private function case(astNode)
 * --------------------------------------------------------------------------
 * Parses rule case, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * case :=
 *   caseLabels ( ',' caseLabels )* : statementSequence
 *   ;
 *
 * astNode: (CASE caseLabelListNode stmtSeqNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE case ( VAR astNode : AstT ) : SymbolT;

VAR
  cllist, stmtSeq : AstT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("case");  
  
  (* caseLabels ( ',' caseLabels )* *)
  lookahead :=
    parseListWSeparator(caseLabels, Token.Comma,
      FIRST(CaseLabels), FOLLOW(CaseLabels),
      AstTypeNode.ClabelList, cllist);
    
  (* ':' *)
  IF matchToken(Token.Colon) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FIRST(StatementSequence))
  END; (*IF *)
  
  (* statementSequence *)
  IF matchSet(FIRST(StatementSequence)) THEN
    lookahead := statementSequence(stmtSeq)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FIRST(StatementSequence))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.Case, cllist, stmtSeq);
  AstQueue.Release(tmplist);
  
  RETURN lookahead
END case;


(* --------------------------------------------------------------------------
 * private function caseLabels(astNode)
 * --------------------------------------------------------------------------
 * Parses rule caseLabels, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * caseLabels :=
 *   constExpression ( .. constExpression )?
 *   ;
 *
 * astNode: (CLABELS exprNode exprNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE caseLabels ( VAR astNode : AstT ) : SymbolT;

VAR
  expr1, expr2 : AstT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("caseLabels");
  
  (* constExpression *)
  lookahead := expression(expr1);
  
  (* ( .. constExpression )? *)
  IF lookahead.token = Token.DotDot THEN
    (* '..' *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* constExpression *)
    IF matchSet(FIRST(Expression)) THEN
      lookahead := expression(expr2)
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(caseLabels))
    END (* IF *)
    
  ELSE (* sole case label *)
    expr2 := AST.emptyNode()
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.CLabels, expr1, expr2);
  
  RETURN lookahead
END caseLabels;


(* --------------------------------------------------------------------------
 * private function loopStatement(astNode)
 * --------------------------------------------------------------------------
 * Parses rule loopStatement, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * loopStatement :=
 *   LOOP statementSequence END
 *   ;
 *
 * astNode: (LOOP stmtSeqNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE loopStatement ( VAR astNode : AstT ) : SymbolT;

VAR
  stmtSeq : AstT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("loopStatement");
  
  (* LOOP *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* statementSequence *)
  IF matchSet(FIRST(StatementSequence)) THEN
    lookahead := statementSequence(stmtSeq)
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrSet(Token.End, FOLLOW(LoopStatement))
  END; (* IF *)
  
  (* END *)
  IF matchToken(Token.End) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(LoopStatement))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.Loop, stmtSeq);
  
  RETURN lookahead
END loopStatement;


(* --------------------------------------------------------------------------
 * private function whileStatement(astNode)
 * --------------------------------------------------------------------------
 * Parses rule whileStatement, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * whileStatement :=
 *   WHILE boolExpression DO statementSequence END
 *   ;
 *
 * astNode: (WHILE exprNode stmtSeqNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE whileStatement ( VAR astNode : AstT ) : SymbolT;

VAR
  expr, stmtSeq : AstT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("whileStatement");
  
  (* WHILE *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* boolExpression *)
  IF matchSet(FIRST(Expression)) THEN
    lookahead := expression(expr)
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrSet(Token.Do, FIRST(StatementSequence))
  END; (* IF *)
  
  (* DO *)
  IF matchToken(Token.Do) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FIRST(StatementSequence))
  END; (* IF *)
  
  (* statementSequence *)
  IF matchSet(FIRST(StatementSequence)) THEN
    lookahead := statementSequence(stmtSeq)
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrSet(Token.End, FOLLOW(WhileStatement))
  END; (* IF *)
  
  (* END *)
  IF matchToken(Token.End) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(WhileStatement))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.While, expr, stmtSeq);
  
  RETURN lookahead
END whileStatement;


(* --------------------------------------------------------------------------
 * private function repeatStatement(astNode)
 * --------------------------------------------------------------------------
 * Parses rule repeatStatement, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * repeatStatement :=
 *   REPEAT statementSequence UNITL boolExpression
 *   ;
 *
 * astNode: (REPEAT stmtSeqNode exprNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE repeatStatement ( VAR astNode : AstT ) : SymbolT;

VAR
  expr, stmtSeq : AstT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("repeatStatement");
  
  (* REPEAT *)
  lookahead := Lexer.consumeSym(lexer);
    
  (* statementSequence *)
  IF matchSet(FIRST(StatementSequence)) THEN
    lookahead := statementSequence(stmtSeq)
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrSet(Token.Until, FIRST(Expression))
  END; (* IF *)
  
  (* UNTIL *)
  IF matchToken(Token.Until) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FIRST(Expression))
  END; (* IF *)
  
  (* boolExpression *)
  IF matchSet(FIRST(Expression)) THEN
    lookahead := expression(expr)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(RepeatStatement))
  END; (* IF *)
    
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.Repeat, stmtSeq, expr);
  
  RETURN lookahead
END repeatStatement;


(* --------------------------------------------------------------------------
 * private function forStatement(astNode)
 * --------------------------------------------------------------------------
 * Parses rule forStatement, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * forStatement :=
 *   FOR forLoopVariants IN iterableExpr DO statementSequence END
 *   ;
 *
 * astNode: (FOR loopVarNode iterExprNode stmtSeqNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE forStatement ( VAR astNode : AstT ) : SymbolT;

VAR
  loopVariants, iterExpr, stmtSeq : AstT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("forStatement");
  
  (* FOR *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* forLoopVariants *)
  IF matchSet(FIRST(ForLoopVariants)) THEN
    lookahead := forLoopVariants(loopVariants)
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrSet(Token.In, FIRST(IterableExpr))
  END; (* IF *)
  
  (* IN *)
  IF matchToken(Token.In) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FIRST(IterableExpr))
  END; (* IF *)
  
  (* iterableExpr *)
  IF matchSet(FIRST(IterableExpr)) THEN
    lookahead := iterableExpr(iterExpr)
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrSet(Token.Do, FIRST(StatementSequence))
  END; (* IF *)
  
  (* DO *)
  IF matchToken(Token.Do) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FIRST(StatementSequence))
  END; (* IF *)
  
  (* statementSequence *)
  IF matchSet(FIRST(StatementSequence)) THEN
    lookahead := statementSequence(stmtSeq)
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrSet(Token.End, FOLLOW(ForStatement))
  END; (* IF *)
  
  (* END *)
  IF matchToken(Token.End) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(ForStatement))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.For, loopVariants, iterExpr, stmtSeq);
  
  RETURN lookahead
END forStatement;


(* --------------------------------------------------------------------------
 * private function forLoopVariants(astNode)
 * --------------------------------------------------------------------------
 * Parses rule forLoopVariants, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * forLoopVariants :=
 *   accessor ascOrDesc? ( ',' value )?
 *   ;
 *
 * alias accessor = ident ;
 *
 * alias value = ident ;
 *
 * alias ascOrDesc = IncOrDecSuffix ;
 *
 * astNode: (FLV identNode ascOrDescNode identNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE forLoopVariants ( VAR astNode : AstT ) : SymbolT;

VAR
  accessor, value : AstT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("forLoopVariants");
  
  (* accessor *)
  lookahead := ident(accessor);
  
  (* ascOrDesc? *)
  CASE lookahead.token OF
  (* '++' *)
    Token.PlusPlus : accessor := AST.NewNode(AstNodeType.Asc, accessor)
      
  (* '--' *)
  | Token.MinusMinus : accessor := AST.NewNode(AstNodeType.Desc, accessor)
  END; (* CASE *)
  
  (* ( ',' value )? *)
  IF matchToken(Token.Comma) THEN
    (* ',' *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* value *)
    IF matchSet(FIRST(Ident)) THEN
      lookahead := ident(value)
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(forLoopVariants))
    END (* IF *)
    
  ELSE (* no value given *)
    value := AST.emptyNode()
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.FLV, accessor, value);
  
  RETURN lookahead
END forLoopVariants;


(* --------------------------------------------------------------------------
 * private function iterableExpr(astNode)
 * --------------------------------------------------------------------------
 * Parses rule iterableExpr, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * iterableExpr :=
 *   rangeOfOrdinalType | designator
 *   ;
 *
 * astNode: desigNode | rangeNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE iterableExpr ( VAR astNode : AstT ) : SymbolT;

VAR
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("iterableExpr");
  
  (* ordinalRange OF ordinalType | *)
  IF lookahead.token = Token.LeftBracket THEN
    lookahead := rangeOfOrdinalType(astNode)
  ELSE (* designator *)
    lookahead := designator(astNode)
  END; (* IF *)
  
  RETURN lookahead
END iterableExpr;


(* --------------------------------------------------------------------------
 * private function designator(astNode)
 * --------------------------------------------------------------------------
 * Parses rule designator, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * designator :=
 *   ident designatorTail?
 *   ;
 *
 * astNode: (DESIG head tail)
 * --------------------------------------------------------------------------
 *)
PROCEDURE designator ( VAR astNode : AstT ) : SymbolT;

VAR
  head, tail : AstT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("designator");
  
  (* ident *)
  lookahead := ident(head);
    
  (* designatorTail? *)
  IF inFIRST(DesignatorTail) THEN
    lookahead := designatorTail(head, tail)
  ELSE
    tail := AST.emptyNode()
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.Desig, head, tail);
  
  RETURN lookahead
END designator;


(* --------------------------------------------------------------------------
 * private function designatorTail(astNode)
 * --------------------------------------------------------------------------
 * Parses rule designatorTail, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * designatorTail :=
 *   ( deref | fieldSelector ) designatorTail? | subscriptOrSlice
 *   ;
 *
 * alias deref = '^' ;
 *
 * fieldSelector :=
 *   '.' ident
 *   ;
 *
 * astNode: (DESIG )
 * --------------------------------------------------------------------------
 *)
PROCEDURE designatorTail ( head : AstT; VAR astNode : AstT ) : SymbolT;

VAR
  deref, fieldId, field : AstT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("designatorTail");
  
  (* deref *)
  IF lookahead.token = Token.Caret THEN
    (* '^' *)
    lookahead := Lexer.consumeSym(lexer);
    deref := AST.NewNode(AstTokenType.Deref, head);
    
    (* designatorTail? *)
    IF inFIRST(DesignatorTail, lookahead.token) THEN
      lookahead := designatorTail(deref, astNode)
    ELSE
      astNode := deref
    END (* IF *)
      
  (* fieldSelector *)
  ELSIF lookahead.token = Token.Dot THEN
    (* '.' *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* ident *)
    lookahead := ident(fieldId);
    
    field := AST.NewNode(AstTokenType.Field, fieldId);
    
    (* designatorTail? *)
    IF inFIRST(DesignatorTail, lookahead.token) THEN
      lookahead := designatorTail(field, astNode)
    ELSE
      astNode := field
    END (* IF *)
    
  (* subscriptOrSlice *)
  ELSE
    lookahead := subscriptOrSlice(astNode)
  END; (* IF *)
  
  RETURN lookahead
END designatorTail;


(* --------------------------------------------------------------------------
 * private function subscriptOrSlice(astNode)
 * --------------------------------------------------------------------------
 * Parses rule subscriptOrSlice, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * subscriptOrSlice :=
 *   '[' expression ( ']' designatorTail? | '..' expression? ']' )
 *   ;
 *
 * astNode: (DESIG head tail)
 * --------------------------------------------------------------------------
 *)
PROCEDURE subscriptOrSlice ( VAR astNode : AstT ) : SymbolT;

VAR
  expr1, expr2, subscript : AstT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("designator");
  
  (* '[' *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* expression *)
  IF matchSet(FIRST(Expression)) THEN
    lookahead := expression(expr1)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FIRST(SubscriptOrSliceTail))
  END; (* IF *)
  
  (* ']' designatorTail? | '..' expression? ']' *)
  IF matchSet(FIRST(SubscriptOrSliceTail)) THEN
    (* ']' designatorTail? | *)
    IF lookahead.token = Token.LeftBracket THEN
      (* ']' *)
      lookahead := Lexer.consumeSym(lexer);
      subscript := AST.NewNode(AstNodeType.Index, expr1);
      
      (* designatorTail? *)
      IF inFIRST(designatorTail, lookahead.token) THEN
        lookahead := designatorTail(subscript, astNode)
      ELSE
         astNode := subscript
      END (* IF *)
    
    (* '..' expression? ']' *)
    ELSE
      (* '..' *)
      lookahead := Lexer.consumeSym(lexer);
      
      (* expression? *)
      IF inFIRST(expression, lookahead.token) THEN
        lookahead := expression(expr2)
      ELSE (* open ended slice *)
        expr2 := AST.emptyNode()
      END (* IF *)
      
      (* build AST node and pass it back in astNode *)
      astNode := AST.NewNode(AstNodeType.Slice, expr1, expr2)
    END (* IF *)
    
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(SubscriptOrSlice))
  END; (* IF *)
  
  RETURN lookahead
END subscriptOrSlice;


(* --------------------------------------------------------------------------
 * private function expressionList(astNode)
 * --------------------------------------------------------------------------
 * Parses rule expressionList, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * expressionList :=
 *   expression ( ',' expression )*
 *   ;
 *
 * astNode: (EXPRLIST exprNode1 exprNode2 ... exprNodeN)
 * --------------------------------------------------------------------------
 *)
PROCEDURE expressionList ( VAR astNode : AstT ) : SymbolT;

VAR
  expr : AstT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("expressionList");
  
  (* expression ( ',' expression )* *)
  lookahead :=
    parseListWSeparator(expression, Token.Comma,
      FIRST(Expression), FOLLOW(Expression),
      AstTypeNode.ExprList, astNode);
  
  RETURN lookahead
END expressionList;


(* --------------------------------------------------------------------------
 * Expression Trees
 * --------------------------------------------------------------------------
 * An expression  or  sub-expression  in which  all operators  have  the same
 * precedence level, such as
 *
 *   a + b + c + d
 *
 * is evaluated  from left to right  and is thus correctly represented by the
 * expression tree
 *
 *         +
 *        / \
 *       +   d
 *      / \
 *     +   c
 *    / \
 *   a   b
 *
 * In order to construct this expression tree,  the most recently constructed
 * expression node  always  becomes the  left subnode  of the expression node
 * that is to be constructed next.
 *
 *   IF matchSet(FIRST(Term)) THEN
 *     lookahead := term(leftNode);
 *     
 *     WHILE inFIRST(OperL2) DO
 *       nodeType := NodeTypeForOper(lookahead.token);
 *       lookahead := Lexer.consumeSym(lexer);
 *       
 *       IF matchSet(FIRST(term)) THEN
 *         lookahead := term(rightNode);
 *         leftNode := AST.NewBinaryNode(nodeType, leftNode, rightNode)
 *       END
 *     END
 *   END
 * --------------------------------------------------------------------------
 *)


(* --------------------------------------------------------------------------
 * private function expression(astNode)
 * --------------------------------------------------------------------------
 * Parses rule expression, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * expression :=
 *   simpleExpression ( OperL1 simpleExpression )?
 *   ;
 *
 * OperL1 :=
 *   '=' | '#' | '<' | '<=' | '>' | '>=' | IN
 *   ;
 *
 * astNode: eqNode | neqNode | ltNode | ltEqNode | gtNode | gtEqNode | inNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE expression ( VAR astNode : AstT ) : SymbolT;

VAR
  leftNode, rightNode : AstT;
  nodeType : AstNodeTypeT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("expression");
    
  (* simpleExpression *)
  lookahead := simpleExpression(leftNode);
  
  (* ( OperL1 simpleExpression )? *)
  IF inFIRST(OperL1) DO
    (* OperL1 *)
    nodeType := AstNodeType.NodeTypeForOper(lookahead.token);
    lookahead := Lexer.consumeSym(lexer);
    
    (* simpleExpression *)
    IF matchSet(FIRST(SimpleExpression)) THEN
      lookahead := simpleExpression(rightNode)
      
    ELSE (* resync *)
      lookahead :=
        skipToMatchTokenOrSet(Token.Comma), FOLLOW(Expression))
    END; (* IF *)
    
    (* construct new node from previous and last leaf nodes *)
    leftNode := AST.NewNode(nodeType, leftNode, rightNode)
    
  END; (* IF *)
  
  (* pass leftNode back in astNode *)
  astNode := leftNode;
  
  RETURN lookahead
END expression;


(* --------------------------------------------------------------------------
 * private function simpleExpression(astNode)
 * --------------------------------------------------------------------------
 * Parses rule simpleExpression, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * simpleExpression :=
 *   term ( OperL2 term )* | '-' simpleFactor
 *   ;
 *
 * OperL2 :=
 *   '+' | '-' | OR | ConcatOp | SetDiffOp
 *   ;
 *
 * alias ConcatOp = '&' ;
 *
 * alias SetDiffOp = '\' ;
 *
 * astNode:
 *  plusNode | minusNode | orNode | concatNode | setDiffNode | negNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE simpleExpression ( VAR astNode : AstT ) : SymbolT;

VAR
  leftNode, rightNode : AstT;
  nodeType : AstNodeTypeT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("term");
    
  (* '-' simpleFactor | *)
  IF lookahead.token = Token.Minus THEN
    (* '-' *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* simpleFactor *)
    IF matchSet(FIRST(SimpleFactor)) THEN
      lookahead := simpleFactor(leftNode);
      leftNode := AST.NewNode(AstNodeType.Neg, leftNode)
      
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(SimpleExpression))
    END (* IF *)
  
  ELSE (* term ( OperL2 term )* *)
    (* term *)
    lookahead := term(leftNode);
    
    (* ( OperL2 term )* *)
    WHILE inFIRST(OperL2) DO
      (* OperL2 *)
      nodeType := AstNodeType.NodeTypeForOper(lookahead.token);
      lookahead := Lexer.consumeSym(lexer);
      
      (* term *)
      IF matchSet(FIRST(term)) THEN
        lookahead := term(rightNode)
      ELSE (* resync *)
        lookahead :=
          skipToMatchSetOrSet(FIRST(OperL2), FOLLOW(SimpleExpression))
      END; (* IF *)
      
      (* construct new node from previous and last leaf nodes *)
      leftNode := AST.NewNode(nodeType, leftNode, rightNode)
    END (* WHILE *)
  END (* IF *)
    
  (* pass leftNode back in astNode *)
  astNode := leftNode;
  
  RETURN lookahead
END simpleExpression;


(* --------------------------------------------------------------------------
 * private function term(astNode)
 * --------------------------------------------------------------------------
 * Parses rule term, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * term :=
 *   simpleTerm ( OperL3 simpleTerm )*
 *   ;
 *
 * OperL3 :=
 *   '*' | '/' | DIV | MOD | AND
 *   ;
 *
 * astNode: starNode | slashNode | divNode | modNode | andNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE term ( VAR astNode : AstT ) : SymbolT;

VAR
  leftNode, rightNode : AstT;
  nodeType : AstNodeTypeT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("term");
    
  (* simpleTerm *)
  lookahead := simpleTerm(leftNode);
  
  (* ( OperL3 simpleTerm )* *)
  WHILE inFIRST(OperL3) DO
    (* OperL3 *)
    nodeType := AstNodeType.NodeTypeForOper(lookahead.token);
    lookahead := Lexer.consumeSym(lexer);
    
    (* simpleTerm *)
    IF matchSet(FIRST(SimpleTerm)) THEN
      lookahead := simpleTerm(rightNode)
    ELSE (* resync *)
      lookahead := skipToMatchSetOrSet(FIRST(OperL3), FOLLOW(Term))
    END; (* IF *)
    
    (* construct new node from previous and last leaf nodes *)
    leftNode := AST.NewNode(nodeType, leftNode, rightNode)
  END; (* WHILE *)
  
  (* pass leftNode back in astNode *)
  astNode := leftNode;
  
  RETURN lookahead
END term;


(* --------------------------------------------------------------------------
 * private function simpleTerm(astNode)
 * --------------------------------------------------------------------------
 * Parses rule simpleTerm, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * simpleTerm :=
 *   NOT? factor
 *   ;
 *
 * astNode: (NOT exprNode) | factorNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE simpleTerm ( VAR astNode : AstT ) : SymbolT;

VAR
  seenNot : BOOLEAN;
  factorNode : AstT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("simpleTerm");
  
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* NOT? *)
  IF lookahead.token = Token.Not THEN
    lookahead := Lexer.consumeSym(lexer);
    seenNot := TRUE
  ELSE
    seenNot := FALSE
  END; (* IF *)
  
  (* factor *)
  IF matchSet(FIRST(Factor)) THEN
    lookahead := factor(factorNode)
  ELSE
    lookahead := skipToMatchSet(FOLLOW(simpleTerm))
  END;
  
  (* build AST node and pass it back in astNode *)
  IF seenNot THEN
    astNode := AST.NewNode(AstNodeType.Not, factorNode)
  ELSE
    astNode := factorNode
  END;
  
  RETURN lookahead
END simpleTerm;


(* --------------------------------------------------------------------------
 * private function factor(astNode)
 * --------------------------------------------------------------------------
 * Parses rule factor, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * factor :=
 *   simpleFactor ( TypeConvOp typeIdent )?
 *   ;
 *
 * alias TypeConvOp = '::' ;
 *
 * astNode: (CONV exprNode qualidentNode) | simpleFactorNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE factor ( VAR astNode : AstT ) : SymbolT;

VAR
  leftNode, rightNode : AstT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("factor");
    
  (* simpleFactor *)
  lookahead := simpleFactor(leftNode);
  
  (* ( TypeConvOp typeIdent )? *)
  IF lookahead.token = Token.DoubleColon DO
    (* '::' *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* simpleFactor *)
    IF matchSet(FIRST(SimpleFactor)) THEN
      lookahead := simpleFactor(rightNode)
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(SimpleFactor))
    END; (* IF *)
    
    (* construct new node from left and right leaf nodes *)
    leftNode := AST.NewNode(AstNodeType.TypeConv, leftNode, rightNode)
  END; (* IF *)
  
  (* pass leftNode back in astNode *)
  astNode := leftNode;
  
  RETURN lookahead
END factor;


(* --------------------------------------------------------------------------
 * private function simpleFactor(astNode)
 * --------------------------------------------------------------------------
 * Parses rule simpleFactor, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * simpleFactor :=
 *   NumberLiteral | StringLiteral |
 *   structuredValue | designatorOrFuncCall | '(' expression ')'
 *   ;
 *
 * astNode:
 *  intValNode | quotedValNode | structValNode |
 *  desigNode | fcallNode | exprNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE simpleFactor ( VAR astNode : AstT ) : SymbolT;

VAR
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("factor");
  
  lookahead := Lexer.lookaheadSym(lexer);
  
  CASE lookahead.token OF
    (* NumberLiteral *)
    Token.NumberLiteral :
      astNode := AST.NewTerminalNode(AstNodeType.IntVal, lookahead.lexeme);
      lookahead := Lexer.consumeSym(lexer)
      
      (* TO DO: real and character code values *)
      
    (* StringLiteral *)
  | Token.StringLiteral :
      astNode := AST.NewTerminalNode(AstNodeType.QuotedVal, lookahead.lexeme);
      lookahead := Lexer.consumeSym(lexer)
  
    (* structuredValue *)
  | Token.LeftBrace :
      lookahead := structuredValue(astNode)
  
    (* '(' expression ')' *)
  | Token.LeftParen :
      (* '(' *)
      lookahead := Lexer.consumeSym(lexer);
      
      (* expression *)
      IF matchSet(FIRST(Expression)) THEN
        lookahead := expression(astNode)
      ELSE (* resync *)
        lookahead :=
          skipToMatchTokenOrSet(Token.RightParen, FOLLOW(simpleFactor))
      END; (* IF *)
      
      (* ')' *)
      IF matchToken(Token.RightParen) THEN
        lookahead := Lexer.consumeSym(lexer)
      ELSE
        lookahead := skipToMatchSet(FOLLOW(simpleFactor))
      END (* IF *)
      
  ELSE (* designatorOrFuncCall *)
    lookahead := designatorOrFuncCall(astNode)
  END; (* CASE *)
  
  RETURN lookahead
END simpleFactor;


(* --------------------------------------------------------------------------
 * private function designatorOrFuncCall(astNode)
 * --------------------------------------------------------------------------
 * Parses rule designatorOrFuncCall, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * designatorOrFuncCall :=
 *   designator ( '(' expressionList? ')' )?
 *   ;
 *
 * astNode:
 *  desigNode | (FCALL desigNode exprListNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE designatorOrFuncCall ( VAR astNode : AstT ) : SymbolT;

VAR
  desig, exprList : AstT;
  lookahead := SymbolT;

BEGIN
  PARSER_DEBUG_INFO("designatorOrFuncCall");
  
  (* designator *)
  lookahead := designator(desig);
  
  (* ( '(' expressionList? ')' )? *)
  IF lookahead.token = Token.LeftParen THEN
    (* '(' *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* expressionList? *)
    IF inFIRST(ExpressionList, lookahead.token) THEN
      lookahead := expressionList(exprList)
    ELSE
      exprList := AST.emptyNode()
    END; (* IF *)
    
    (* ')' *)
    IF matchToken(Token.RightParen) THEN
      lookahead := Lexer.consumeSym(lexer)
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(ExpressionList))
    END; (* IF *)
    
    (* construct function call node *)
    astNode := AST.NewNode(AstNodeType.FCall, desig, exprList)
    
  ELSE (* sole designator *)
    astNode := desig
  END; (* IF *)
  
  RETURN lookahead
END designatorOrFuncCall;


(* --------------------------------------------------------------------------
 * private function structuredValue(astNode)
 * --------------------------------------------------------------------------
 * Parses rule structuredValue, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * structuredValue :=
 *   '{' valueComponent ( ',' valueComponent )* '}'
 *   ;
 *
 * astNode: (STRUCTVAL exprListNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE structuredValue ( VAR astNode : AstT ) : SymbolT;

VAR
  lookahead := SymbolT;

BEGIN
  PARSER_DEBUG_INFO("structuredValue");
  
  AstQueue.New(tmplist);
  
  (* '{' *)
  lookahead := Lexer.consumeSym(lexer);
    
  (* valueComponent *)
  lookahead := valueComponent(value);
  AstQueue.Enqueue(tmplist, value);
  
  (* ( ';' valueComponent )* *)
  WHILE lookahead.token = Token.Comma DO
    (* ',' *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* valueComponent *)
    IF matchSet(FIRST(ValueComponent)) THEN
      lookahead := valueComponent(value);
      AstQueue.Enqueue(tmplist, value)
      
    ELSE (* resync *)
      lookahead := skipToMatchTokenOrSet(Token.Comma), FOLLOW(ValueComponent))
    END (* IF *)
  END; (* WHILE *)
  
  (* '}' *)
  IF matchToken(Token.RightBrace) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(StructuredValue))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.StructVal, tmplist);
  AstQueue.Release(tmplist);
  
  RETURN lookahead
END structuredValue;


(* --------------------------------------------------------------------------
 * private function valueComponent(astNode)
 * --------------------------------------------------------------------------
 * Parses rule valueComponent, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * valueComponent :=
 *   constExpression ( '..' constExpression )? |
 *   runtimeExpression
 *   ;
 *
 * alias runtimeExpression = expression ;
 *
 * astNode: expr | (VALRANGE expr expr)
 * --------------------------------------------------------------------------
 *)
PROCEDURE valueComponent ( VAR astNode : AstT ) : SymbolT;

VAR
  leftNode, rightNode : AstT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("valueComponent");
    
  (* constExpression *)
  lookahead := expression(leftNode);
  
  (* ( .. constExpression )? *)
  IF lookahead.token = Token.DotDot DO
    (* '..' *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* constExpression *)
    IF matchSet(FIRST(Expression)) THEN
      lookahead := expression(rightNode)
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(Expression))
    END; (* IF *)
    
    (* construct new node from left and right leaf nodes *)
    leftNode := AST.NewNode(AstNodeType.ValRange, leftNode, rightNode)
  END; (* IF *)
  
  (* pass leftNode back in astNode *)
  astNode := leftNode;
  
  RETURN lookahead
END valueComponent;


END Parser.