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


(* Parser context *)

VAR
  lexer : LexerT;
  stats : Statistics;


(* Operations *)

PROCEDURE parseDef
  ( source : StringT; VAR stats : Statistics; VAR status : Status ) : AstT;
(* Parses .def source file, returns AST on success, NIL on failure. *)

BEGIN

END parseDef;


PROCEDURE parseMod
  ( source : StringT; VAR stats : Statistics; VAR status : Status ) : AstT;
(* Parses .mod source file, returns AST on success, NIL on failure. *)

BEGIN

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
  
  (* check if lookahead matches any token in expected_set *)
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
 * private function skipToMatchToken(resyncToken)
 * --------------------------------------------------------------------------
 * Cconsumes symbols until the lookahead symbol's token matches the given
 * resync token and returns the new lookahead symbol.
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
 * private function skipToMatchSet(resyncSet)
 * --------------------------------------------------------------------------
 * Cconsumes symbols until the lookahead symbol's token matches one of the
 * tokens in the given resync set and returns the new lookahead symbol.
 * --------------------------------------------------------------------------
 *)
PROCEDURE skipToMatchSet ( resyncSet : TokenSetT ) : SymbolT;

VAR
 lookahead : SymbolT;
 
BEGIN

  lookahead := Lexer.lookaheadSym(lexer);
  
  (* skip symbols until lookahead matches resyncSet *)
  WHILE NOT TokenSet.isElement(resyncSet, lookahead.token) DO
    lookahead = Lexer.consumeSym(lexer)
  END; (* WHILE *)
  
  RETURN lookahead
END skipToMatchSet;


(* --------------------------------------------------------------------------
 * private function skipToMatchTokenOrSet(resyncToken, resyncSet)
 * --------------------------------------------------------------------------
 * Cconsumes symbols until the lookahead symbol's token matches one of the
 * tokens in the given resync set and returns the new lookahead symbol.
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
    (NOT TokenSet.isElement(resyncSet, lookahead.token)) DO
    lookahead = Lexer.consumeSym(lexer)
  END; (* WHILE *)
  
  RETURN lookahead
END skipToMatchTokenOrSet;



(* ************************************************************************ *
 * Syntax Analysis                                                          *
 * ************************************************************************ *)

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
    Token.Definition :
      lookahead := definitionModule(astNode)
      
  | Token.Implementation,
    Token.Module :
      lookahead := implOrPrgmModule(astNode)
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
    lookahead := Lexer.consumeSym(lexer);
    
    (* moduleIdent *)
    IF matchToken(Token.StdIdent) THEN
      ident1 = lookahead.lexeme;
      lookahead := Lexer.consumeSym(lexer);
      
      (* ';' *)
      IF matchToken(Token.Semicolon) THEN
        lookahead := Lexer.consumeSym(lexer)
      ELSE (* resync *)
        lookahead := skipToMatchSet(FOLLOW(Import))
      END (* IF *)
      
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(Import))
    END (* IF *)
    
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(Import))
  END; (* IF *)
  
  tmplist := AstQueue.New();

  (* import* *)
  WHILE lookahead.token = Token.Import DO
    lookahead := import(implist);
    AstQueue.Enqueue(tmplist, implist)
  END (* WHILE *)
  
  implist := AST.NewListNode(AstNodeType.ImpList, tmplist);
  tmplist:= AstQueue.ResetQueue(tmplist);
  
  (* definition* *)
  while lookahead.token = Token.Const OR
        lookahead.token = Token.Type OR
        lookahead.token = Token.Var OR
        lookahead.token = Token.Procedure DO
    lookahead := definition(deflist);
    AstQueue.Enqueue(tmplist, deflist)
  END (* WHILE *)
  
  deflist := AST.NewListNode(AstNodeType.DefList, tmplist);
  tmplist := AstQueue.ResetQueue(tmplist);
  
  (* END *)
  IF matchToken(Token.End) THEN
    lookahead := Lexer.consumeSym(lexer);
    
    (* moduleIdent *)
    IF matchToken(Token.StdIdent) THEN
      ident2 := lookahead.lexeme;
      lookahead := Lexer.ConsumeSym(lexer);
    
      IF ident1 # ident2 THEN
        (* TO DO: report error -- module identifiers don't match *) 
      END; (* IF *)
    
      (* '.' *)
      IF matchToken(Token.Period, FOLLOW(DefinitionModule)) THEN
        lookahead := Lexer.consumeSym(lexer)
      ELSE (* resync *)
        lookahead := skipToMatchSet(FOLLOW(DefinitionModule))
      END (* IF *)
      
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(DefinitionModule))
    END (* IF *)
    
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
  libId, implist : AstT;
  tmplist : AstQueueT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("import");

  (* IMPORT *)
  lookahead := Lexer.consumeSym(lexer);
  
  templist := LexQueue.New();
  
  (* libIdent *)
  IF matchToken(Token.StdIdent) THEN
    lookahead := Lexer.consumeSym(lexer);
    libId := AstNewTerminalNode(AstNodeType.Ident, lookahead.lexeme);
    LexQueue.Enqueue(tmplist, libId);
        
    (* ( ',' libIdent )* *)
    WHILE lookahead.token = Token.Comma DO
      lookahead := Lexer.consumeSym(lexer);
      
      (* libIdent *)
      IF matchToken(Token.StdIdent) THEN
        lookahead := Lexer.ConsumeSym(lexer);
        libId := AstNewTerminalNode(AstNodeType.Ident, lookahead.lexeme);
        LexQueue.Enqueue(templist, libId)
        
      ELSE (* resync *)
        lookahead := skipToMatchSet(FOLLOW(Import))
      END (* IF *)
    END (* WHILE *)
    
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(Import))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewListNode(AstNodeType.Implist, tmplist);
  AstQueue.Release(tmplist);
  
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
  ast := AST.NewTerminalNode(AstNodeType.Ident, lexeme);
  
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
      lookahead := skipToMatchSet(FOLLOW(Ident))
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
      lookahead := skipToMatchSet(FOLLOW(Ident))
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
      lookahead := constDefSection(astNode)
      
  (* TYPE *)
  | Token.Type :
      lookahead := typeDefSection(astNode)
  
  (* VAR *)
  | Token.Var :
      lookahead := varDeclSection(astNode)
  
  (* PROCEDURE *)
  | Token.Procedure :
      (* procedureHeader *)
      lookahead := procedureHeader(astNode);
      
      (* ';' *)
      IF matchToken(Token.Semicolon) THEN
        (* consume semicolon *)
        lookahead := Lexer.consumeSym(lexer)
      ELSE (* resync *)
        lookahead := skipToMatchSet(FOLLOW(Definition))
      END (* IF *)
      
  (* TO *)
  | Token.To :
      lookahead := Lexer.consumeSym(lexer);
      (* toDoList *)
      lookahead := toDoList(astNode);
      
      (* ';' *)
      IF matchToken(Token.Semicolon) THEN
        (* consume semicolon *)
        lookahead := Lexer.consumeSym(lexer)
      ELSE (* resync *)
        lookahead := skipToMatchSet(FOLLOW(Definition))
      END (* IF *)
      
  END; (* CASE *)
    
  RETURN lookahead
END definition;


(* --------------------------------------------------------------------------
 * private function constDefSection(astNode)
 * --------------------------------------------------------------------------
 * Parses rule constDefSection, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * constDefSection :=
 *   CONST ( constDefinition ';' )+
 *   ;
 *
 * astNode: constDefListNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE constDefSection ( VAR astNode : AstT ) : SymbolT;

VAR
  constDef : AstT;
  tmplist : AstQueueT;
  lookahead := SymbolT;

BEGIN
  PARSER_DEBUG_INFO("constDefSection");
  
  AstQueue.New(tmplist);
  
  (* CONST *)
  lookahead := Lexer.ConsumeSym(lexer);
  
  (* constDefinition *)
  IF matchSet(FIRST(ConstDefinition)) THEN
    lookahead := constDefinition(constDef);
    AstQueue.Enqueue(tmplist, constDef)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(ConstDefinition))
  END (* IF *)
  
  (* ';' *)
  IF matchToken(Token.Semicolon) THEN
    (* consume semicolon *)
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(ConstDefinition))
  END (* IF *)
  
  (* ( constDefinition ';' )* *)
  WHILE inFIRST(ConstDefinition, lookahead.token) DO
    (* constDefinition *)
    lookahead := constDefinition(constDef);
    AstQueue.Enqueue(tmplist, constDef)
    
    (* ';' *)
    IF matchToken(Token.Semicolon) THEN
      (* consume semicolon *)
      lookahead := Lexer.consumeSym(lexer)
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(ConstDefinition))
    END (* IF *)
  END; (* WHILE *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewListNode(AstNodeType.DefList, tmplist);
  AstQueue.Release(tmplist);
  
  RETURN lookahead
END constDefSection;


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
    (* consume '=' *)
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FIRST(ConstDefinition))
  END; (* IF *)
  
  (* constExpression *)
  IF matchSet(FIRST(Expression)) THEN
    (* alias constExpression = expression *)
    lookahead := expression(expr)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(Expression))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.ConstDef, constId, expr);
  
  RETURN lookahead
END constDefinition;


(* --------------------------------------------------------------------------
 * private function typeDefSection(astNode)
 * --------------------------------------------------------------------------
 * Parses rule typeDefSection, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * constDefSection :=
 *   TYPE ( typeDefinition ';' )+
 *   ;
 *
 * astNode: typeDefListNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE typeDefSection ( VAR astNode : AstT ) : SymbolT;

VAR
  typeDef : AstT;
  tmplist : AstQueueT;
  lookahead := SymbolT;

BEGIN
  PARSER_DEBUG_INFO("typeDefSection");
  
  AstQueue.New(tmplist);
  
  (* TYPE *)
  lookahead := Lexer.ConsumeSym(lexer);
  
  (* typeDefinition *)
  IF matchSet(FIRST(TypeDefinition)) THEN
    lookahead := typeDefinition(typeDef)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(TypeDefinition))
  END (* IF *)
  
  (* ';' *)
  IF matchToken(Token.Semicolon) THEN
    (* consume semicolon *)
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(TypeDefinition))
  END (* IF *)
  
  (* ( typeDefinition ';' )* *)
  WHILE inFIRST(TypeDefinition, lookahead.token) DO
    (* typeDefinition *)
    lookahead := typeDefinition(typeDef);
    
    (* ';' *)
    IF matchToken(Token.Semicolon) THEN
      (* consume semicolon *)
      lookahead := Lexer.consumeSym(lexer)
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(TypeDefinition))
    END (* IF *)
  END; (* WHILE *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewListNode(AstNodeType.DefList, tmplist);
  AstQueue.Release(tmplist);
  
  RETURN lookahead
END typeDefSection;


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
    (* consume '=' *)
    lookahead := Lexer.consumeSym(lexer)
  ELSE
    (* resync *)
    lookahead := skipToMatchSet(NonTerminals.Resync(OpaqueOrType))
  END; (* IF *)
  
  (* OPAQUE | type *)
  IF matchSet(FIRST(TypeDefinitionTail)) THEN
    
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
    Token.Alias :
      lookahead := aliasType(astNode)
      
  (* derivedType | *)
  | Token.StdIdent,
    Token.ForeignIdent :
      (* alias typeIdent = qualident *)
      lookahead := qualident(astNode)
    
  (* subrangeType | *)
  | Token.LeftBracket :
      lookahead := subrangeType(astNode)
  
  (* enumType | *)
  | Token.LeftParen :
      lookahead := enumType(astNode)
  
  (* setType | *)
  | Token.Set :
      lookahead := setType(astNode)
  
  (* arrayType | *)
  | Token.Array :
      lookahead := arrayType(astNode)
  
  (* recordType | *)
  | Token.Record :
      lookahead := recordType(astNode)
  
  (* pointerType | *)
  | Token.Pointer :
      lookahead := pointerType(astNode)
  
  (* procedureType *)
  | Token.Procedure :
      lookahead := procedureType(astNode)
  
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
    (* consume OF *)
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FIRST(Qualident))
  END; (* IF *)
  
  (* typeIdent *)
  IF matchSet(FIRST(Qualident)) THEN
    (* alias typeIdent = qualident *)
    lookahead := qualident(baseType)
    
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(AliasType))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.AliasType, baseType);
  
  RETURN lookahead
END aliasType;


(* --------------------------------------------------------------------------
 * private function subrangeType(astNode)
 * --------------------------------------------------------------------------
 * Parses rule subrangeType, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * subrangeType :=
 *   range OF ordinalType
 *   ;
 *
 * range :=
 *   '[' lowerBound '..' upperBound ']'
 *   ;
 *
 * alias lowerBound = constExpression ;
 *
 * alias upperBound = constExpression ;
 *
 * alias ordinalType = typeIdent ;
 *
 * astNode: (SUBR lowerBound upperBound baseType)
 * --------------------------------------------------------------------------
 *)
PROCEDURE subrangeType ( VAR astNode : AstT ) : SymbolT;

VAR
  lowerBound, upperBound, baseType : AstT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("subrangeType");
  
  (* '[' *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* lowerBound *)
  IF matchSet(FIRST(Expression)) THEN
    lookahead := expression(lowerBound);
    
    (* '..' *)
    IF matchToken(Token.DotDot) THEN
      lookahead := Lexer.consumeSym();
      
      (* upperBound *)
      IF matchSet(FIRST(Expression) THEN
        lookahead := expression(upperBound);
        
        (* ']' *)
        IF matchToken(Token.RightBracket) THEN
          lookahead := Lexer.consumeSym()
        ELSE (* resync *)
          lookahead := skipToMatchSet(FOLLOW(Range))
        END (* IF *)
        
      ELSE (* resync *)
        lookahead := skipToMatchSet(FOLLOW(Range))
      END (* IF *)
      
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(Range))
    END (* IF *)
    
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(Range))
  END; (* IF *)
  
  (* OF *)
  IF matchToken(Token.Of) THEN
    lookahead := Lexer.consumeSym();
    
    (* ordinalType *)
    IF matchSet(FIRST(Qualident)) THEN
      lookahead := qualident(baseType)
      
    ELSE (* resync *)
      skipToMatchSet(FOLLOW(SubrangeType))
    END (* IF *)
    
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(SubrangeType))
  END; (* IF *)
    
  (* build AST node and pass it back in astNode *)
  astNode :=
    AST.NewNode(AstNodeType.SubrType, lowerBound, upperBound, baseType);
  
  RETURN lookahead
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
      lookahead := skipToMatchSet(FIRST(IdentList))
    END (* IF *)
  END; (* IF *)
  
  (* identList *)
  IF matchSet(FIRST(IdentList)) THEN
    lookahead := identList(valueList);
    
    (* ')' *)
    IF matchToken(Token.RightParen) THEN
      lookahead := Lexer.consumeSym(lexer)
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(EnumType))
    END (* IF *)
    
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
    lookahead := Lexer.consumeSym(lexer);
    
    (* enumTypeIdent *)
    IF matchSet(FIRST(Qualident)) THEN
      lookahead := qualident(elemType)
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(SetType))
    END (* IF *)
    
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
    
    (* OF *)
    IF matchToken(Token.Of) THEN
      lookahead := Lexer.consumeSym(lexer);
      
      (* typeIdent *)
      IF matchSet(FIRST(Qualident)) THEN
        lookahead := qualident(baseType);
        
      ELSE (* resync *)
        lookahead := skipToMatchSet(FOLLOW(ArrayType))
      END (* IF *)
      
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(ArrayType))
    END (* IF *)
    
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(ArrayType))
  END; (* IF *)
  
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
  baseType, listNode, fieldListSeq : AstT;
  tmpList : AstQueue;
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
      
      (* ')' *)
      IF matchToken(Token.RightParen) THEN
        lookahead := Lexer.consumeSym(lexer);
      ELSE (* resync *)
        lookahead := skipToMatchSet(FIRST(FieldList))
      END (* IF *)
      
    ELSE (* resync *)
      lookahead := skipToMatchSet(FIRST(FieldList))
    END (* IF *)
    
  ELSE (* non-extensible record *)
    baseType := AST.emptyNode()
  END; (* IF *)
  
  (* fieldList *)
  IF matchSet(FIRST(FieldList)) THEN
    lookahead := fieldList(listNode);
    AstQueue.Enqueue(tmplist, listNode)
    
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(FieldList))
  END; (* IF *)
  
  (* ( ';' fieldList )* *)
  WHILE lookahead.token = Token.Semicolon DO
    (* ';' *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* fieldList *)
    IF matchSet(FIRST(FieldList)) THEN
      lookahead := fieldList(listNode);
      AstQueue.Enqueue(tmplist, listNode)
      
    ELSE
      lookahead := skipToMatchSet(FOLLOW(FieldList))
    END (* IF *)
  END (* WHILE *)
  
  (* END *)
  IF matchToken(Token.End) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(RecordType))
  END; (* IF *)
    
  (* build AST node and pass it back in astNode *)
  fieldListSeq := AST.NewListNode(AstNodeType.FieldListSeq, tmplist);
  astNode := AST.NewNode(AstNodeType.RecordType, baseType, fieldListSeq);
  AstQueue.Release(tmplist);
  
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
    lookahead := Lexer.consumeSym(lexer);
    
    (* typeIdent *)
    IF matchSet(FIRST(Qualident)) THEN
      lookahead := qualident(baseType)
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(PointerType))
    END (* IF *)
    
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
  tmplist : AstQueueT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("procedureType");
  
  AstQueue.New(tmplist);
  
  (* PROCEDURE *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* ( '(' formalType ( ',' formalType )* ')' )? *)
  IF lookahead.token = Token.LeftParen THEN
    
    (* '(' *)
    lookahead := Lexer.consumeSym(lexer);
    
    IF matchSet(FIRST(FormalType)) THEN
      (* formalType *)
      lookahead := formalType(ftype);
      AstQueue.Enqueue(tmplist, ftype)
      
      (* ( ',' formalType )* *)
      WHILE lookahead.token = Token.Comma DO
        (* ',' *)
        lookahead := Lexer.consumeSym(lexer);
        
        (* formalType *)
        IF matchSet(FIRST(FormalType)) THEN
          lookahead := formalType(ftype);
          AstQueue.Enqueue(tmplist, ftype)
        ELSE (* resync *)
          lookahead := skipToMatchSet(FOLLOW(FormalType))
        END (* IF *)
      END; (* WHILE *)
      
      (* ')' *)
      IF matchToken(Token.RightParen) THEN
        lookahead := Lexer.consumeSym(lexer)
      ELSE (* resync *)
        lookahead := skipToMatchSet(<ColonOrQualidentOrSemicolon>)
      END (* IF *)
      
    ELSE (* resync *)
      lookahead := skipToMatchSet(<ColonOrQualidentOrSemicolon>)
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
  formalTypeList :=
    AST.NewListNode(AstNodeType.FormalTypeList, tmplist);
  astNode :=
    AST.NewNode(AstNodeType.ProcedureType, formalTypeList, retType);
  AstQueue.Release(tmplist);
  
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
        lookahead := skipToMatchTokenOrSet(Token.Semicolon, FIRST(Qualident))
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
      lookahead := skipToMatchTokenOrSet(Token.Semicolon, FOLLOW(Qualident))
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
  
  (* addressType | BARE *)
  IF matchSet(FIRST(CastingFormalTypeTail)) THEN
    
    (* addressType | *)
    IF inFIRST(AddressTypeIdent, lookahead.token) THEN
      lookahead := addressTypeIdent(astNode)
    ELSE (* BARE *)
    
      (* BARE *)
      IF matchToken(Token.Bare) THEN
        lookahead := Lexer.consumeSym(lexer);
        
        (* ARRAY *)
        IF matchToken(Token.Array) THEN
          lookahead := Lexer.consumeSym(lexer);
          
          (* OF *)
          IF matchToken(Token.Of) THEN
            lookahead := Lexer.consumeSym(lexer);
            
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
            END (* IF *)
            
          ELSE (* resync *)
            lookahead := skipToMatchSet(FOLLOW(CastingFormalType))
          END (* IF *)
          
        ELSE (* resync *)
          lookahead := skipToMatchSet(FOLLOW(CastingFormalType))
        END (* IF *)
        
      ELSE (* resync *)
        lookahead := skipToMatchSet(FOLLOW(CastingFormalType))
      END; (* IF *)
    
    END (* addressType | *)
    
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(CastingFormalType))
  END; (* addressType | BARE *)
    
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
    LexQueue.Enqueue(tmplist, token.lexeme);
    lookahead := Lexer.consumeSym(lexer);
    
    (* '.' *)
    IF matchToken(Token.Dot) THEN
      lookahead := Lexer.consumeSym(lexer)
    ELSE (* resync *)
      lookahead :=
        skipToMatchTokenOrSet(Token.Address, FOLLOW(addressTypeIdent))
    END (* IF *)
  END; (* IF *)
  
  (* ADDRESS *)
  IF matchToken(Token.Address) THEN
    LexQueue.Enqueue(tmplist, token.lexeme);
    lookahead := Lexer.consumeSym(lexer);
    
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(CastingFormalType))
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
  IF matchTokenOrSet(Token.ArgList, FIRST(nonAttrFormalType)) THEN
    
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
    lookahead := Lexer.consumeSym(lexer);
    
    (* nonAttrFormalType *)
    IF matchSet(FIRST(NonAttrFormalType)) THEN
      lookahead := nonAttrFormalType(ftype)
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(SimpleVariadicFormalType))
    END (* IF *)
    
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(SimpleVariadicFormalType))
  END; (* IF *)
  
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
  procId, fparams, fpList, retType : AstT;
  tmplist : AstQueueT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("procedureHeader");
  
  AstQueue.New(tmplist);
  
  (* PROCEDURE *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* procedureSignature *)
  IF matchSet(FIRST(ProcedureSignature)) THEN
    
    (* ident *)
    IF matchSet(FIRST(Ident)) THEN
      lookahead := ident(procId);
      
      (* ( '(' formalParams ( ';' formalParams )* ')' )? *)
      IF matchToken(Token.LeftParen) THEN
        (* '(' *)
        lookahead := Lexer.consumeSym(lexer);
        
        (* formalParams *)
        IF matchSet(FIRST(formalParams)) THEN
          lookahead := formalParams(fparams);
          AstQueue.Enqueue(tmplist, fparams)
        ELSE (* resync *)
          lookahead := skipToMatchSet(FOLLOW(FormalParams))
        END; (* IF *)
        
        (* ( ';' formalParams )* *)
        WHILE lookahead.token = Token.Semicolon DO
          (* ';' *)
          lookahead := Lexer.consumeSym(lexer);
          
          (* formalParams *)
          IF matchSet(FIRST(formalParams)) THEN
            lookahead := formalParams(fparams);
            AstQueue.Enqueue(tmplist, fparams)
          ELSE (* resync *)
            lookahead := skipToMatchSet(FOLLOW(FormalParams))
          END (* IF *)
        END; (* WHILE *)
        
        (* ')' *)
        IF matchToken(Token.RightParen) THEN
          lookahead := Lexer.consumeSym(lexer)
        ELSE (* resync *)
          lookahead :=
            skipToMatchTokenOrSet(Token.Colon, FOLLOW(procedureHeader))
        END (* IF *)
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
      END (* IF *)
      
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(procedureHeader))
    END (* IF *)
    
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(procedureHeader))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  fplist := AST.NewListNode(AstNodeType.FPList, tmplist);
  astNode := AST.NewNode(AstNodeType.Proc, procId, fplist, retType);
  AstQueue.Release(tmplist);
  
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
      lookahead := skipToMatchSet(FOLLOW(formalParams))
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
    lookahead := skipToMatchSet(FOLLOW(attributedFormalParams))
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
    lookahead := Lexer.consumeSym(lexer);
    
    (* moduleIdent *)
    IF matchToken(Token.StdIdent) THEN
      ident1 = lookahead.lexeme;
      lookahead := Lexer.consumeSym(lexer);
      
      (* ';' *)
      IF matchToken(Token.Semicolon) THEN
        lookahead := Lexer.consumeSym(lexer)
      ELSE (* resync *)
        lookahead := skipToMatchTokenOrSet(Token.Import, FIRST(Block))
      END (* IF *)
      
    ELSE (* resync *)
      lookahead := skipToMatchTokenOrSet(Token.Import, FIRST(Block))
    END (* IF *)
    
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrSet(Token.Import, FIRST(Block))
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
    lookahead := block(blockNode);
    
    (* moduleIdent *)
    IF matchToken(Token.StdIdent) THEN
      ident2 := lookahead.lexeme;
      lookahead := Lexer.ConsumeSym(lexer);
    
      IF ident1 # ident2 THEN
        (* TO DO: report error -- module identifiers don't match *) 
      END; (* IF *)
    
      (* '.' *)
      IF matchToken(Token.Period, FOLLOW(ImplementationnModule)) THEN
        lookahead := Lexer.consumeSym(lexer)
      ELSE (* resync *)
        lookahead := skipToMatchSet(FOLLOW(ImplementationnModule))
      END (* IF *)
      
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(ImplementationnModule))
    END (* IF *)
    
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(ImplementationnModule))
  END (* IF *)
  
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
    (* ALIAS ( aliasDeclaration ';' )+ | *)
    Token.Alias :
      lookahead := aliasDeclSection(astNode)
      
   (* CONST ( constDeclaration ';' )+ | *)
 | Token.Const :
      (* alias constDeclaration = constDefinition *)
      lookahead := constDefSection(astNode)
      
   (* TYPE ( typeDeclaration ';' )+ | *)
  | Token.Type :
      lookahead := typeDeclSection(astNode)
  
   (* VAR ( varOrFieldDeclaration ';' )+ | *)
  | Token.Var :
      lookahead := varDeclSection(astNode)
  
   (* procedureHeader ';' block ident ';' | *)
  | Token.Procedure :
      (* procedureHeader *)
      lookahead := procDeclaration(astNode);
            
   (* toDoList ';' *)
  | Token.To :  
      (* toDoList *)
      lookahead := toDoList(astNode);
      
      (* ';' *)
      IF matchToken(Token.Semicolon) THEN
        (* consume semicolon *)
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
 * private function aliasDeclSection(astNode)
 * --------------------------------------------------------------------------
 * Parses rule aliasDeclSection, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * aliasDeclSection :=
 *   ALIAS ( aliasDeclaration ';' )+
 *   ;
 *
 * astNode: aliasDeclListNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE aliasDeclSection ( VAR astNode : AstT ) : SymbolT;

VAR
  aliasDecl : AstT;
  tmplist : AstQueueT;
  lookahead := SymbolT;

BEGIN
  PARSER_DEBUG_INFO("aliasDeclSection");
  
  AstQueue.New(tmplist);
  
  (* ALIAS *)
  lookahead := Lexer.ConsumeSym(lexer);
  
  (* aliasDeclaration *)
  IF matchSet(FIRST(AliasDeclaration)) THEN
    lookahead := aliasDeclaration(aliasDecl);
    AstQueue.Enqueue(tmplist, aliasDecl)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(AliasDeclaration))
  END (* IF *)
  
  (* ';' *)
  IF matchToken(Token.Semicolon) THEN
    (* consume semicolon *)
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(ConstDefinition))
  END (* IF *)
  
  (* ( aliasDeclaration ';' )* *)
  WHILE inFIRST(AliasDeclaration, lookahead.token) DO
    (* aliasDeclaration *)
    lookahead := constDefinition(aliasDecl);
    AstQueue.Enqueue(tmplist, aliasDecl)
    
    (* ';' *)
    IF matchToken(Token.Semicolon) THEN
      (* consume semicolon *)
      lookahead := Lexer.consumeSym(lexer)
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(AliasDeclaration))
    END (* IF *)
  END; (* WHILE *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewListNode(AstNodeType.AliasDecl, tmplist);
  AstQueue.Release(tmplist);
  
  RETURN lookahead
END aliasDeclSection;


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
      LOOP
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
          lookahead := skipToMatch();
          EXIT
        END (* IF *)
        
      END; (* LOOP *)
      
      (* '=' qualifiedWildcard *)
      IF matchToken(Token.Equal) THEN
        (* '=' *)
        lookahead := Lexer.consumeSym(lexer);
        
        (* qualifiedWildcard *)
        IF matchSet(FIRST(QualifiedWildcard)) THEN
          lookahead := qualifiedWildcard(translation)
          
        ELSE (* resync *)
          lookahead := skipToMatchSet(FOLLOW(namedAliasDecl))
        END (* IF *)
        
        (* TO DO : resolve aliases in tmplist using wildcard *)
        
      ELSE (* resync *)
        lookahead := skipToMatchSet(FOLLOW(namedAliasDecl))
      END; (* IF *)
      
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
    
    (* qualifiedWildcard *)
    IF matchSet(FIRST(QualifiedWildcard)) THEN
      lookahead := qualifiedWildcard(translation);
      
      (* build AST node and pass it back in astNode *)
      astNode := AST.NewNode(AstNodeType.WcAlias, translation);
      
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(WildcardAliasDecl))
    END (* IF *)
    
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(WildcardAliasDecl))
  END; (* IF *)
  
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
    lookahead := skipToMatchSet(FOLLOW(qualifiedWildcard))
  END; (* IF *)
  
  RETURN lookahead
END qualifiedWildcard;


(* --------------------------------------------------------------------------
 * private function typeDeclSection(astNode)
 * --------------------------------------------------------------------------
 * Parses rule typeDeclSection, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * typeDeclSection :=
 *   TYPE ( typeDeclaration ';' )+
 *   ;
 *
 * astNode: typeDeclListNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE typeDeclSection ( VAR astNode : AstT ) : SymbolT;

VAR
  typeDecl : AstT;
  tmplist : AstQueueT;
  lookahead := SymbolT;

BEGIN
  PARSER_DEBUG_INFO("typeDeclSection");
  
  AstQueue.New(tmplist);
  
  (* TYPE *)
  lookahead := Lexer.ConsumeSym(lexer);
  
  (* typeDeclaration *)
  IF matchSet(FIRST(TypeDeclaration)) THEN
    lookahead := typeDeclaration(typeDecl)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(TypeDeclaration))
  END (* IF *)
  
  (* ';' *)
  IF matchToken(Token.Semicolon) THEN
    (* consume semicolon *)
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(TypeDeclaration))
  END (* IF *)
  
  (* ( typeDeclaration ';' )* *)
  WHILE inFIRST(TypeDeclaration, lookahead.token) DO
    (* typeDefinition *)
    lookahead := typeDeclaration(typeDecl);
    
    (* ';' *)
    IF matchToken(Token.Semicolon) THEN
      (* consume semicolon *)
      lookahead := Lexer.consumeSym(lexer)
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(TypeDeclaration))
    END (* IF *)
  END; (* WHILE *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewListNode(AstNodeType.DeclList, tmplist);
  AstQueue.Release(tmplist);
  
  RETURN lookahead
END typeDeclSection;


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
    (* consume '=' *)
    lookahead := Lexer.consumeSym(lexer)
  ELSE
    (* resync *)
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
  fieldList, fieldListSeq, inField : AstT;
  tmplist : AstQueueT;
  lookahead := SymbolT;

BEGIN
  PARSER_DEBUG_INFO("typeDeclaration");
  
  AstQueue.New(tmplist);
  
  (* IN *)
  lookahead := Lexer.consumeSym(lexer);
      
  (* RECORD *)
  IF matchToken(Token.Record) THEN
    lookahead := Lexer.consumeSym(lexer);
    
    (* ( fieldDeclaration ';' )+ *)
    REPEAT
      (* fieldDeclaration *)
      IF matchSet(FIRST(VarOrFieldDeclaration)) THEN
        lookahead := varOrFieldDeclaration(fieldList);
        AstQueue.Enqueue(tmplist, fieldList);
        
        (* ';' *)
        IF matchToken(Token.Semicolon) THEN
          lookahead := Lexer.consumeSym(lexer)
        ELSE (* resync *)
          lookahead := skiptToMatchSet(FIRST(VarOrFieldDeclaration))
        END (* IF *)
        
      ELSE (* resync *)
        lookahead := skiptToMatchSet(FIRST(VarOrFieldDeclaration))
      END (* IF *)
    UNTIL NOT inFIRST(VarOrFieldDeclaration, lookahead.token);
    
  ELSE (* resync *)
    lookahead := skipToMatchSet(FIRST(IndeterminateField))
  END; (* IF *)
  
  (* indeterminateType *)
  IF matchSet(FIRST(indeterminateField)) THEN
    lookahead := indeterminateField(inField)
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrSet(Token.End, FOLLOW(IndeterminateType))
  END; (* IF *)
  
  IF matchToken(Token.End) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE
    lookahead := skipToMatchSet(FOLLOW(IndeterminateType))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  fieldListSeq := AST.NewListNode(AstNodeType.FieldListSeq, tmplist);
  astNode := AST.NewNode(AstNodeType.InRec, fieldListSeq, inField);
  AstQueue.Release(tmplist);
  
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
    lookahead :=
      skipToMatchTokenOrSet(Token.Colon, FOLLOW(indeterminateField))
  END; (* IF *)

  (* ':' *)
  IF matchToken(Token.Colon) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE
    lookahead :=
      skipToMatchTokenOrSet(Token.Bare, FOLLOW(indeterminateField))
  END; (* IF *)
  
  (* BARE *)
  IF matchToken(Token.Bare) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE
    lookahead :=
      skipToMatchTokenOrSet(Token.Array, FOLLOW(indeterminateField))
  END; (* IF *)
  
  (* ARRAY *)
  IF matchToken(Token.Bare) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE
    lookahead :=
      skipToMatchTokenOrSet(Token.StdIdent, FOLLOW(indeterminateField))
  END; (* IF *)
  
  (* discriminantFieldIdent *)
  IF matchSet(FIRST(Ident)) THEN
    lookahead := ident(discrId)
  ELSE (* resync *)
    lookahead :=
      skipToMatchTokenOrSet(Token.Of, FOLLOW(indeterminateField))
  END; (* IF *)
  
  (* OF *)
  IF matchToken(Token.Of) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE
    lookahead :=
      skipToMatchTokenOrSet(Token.StdIdent, FOLLOW(indeterminateField))
  END; (* IF *)
  
  (* typeIdent *)
  IF matchSet(FIRST(Ident)) THEN
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
    lookahead := skipToMatchSet(FIRST(VarOrFieldDeclTail))
  END; (* IF *)
  
  (* typeIdent | anonType *)
  IF matchSet(FIRST(VarOrFieldDeclTail)) THEN
    
    (* typeIdent | *)
    IF inFIRST(Ident, lookahead.token) THEN
      lookahead := ident(typeNode)
    ELSE (* anonType *)
      lookahead := anonType(typeNode)
    END (* IF *)
    
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(varOrFieldDeclaration))
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
      lookahead := skipToMatchTokenOrSet(Token.Of, FOLLOW(anonType))
    END; (* IF *)
    
    (* OF *)
    IF matchToken(Token.Of) THEN
      lookahead := Lexer.consumeSym(lexer)
    ELSE (* resync *)
      lookahead := skipToMatchTokenOrSet(Token.StdIdent, FOLLOW(anonType))
    END; (* IF *)
    
    (* typeIdent *)
    IF matchSet(FIRST(Qualident) THEN
      lookahead := qualident(baseType)
    ELSE (* resync *)
      lookahead := skipToMatchTokenOrSet(Token.Of, FOLLOW(anonType))
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
    lookahead := block(blockNode);
    
    (* ident *)
    IF matchSet(FIRST(Ident)) THEN
      ident2 := lookahead.lexeme;
      lookahead := Lexer.ConsumeSym(lexer);
    
      IF ident1 # ident2 THEN
        (* TO DO: report error -- procedure identifiers don't match *) 
      END; (* IF *)
    
      (* ';' *)
      IF matchToken(Token.Semicolon, FOLLOW(Declaration)) THEN
        lookahead := Lexer.consumeSym(lexer)
      ELSE (* resync *)
        lookahead := skipToMatchSet(FOLLOW(Declaration))
      END (* IF *)
      
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(Declaration))
    END (* IF *)
    
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
  stmt : AstT;
  tmplist : AstQueueT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("statementSequence");
  
  AstQueue.New(tmplist);
  
  (* statement *)
  lookahead := statement(stmt);
  AstQueue.Enqueue(tmplist, stmt);
  
  (* ( ';' statement )* *)
  WHILE lookahead.token = Token.Semicolon DO
    (* ';' *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* statement *)
    IF matchSet(FIRST(Statement)) THEN
      lookahead := statement(stmt);
      AstQueue.Enqueue(tmplist, stmt)
      
    ELSE (* resync *)
      lookahead :=
        skipToMatchTokenOrSet(Token.Semicolon), FOLLOW(Statement))
    END (* IF *)
  END; (* WHILE *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.StmtSeq, tmplist);
  AstQueue.Release(tmplist);
  
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

BEGIN
  PARSER_DEBUG_INFO("statement");
  
  lookahead := Lexer.lookaheadSym(lexer);
    
  CASE lookahead.token OF
  
  (* emptyStatement | *)
    Token.To :
      lookahead := toDoList(astNode)
      
  (* memMgtOperation | *)
  | Token.New,
    Token.Release :
      lookahead := memMgtOperation(astNode)
      
  (* returnStatement | *)
  | Token.Return :
      lookahead := returnStatement(astNode)
    
  (* ifStatement | *)
  | Token.If :
      lookahead := ifStatement(astNode)
    
   (* caseStatement | *)
  | Token.Case :
      lookahead := caseStatement(astNode)
    
  (* loopStatement | *)
  | Token.Loop :
      lookahead := loopStatement(astNode)
    
  (* whileStatement | *)
  | Token.While :
      lookahead := whileStatement(astNode)
    
  (* repeatStatement | *)
  | Token.Repeat :
      lookahead := repeatStatement(astNode)
    
  (* forStatement | *)
  | Token.For :
      lookahead := forStatement(astNode)
    
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
 * taskToDo :=
 *   description ( ',' estimatedHours )?
 *   ;
 *
 * alias description = StringLiteral ;
 *
 * alias weight = constExpression ;
 *
 * alias estimatedHours = constExpression ;
 *
 * astNode: (TODO intValNode exprNode quotedValNode exprNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE toDoList ( VAR astNode : AstT ) : SymbolT;

BEGIN

END toDoList;


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
    
  ELSE (* RELEASE designator *)
    
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
      lookahead := Lexer.consumeSym(lexer);
      
      (* '(' *)
      lookahead := Lexer.consumeSym(lexer);
      
      (* expressionList *)
      IF matchSet(FIRST(ExpressionList)) THEN
        lookahead := expressionList(exprList)
      ELSE (* resync *)
        lookahead :=
          skipToMatchTokenOrSet(Token.RightParen, FOLLOW(UpdateOrProcCall))
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

BEGIN

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

BEGIN

END caseStatement;


(* --------------------------------------------------------------------------
 * private function case(astNode)
 * --------------------------------------------------------------------------
 * Parses rule case, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * case :=
 *   caseLabels ( ',' caseLabels )* : StatementSequence
 *   ;
 *
 * astNode: (CASE caseLabelListNode stmtSeqNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE case ( VAR astNode : AstT ) : SymbolT;

BEGIN

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

BEGIN

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
    lookahead := skipToMatchTokenOrSet(Token.END, FOLLOW(LoopStatement))
  END; (* IF *)
  
  (* END *)
  IF matchToken(Token.End) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrSet(Token.END, FOLLOW(LoopStatement))
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

BEGIN

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

BEGIN

END forLoopVariants;


(* --------------------------------------------------------------------------
 * private function iterableExpr(astNode)
 * --------------------------------------------------------------------------
 * Parses rule iterableExpr, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * iterableExpr :=
 *   ordinalRange OF ordinalType | designator
 *   ;
 *
 * ordinalRange :=
 *   '[' firstValue '..' lastValue ']'
 *   ;
 *
 * alias firstValue = expression ;
 *
 * alias lastValue = expression ;
 *
 * alias ordinalType = typeIdent ;
 *
 * astNode: desigNode | rangeNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE iterableExpr ( VAR astNode : AstT ) : SymbolT;

BEGIN

END iterableExpr;


(* --------------------------------------------------------------------------
 * private function designator(astNode)
 * --------------------------------------------------------------------------
 * Parses rule designator, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * designator :=
 *   qualident designatorTail?
 *   ;
 *
 * designatorTail :=
 *   ( ( '[' exprOrSlice ']' | '^' ) ( '.' ident )* )+
 *   ;
 *
 *
 * astNode: (DESIG )
 * --------------------------------------------------------------------------
 *)
PROCEDURE designator ( VAR astNode : AstT ) : SymbolT;

BEGIN

END designator;


(* --------------------------------------------------------------------------
 * private function designatorTail(astNode)
 * --------------------------------------------------------------------------
 * Parses rule designatorTail, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * designatorTail :=
 *   ( ( '[' exprOrSlice ']' | '^' ) ( '.' ident )* )+
 *   ;
 *
 * exprOrSlice :=
 *   expression sliceTail?
 *   ;
 *
 * sliceTail :=
 *   '..' expression?
 *   ;
 *
 * astNode: (DESIG )
 * --------------------------------------------------------------------------
 *)
PROCEDURE designatorTail ( VAR astNode : AstT ) : SymbolT;

BEGIN

END designatorTail;


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
  tmplist : AstQueueT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("expressionList");
  
  AstQueue.New(tmplist);
  
  (* expression *)
  lookahead := expression(expr);
  AstQueue.Enqueue(tmplist, expr);
  
  (* ( ',' expression )* *)
  WHILE lookahead.token = Token.Comma DO
    (* ',' *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* expression *)
    IF matchSet(FIRST(Expression)) THEN
      lookahead := expression(expr);
      AstQueue.Enqueue(tmplist, expr)
      
    ELSE (* resync *)
      lookahead :=
        skipToMatchTokenOrSet(Token.Comma), FOLLOW(Expression))
    END (* IF *)
  END; (* WHILE *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.ExprList, tmplist);
  AstQueue.Release(tmplist);
  
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
      lookahead := simpleFactor(leftNode)
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
          skipToMatchTokenOrSet(Token.Comma), FOLLOW(SimpleExpression))
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
      lookahead :=
        skipToMatchTokenOrSet(Token.Comma), FOLLOW(Term))
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
  seenNOT : BOOLEAN;
  factorNode : AstT;
  lookahead := SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("simpleTerm");
  
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* NOT? *)
  IF lookahead.token = Token.Not THEN
    lookahead := Lexer.consumeSym(lexer);
    seenNOT := TRUE
  ELSE
    seenNOT := FALSE
  END; (* IF *)
  
  (* factor *)
  IF matchSet(FIRST(Factor)) THEN
    lookahead := factor(factorNode)
  ELSE
    lookahead := skipToMatchSet(FOLLOW(simpleTerm))
  END;
  
  IF seenNOT THEN
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
      lookahead :=
        skipToMatchSet(FOLLOW(SimpleFactor))
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
    
    astNode := AST.NewNode(AstNodeType.FCall, desig, exprList)
  ELSE
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
  
  (* ( ';' statement )* *)
  WHILE lookahead.token = Token.Semicolon DO
    (* ';' *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* valueComponent *)
    IF matchSet(FIRST(ValueComponent)) THEN
      lookahead := valueComponent(value);
      AstQueue.Enqueue(tmplist, value)
      
    ELSE (* resync *)
      lookahead :=
        skipToMatchTokenOrSet(Token.Comma), FOLLOW(ValueComponent))
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
      lookahead :=
        skipToMatchSet(FOLLOW(Expression))
    END; (* IF *)
    
    (* construct new node from left and right leaf nodes *)
    leftNode := AST.NewNode(AstNodeType.ValRange, leftNode, rightNode)
    
  END; (* IF *)
  
  (* pass leftNode back in astNode *)
  astNode := leftNode;
  
  RETURN lookahead
END valueComponent;


END Parser.