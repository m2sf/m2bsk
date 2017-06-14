(*!m2pim*) (* Copyright (c) 2015 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE Parser;

(* Parser for Modula-2 R10 Bootstrap Kernel *)

IMPORT
  AST, AstNodeType, AstQueue, NonTerminals,
  Lexer, LexQueue, Symbol, Token, TokenSet, String;

FROM AST IMPORT AstT; (* AST.AST *)
FROM AstNodeType IMPORT AstNodeTypeT; (* AstNodeType.AstNodeType *)
FROM AstQueue IMPORT AstQueueT; (* AstQueue.AstQueue *)
FROM Lexer IMPORT LexerT; (* Lexer.Lexer *)
FROM LexQueue IMPORT LexQueueT; (* LexQueue.LexQueue *)
FROM Symbol IMPORT SymbolT; (* Symbol.Symbol *)
FROM Token IMPORT TokenT; (* Token.Token *)
FROM TokenSet IMPORT TokenSetT; (* TokenSet.TokenSet *)
FROM String IMPORT StringT; (* String.String *)

FROM NonTerminals IMPORT FIRST, FOLLOW;


(* Parser context *)

VAR
  ast : AstT;
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
  
  IF (expectedToken = lookahead.token) THEN
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
  IF TokenSet.isElement(expectedSet, lookahead) THEN
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
  
  implist := AST.NewListNode(AstNodeTypeT.ImpList, tmplist);
  tmplist:= AstQueue.ResetQueue(tmplist);
  
  (* definition* *)
  while lookahead.token = Token.Const OR
        lookahead.token = Token.Type OR
        lookahead.token = Token.Var OR
        lookahead.token = Token.Procedure DO
    lookahead := definition(deflist);
    AstQueue.Enqueue(tmplist, deflist)
  END (* WHILE *)
  
  deflist := AST.NewListNode(AstNodeTypeT.DefList, tmplist);
  tmplist := AstQueue.ResetQueue(tmplist);
  
  (* END *)
  IF matchToken(Token.End) THEN
    lookahead := Lexer.consumeSym(lexer);
    
    (* moduleIdent *)
    IF matchToken(Token.StdIdent) THEN
      lookahead := Lexer.ConsumeSym(lexer);
      ident2 := lookahead.lexeme;
    
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
  
BEGIN
  PARSER_DEBUG_INFO("import");

  (* IMPORT *)
  lookahead := Lexer.consumeSym(lexer);
  
  templist := LexQueue.New();
  
  (* libIdent *)
  IF matchToken(Token.StdIdent) THEN
    lookahead := Lexer.consumeSym(lexer);
    libId := AstNewTerminalNode(AstNodeTypeT.Ident, lookahead.lexeme);
    LexQueue.Enqueue(tmplist, libId);
        
    (* ( ',' libIdent )* *)
    WHILE lookahead.token = Token.Comma DO
      lookahead := Lexer.consumeSym(lexer);
      
      (* libIdent *)
      IF matchToken(Token.StdIdent) THEN
        lookahead := Lexer.ConsumeSym(lexer);
        libId := AstNewTerminalNode(AstNodeTypeT.Ident, lookahead.lexeme);
        LexQueue.Enqueue(templist, libId)
        
      ELSE (* resync *)
        lookahead := skipToMatchSet(FOLLOW(Import))
      END (* IF *)
    END (* WHILE *)
    
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(Import))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewListNode(AstNodeTypeT.Implist, tmplist);
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
  
BEGIN
  PARSER_DEBUG_INFO("ident");
  
  lookahead := Lexer.lookaheadSym(lexer);
  
  lexeme := lookahead.lexeme;

  (* StdIdent | ForeignIdent *)
  lookahead := Lexer.consumeSym(lexer)
    
  (* build AST node and pass it back in astNode *)
  ast := AST.NewTerminalNode(AstNodeTypeT.Ident, lexeme);
  
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
  astNode := AST.NewTerminalListNode(AstNodeTypeT.Qualident, tmplist);
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
  astNode := AST.NewTerminalListNode(AstNodeTypeT.IdentList, tmplist);
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

BEGIN
  PARSER_DEBUG_INFO("definition");

  lookahead := Lexer.lookaheadSym(lexer);
  
  CASE lookahead.token OF
    (* CONST *)
    Token.Const :
      lookahead := constDefSection(astNode)
      
  | Token.Type :
      lookahead := typeDefSection(astNode)
  
  | Token.Var :
      lookahead := varDeclSection(astNode)
  
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
      
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(Definition))
  
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
  astNode := AST.NewNode(AstNodeTypeT.ConstDef, constId, expr);
  
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
  astNode := AST.NewNode(AstNodeTypeT.TypeDef, typeId, typeDef);
  
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
  astNode := AST.NewNode(AstNodeTypeT.AliasType, baseType);
  
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
  astNode := AST.NewNode(AstNodeTypeT.EnumType, baseType, valueList);
  
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
  astNode := AST.NewNode(AstNodeTypeT.SetType, elemType);
  
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
  astNode := AST.NewNode(AstNodeTypeT.ArrayType, valueCount, baseType);
  
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
  astNode := AST.NewNode(AstNodeTypeT.RecordType, baseType, fieldListSeq);
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
  astNode := AST.NewNode(AstNodeTypeT.PointerType, baseType);
  
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

BEGIN

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
  nodeType : AstNodeTypeT;
  
BEGIN
  PARSER_DEBUG_INFO("attributedFormalType");
  
  
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(nodeType, formalTypeNode);
  
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

BEGIN
  PARSER_DEBUG_INFO("simpleVariadicFormalType");
  
  
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.ArgList, formalTypeNode);
  
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
 * astNode: (PROC identNode fparamsListNode qualidentNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE procedureHeader ( VAR astNode : AstT ) : SymbolT;

VAR
  procId, fpList, retType : AstT;
  
BEGIN
  PARSER_DEBUG_INFO("procedureHeader");
  
  
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.Proc, procId, fpList, retType);
  
  RETURN lookahead
END procedureHeader;


(* --------------------------------------------------------------------------
 * private function procedureSignature(astNode)
 * --------------------------------------------------------------------------
 * Parses rule procedureSignature, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * procedureHeader :=
 *   PROCEDURE procedureSignature
 *   ;
 *
 * astNode: (PROC identNode fparamsListNode qualidentNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE procedureSignature ( VAR astNode : AstT ) : SymbolT;

BEGIN

END procedureSignature;


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
  idList, ftype : AstT;
  
BEGIN
  PARSER_DEBUG_INFO("formalParams");
  
  
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.FParams, idList, ftype);
  
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
  fparams : AstT;
  nodeType : AstNodeTypeT;
  
BEGIN
  PARSER_DEBUG_INFO("attributedFormalParams");
  
  
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.nodeType, fparams);
  
  RETURN lookahead
END attributedFormalParams;


(* --------------------------------------------------------------------------
 * private function implOrPrgmModule(astNode)
 * --------------------------------------------------------------------------
 * Parses rule implOrPrgmModule, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * implOrPrgmModule :=
 *   IMPLEMENTATION MODULE moduleIdent ';'
 *   privateImport* block moduleIdent '.'
 *   ;
 *
 * alias privateImport = import ;
 *
 * astNode: (IMPMOD qualidentNode implistNode blockNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE implOrPrgmModule ( VAR astNode : AstT ) : SymbolT;

BEGIN

END implOrPrgmModule;


(* --------------------------------------------------------------------------
 * private function block(astNode)
 * --------------------------------------------------------------------------
 * Parses rule block, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * block :=
 *   declaration*
 *   BEGIN statementSequence END
 *   ;
 *
 * astNode: (BLOCK declListNode stmtSeqNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE block ( VAR astNode : AstT ) : SymbolT;

BEGIN

END block;


(* --------------------------------------------------------------------------
 * private function declaration(astNode)
 * --------------------------------------------------------------------------
 * Parses rule declaration, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * declaration :=
 *   ALIAS ( aliasDeclaration ';' )+ |
 *   CONST ( ident '=' constExpression ';' )+ |
 *   TYPE ( typeDeclaration ';' )+ |
 *   VAR ( varOrFieldDeclaration ';' )+ |
 *   procedureHeader ';' block ident ';' |
 *   toDoList ';'
 *   ;
 *
 * astNode: (DECLLIST declNode1 declNode2 ... declNodeN)
 * --------------------------------------------------------------------------
 *)
PROCEDURE declaration ( VAR astNode : AstT ) : SymbolT;

BEGIN

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

BEGIN

END aliasDeclaration;


(* --------------------------------------------------------------------------
 * private function namedAliasDecl(astNode)
 * --------------------------------------------------------------------------
 * Parses rule namedAliasDecl, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * namedAliasDecl :=
 *   aliasName
 *     ( '=' qualifiedName | ( ',' aliasName )* '=' qualifiedWildcard )
 *   ;
 *
 * alias aliasName = StdIdent ;
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

BEGIN

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

BEGIN

END wildcardAliasDecl;


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

BEGIN

END typeDeclaration;


(* --------------------------------------------------------------------------
 * private function indeterminateType(astNode)
 * --------------------------------------------------------------------------
 * Parses rule indeterminateType, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * indeterminateType :=
 *   IN RECORD
 *     fieldDeclaration ( fieldDeclaration ';' ) indeterminateField END
 *   ;
 *
 * alias fieldDeclaration = varOrFieldDeclaration ;
 *
 * astNode: (INREC fieldListNode identNode identNode qualidentNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE indeterminateType ( VAR astNode : AstT ) : SymbolT;

BEGIN

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

BEGIN

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

BEGIN

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

BEGIN

END anonType;


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

BEGIN

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

BEGIN

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

BEGIN

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

BEGIN

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

BEGIN

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

BEGIN

END whileStatement;


(* --------------------------------------------------------------------------
 * private function repeatStatement(astNode)
 * --------------------------------------------------------------------------
 * Parses rule repeatStatement, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * repeatStatement :=
 *   WHILE boolExpression DO statementSequence END
 *   ;
 *
 * astNode: (REPEAT stmtSeqNode exprNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE repeatStatement ( VAR astNode : AstT ) : SymbolT;

BEGIN

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

BEGIN

END expressionList;


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

BEGIN

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

BEGIN

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

BEGIN

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

BEGIN

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

BEGIN

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

BEGIN

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

BEGIN

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
 * valueComponent :=
 *   constExpression ( '..' constExpression )? |
 *   runtimeExpression
 *   ;
 *
 * alias runtimeExpression = expression ;
 *
 * astNode:
 *  (STRUCTVAL exprListNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE structuredValue ( VAR astNode : AstT ) : SymbolT;

BEGIN

END structuredValue;


END Parser.