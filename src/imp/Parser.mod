(*!m2pim*) (* Copyright (c) 2015-2017 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE Parser;

(* Parser for Modula-2 R10 Bootstrap Kernel *)

IMPORT
  AST, AstNodeType, AstQueue, NonTerminals,
  Lexer, LexQueue, Symbol, Token, TokenSet, Filename, String;

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


(* Module Context Type *)

TYPE ModuleContext = (
  Public,    (* when parsing definition modules *)
  Private ); (* when parsing program and implementation modules *)


(* Parser context *)

VAR
  lexer : LexerT;
  moduleContext : ModuleContext;
  fileType : Filename.FileType;
  statistics : Statistics;


(* Operations *)

(* --------------------------------------------------------------------------
 * public function compilationUnit(source, stats, status)
 * --------------------------------------------------------------------------
 * Parses rule compilationUnit depending on the source file type and builds
 * its AST.  Returns the AST on success or NIL on failure.
 *
 * compilationUnit :=
 *   definitionModule | implementationModule | programModule
 *   ;
 * --------------------------------------------------------------------------
 *)
PROCEDURE compilationUnit
  ( source : StringT; VAR stats : Statistics; VAR status : Status ) : AstT;

VAR
  lexerStatus : Lexer.Status;
  lookahead : SymbolT;
  ast : AstT;

BEGIN
  fileType := Filename.fileType(source);
  
  statistics.lexicalWarnings := 0; statistics.lexicalErrors := 0;
  statistics.syntaxWarnings := 0; statistics.syntaxErrors := 0;
  
  IF FileName.isFileTypeDefOrMod(fileType) THEN
    lexer := Lexer.New(source, lexerStatus);
    (* TO DO: verify lexer status *)
    
    lookahead := Lexer.lookaheadSym(lexer);
  
    CASE fileType OF
    (* .def, *.DEF *)
      Filename.FileType.Def :
      moduleContext := Public;
      
      (* DEFINITION *)
      IF lookahead.token = Token.Definition THEN
        lookahead := definitionModule(ast)
        
      ELSE (* missing start symbol *)
        status := Status.MissingStartSymbol;
        ast := NIL
      END (* IF *)
    
    (* .mod, .MOD *)
    | Filename.FileType.Mod :
      moduleContext := Private;
    
      (* IMPLEMENTATION *)
      IF lookahead.token = Token.Implementation THEN
        lookahead := implementationModule(ast)
        
      (* MODULE *)
      ELSIF lookahead.token = Token.Module THEN
        lookahead := programModule(ast)
        
      ELSE (* missing start symbol *)
        status := Status.MissingStartSymbol;
        ast := NIL
      END (* IF *)  
    END; (* CASE *)
    
    (* TO DO: verify lookahead *)
    (* TO DO: get lexical stats from lexer *)
    
    Lexer.Release(lexer)
    
  ELSE (* invalid file type *)
    status := Status.InvalidFileType;
    ast := NIL
  END;
  
  (* pass statistics back in stats *)
  stats := statistics;
  
  RETURN ast
END compilationUnit;


(* Private Operations *)

(* ************************************************************************ *
 * Syntax Analysis                                                          *
 * ************************************************************************ *)

(* --------------------------------------------------------------------------
 * Specific Parsing Functions
 * ------------------------------------------------------------------------ *)

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
 * Parses rule import or privateImport depending on moduleContext,
 * constructs its AST node, passes the node back in out-parameter astNode
 * and returns the new lookahead symbol.
 *
 * import :=
 *   IMPORT libIdent reExport? ( ',' libIdent reExport? )* ';'
 *   ;
 *
 * privateImport :=
 *   IMPORT libIdent ( ',' libIdent )* ';'
 *   ;
 *
 * alias libIdent = StdIdent ;
 *
 * alias reExport = '+' ;
 *
 * astNode: (IMPORT implist)
 * --------------------------------------------------------------------------
 *)
PROCEDURE import ( VAR astNode : AstT ) : SymbolT;

VAR
  idlist : AstT;
  tmplist : LexQueueT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("import");

  (* IMPORT *)
  lookahead := Lexer.consumeSym(lexer);
  
  LexQueue.New(tmplist);
  
  (* libIdent *)
  IF matchToken(Token.StdIdent) THEN
    LexQueue.Enqueue(tmplist, lookahead.lexeme);
    lookahead := Lexer.consumeSym(lexer)
    
    (* reExport? *)
    IF (moduleContext = Public) AND (lookahead.token = Token.Plus) THEN
      (* TO DO : encode re-export flag *)
      lookahead := Lexer.consumeSym(lexer)
    END; (* IF *)
  
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrSet(Token.Comma, FOLLOW(Import))
  END; (* IF *)
  
  (* ( ',' libIdent )* *)
  WHILE lookahead.token = Token.Comma DO
    (* ',' *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* libIdent *)
    IF matchToken(Token.StdIdent) THEN
      LexQueue.Enqueue(tmplist, lookahead.lexeme);
      lookahead := Lexer.ConsumeSym(lexer)

      (* reExport? *)
      IF (moduleContext = Public) AND (lookahead.token = Token.Plus) THEN
        (* TO DO : encode re-export flag *)
        lookahead := Lexer.consumeSym(lexer)
      END; (* IF *)
      
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
  lookahead : SymbolT;

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
  lookahead : SymbolT;
  
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
 * private function ident(astNode)
 * --------------------------------------------------------------------------
 * Parses rule ident, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * alias ident = StdIdent ;
 *
 * astNode: (IDENT "lexeme")
 * --------------------------------------------------------------------------
 *)
PROCEDURE ident ( VAR astNode : AstT ) : SymbolT;

VAR
  lexeme : LexemeT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("ident");
  
  lookahead := Lexer.lookaheadSym(lexer);
  
  lexeme := lookahead.lexeme;

  (* StdIdent *)
  lookahead := Lexer.consumeSym(lexer)
    
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewTerminalNode(AstNodeType.Ident, lexeme);
  
  RETURN lookahead
END ident;


(* --------------------------------------------------------------------------
 * private function typeDefinition(astNode)
 * --------------------------------------------------------------------------
 * Parses rule typeDefinition, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * typeDefinition :=
 *   ident '=' type
 *   ;
 *
 * astNode: (TYPEDEF identNode typeNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE typeDefinition ( VAR astNode : AstT ) : SymbolT;

VAR
  typeId, typeDef : AstT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("typeDefinition");
  
  (* ident *)
  lookahead := ident(typeId);
  
  (* '=' *)
  IF matchToken(Token.Equal) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    (* we assume '=' is simply missing *)
  END; (* IF *)
  
  (* type *)
  IF matchSet(FIRST(Type)) THEN
    lookahead := type(Public, typeDef)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(TypeDefinition))
  END; (* IF *)
        
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.TypeDef, typeId, typeDef);
  
  RETURN lookahead
END typeDefinition;


(* --------------------------------------------------------------------------
 * private function type(context, astNode)
 * --------------------------------------------------------------------------
 * Parses rule type or privateType depending on moduleContext, constructs its
 * AST node, passes the node back in out-parameter astNode and returns the
 * new lookahead symbol.
 *
 * type :=
 *   aliasType | derivedType | subrangeType | enumType | setType |
 *   arrayType | recordType | pointerType | opaqueType | procedureType )
 *   ;
 *
 * privateType :=
 *   aliasType | derivedType | subrangeType | enumType | setType |
 *   arrayType | recordType | octetSeqType | privatePointerType |
 *   procedureType )
 *   ;
 *
 * astNode: (TYPEDECL identNode typeNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE type ( VAR astNode : AstT ) : SymbolT;

VAR
  lookahead : SymbolT;
  
BEGIN
  lookahead := Lexer.lookaheadSym(lexer);
    
  CASE lookahead.token OF
  (* aliasType *)  
    Token.Alias :
    lookahead := aliasType(astNode)
    
  (* derivedType *)
  | Token.StdIdent :
    lookahead := derivedType(astNode)
  
  (* subrangeType *)
  | Token.LeftBracket :
    lookahead := subrangeType(astNode)
  
  (* enumType *)
  | Token.LeftParen :
    lookahead := enumType(astNode)
  
  (* setType *)
  | Token.Set :
    lookahead := setType(astNode)
  
  (* arrayType *)
  | Token.Array :
    lookahead := arrayType(astNode)
    
  (* recordType *)
  | Token.Record :
    lookahead := recordType(astNode)
  
  (* pointerType or privatePointerType *)
  | Token.Pointer :
    lookahead := pointerType(astNode)
  
  (* opaqueType *)
  | Token.Opaque :
    
    IF moduleContext = Public THEN
      lookahead := opaqueType(astNode)
    END; (* IF *)
    
  (* octetSeqType *)
  | Token.OctetSeq :
    
    IF moduleContext = Private THEN
      lookahead := octetSeqType(astNode)
    END (* IF *)
    
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
  lookahead : SymbolT;
  
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
 * private function qualident(astNode)
 * --------------------------------------------------------------------------
 * Parses rule qualident, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * qualident :=
 *   StdIdent | Qualident
 *   ;
 *
 * astNode: (IDENT "lexeme") | (QUALIDENT "lexeme")
 * --------------------------------------------------------------------------
 *)
PROCEDURE qualident ( VAR astNode : AstT ) : SymbolT;

VAR
  lexeme : LexemeT;
  lookahead : SymbolT;

BEGIN
  PARSER_DEBUG_INFO("qualident");
  
  lookahead := Lexer.lookaheadSym(lexer);
  
  lexeme := lookahead.lexeme;
  
  (* StdIdent | Qualident *)
  CASE lookahead.token OF
  
    (* StdIdent *)
    Token.StdIdent :
    astNode := AST.NewTerminalNode(AstNodeType.Ident, lexeme)
    
    (* Qualident *)
  | Token.Qualident :
    astNode := AST.NewTerminalNode(AstNodeType.Qualident, lexeme)
  END; (* CASE *)
  
  lookahead := Lexer.consumeSym(lexer);
  
  RETURN lookahead
END qualident;


(* --------------------------------------------------------------------------
 * private function subrangeType(astNode)
 * --------------------------------------------------------------------------
 * Parses rule subrangeType, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * subrangeType :=
 *   constRange OF countableType
 *   ;
 *
 * constRange :=
 *   '[' lowerBound '..' upperBound ']'
 *   ;
 *
 * alias countableType = typeIdent ;
 *
 * alias upperBound, lowerBound = constExpression ;
 *
 * astNode: (SUBR lowerBound upperBound baseType)
 * --------------------------------------------------------------------------
 *)
PROCEDURE subrangeType ( VAR astNode : AstT ) : SymbolT;

VAR
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("subrangeType");
  
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
      skipToMatchSetOrSet(FIRST(Qualident), FOLLOW(SubrangeType))
  END; (* IF *)
  
  (* countableType *)
  IF matchSet(FIRST(Qualident)) THEN
    lookahead := qualident(typeId)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(SubrangeType))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.Subr, lowerBound, upperBound, typeId);
  
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
  lookahead : SymbolT;
  
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
      lookahead := qualident(baseType)
    ELSE (* resync *)
      lookahead := skipToMatchTokenOrSet(Token.Comma, FIRST(IdentList))
    END; (* IF *)
    
    (* ',' *)
    IF matchToken(Token.Comma) THEN
      lookahead := Lexer.consumeSym(lexer)
    ELSE (* resync *)
      lookahead := skipToMatchSet(FIRST(IdentList))
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
  lookahead : SymbolT;

BEGIN
  PARSER_DEBUG_INFO("identList");

  lookahead := Lexer.lookaheadSym(lexer);
  
  LexQueue.New(tmplist);
  LexQueue.Enqueue(tmplist, lookahead.lexeme);
  
  (* ident *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* ( ',' ident )* *)
  WHILE lookahead.token = Token.Comma DO
    (* ',' *)
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
  lookahead : SymbolT;
  
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
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("arrayType");
  
  (* ARRAY *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* valueCount *)
  IF matchSet(FIRST(Expression)) THEN
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
  lookahead : SymbolT;
  
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
 * private function fieldList(astNode)
 * --------------------------------------------------------------------------
 * Parses rule fieldList, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * fieldList :=
 *   identList ':' ( typeIdent | subrangeType | arrayType | procedureType )
 *   ;
 *
 * astNode: (FIELDLIST identListNode typeNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE fieldList ( VAR astNode : AstT ) : SymbolT;

VAR
  idList, typeNode : AstT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("fieldList");
  
  (* identList *)
  lookahead := identList(idList);
  
  (* ':' *)
  IF matchToken(Token.Colon) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSetOrSet(FIRST(Qualident), FIRST(AnonType))
  END; (* IF *)
  
  (* typeIdent | subrangeType | arrayType | procedureType *)
  CASE lookahead.token OF
    (* typeIdent | *)
    Token.StdIdent,
    Token.Qualident :
      lookahead := qualident(typeNode)
      
    (* subrangeType | *)
  | Token.LeftBracket :
      lookahead := subrangeType(typeNode)
  
    (* arrayType | *)
  | Token.Array :
      lookahead := arrayType(typeNode)
  
    (* procedureType *)
  | Token.Procedure :
      lookahead := procedureType(typeNode)
  
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(FieldList))
  END; (* CASE *)
    
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.FieldList, idlist, typeNode);
  
  RETURN lookahead
END fieldList;


(* --------------------------------------------------------------------------
 * private function pointerType(context, astNode)
 * --------------------------------------------------------------------------
 * Parses rule pointerType or privatePointerType, depending on mdouleContext,
 * constructs its AST node, passes the node back  in out-parameter astNode
 * and returns the new lookahead symbol.
 *
 * pointerType :=
 *   POINTER TO typeIdent
 *   ;
 *
 * privatePointerType :=
 *   POINTER TO ( determinateTarget | indeterminateTarget )
 *   ;
 *
 * alias determinateTarget = typeIdent ;
 *
 * astNode: (POINTER qualidentNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE pointerType ( VAR astNode : AstT ) : SymbolT;

VAR
  baseType : AstT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("pointerType");
  
  (* POINTER *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* TO *)
  IF matchToken(Token.To) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE
    (* we assume 'TO' is simply missing *)
  END; (* IF *)
  
  CASE moduleContext OF
  
    Public :
    (* typeIdent *)
    IF matchSet(FIRST(Qualident)) THEN
      lookahead := qualident(baseType)
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(PointerType))
    END (* IF *)
    
  | Private :
    IF matchTokenOrSet(Token.Record, FIRST(Qualident)) THEN
    
      (* determinateTarget *)
      IF lookahead.token # Token.Record THEN
        lookahead := qualident(baseType)
        
      (* indeterminateTarget *)
      ELSE (* lookahead.token = Token.Record *)
        lookahead := indeterminateTarget(baseType)
      END (* IF *)
    
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(PrivatePointerType))
    END (* IF *)
  END; (* CASE *)
    
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.PointerType, baseType);
  
  RETURN lookahead
END pointerType;


(* --------------------------------------------------------------------------
 * private function opaqueType(astNode)
 * --------------------------------------------------------------------------
 * Parses rule opaqueType, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * opaqueType :=
 *   OPAQUE ( '[' allocSize ']' | POINTER );
 *
 * alias allocSize = constExpression ;
 *
 * astNode: ()
 * --------------------------------------------------------------------------
 *)
PROCEDURE opaqueType ( VAR astNode : AstT ) : SymbolT;

VAR
  constExpr : AstT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("opaqueType");
  
  (* OPAQUE *)
  lookahead := Lexer.consumeSym(lexer);
  
  CASE lookahead.token OF
    (* '[' *)
    Token.LeftBracket :
    lookahead := Lexer.consumeSym(lexer);
    
    (* allocSize *)
    IF matchSet(FIRST(constExpression) THEN
      lookahead := constExpression(constExpr)
    ELSE (* resync *)
      lookahead :=
        skipToMatchTokenOrSet(Token.RightBracket, FOLLOW(OpaqueType))
    END; (* IF *)
    
    (* ']' *)
    IF matchToken(Token.RightBracket) THEN
      lookahead := Lexer.consumeSym(lexer)
    ELSE (* resync *)
      lookahead :=
        skipToMatchSet(FOLLOW(OpaqueType))
    END (* IF *)
    
    (* POINTER *)
  | Token.Pointer :
      lookahead := Lexer.consumeSym(lexer)
      
      (* TO DO : pointer flag *)
      
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(OpaqueType))
  END; (* CASE *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.OpaqueType, TO DO);
  
  RETURN lookahead
END opaqueType;


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
  lookahead : SymbolT;
  
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
      lookahead := skipToMatchSet(FOLLOW(ProcedureType))
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
 *   ( CONST | VAR )? nonAttrFormalType
 *   ;
 *
 * astNode: formalTypeNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE formalType ( VAR astNode : AstT ) : SymbolT;

VAR
  nodeType : AstNodeTypeT;
  lookahead : SymbolT;

BEGIN
  PARSER_DEBUG_INFO("formalType");
  
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* ( CONST | VAR )? *)
  IF lookahead.token = Token.Const THEN
    (* CONST *)
    nodeType := AstNodeType.ConstP
  ELSIF lookahead.token = Token.Var THEN
    (* VAR *)
    nodeType := AstNodeType.VarP
  END; (* IF *)
  
  lookahead := Lexer.consumeSym(lexer);
  
  (* nonAttrFormalType *)
  IF inFIRST(NonAttrFormalType, lookahead.token) THEN
    lookahead := nonAttrFormalType(astNode)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(FormalType))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(nodeType, astNode);
  
  RETURN lookahead
END formalType;


(* --------------------------------------------------------------------------
 * private function nonAttrFormalType(astNode)
 * --------------------------------------------------------------------------
 * Parses rule nonAttrFormalType, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * nonAttrFormalType :=
 *   simpleFormalType | castingFormalType | variadicFormalType
 *   ;
 *
 * astNode: 
 * --------------------------------------------------------------------------
 *)
PROCEDURE nonAttrFormalType ( VAR astNode : AstT ) : SymbolT;

VAR
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("nonAttrFormalType");
  
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* simpleFormalType | castingFormalType | variadicFormalType *)
  CASE lookahead.token OF
    (* ARGLIST *)
    Token.Arglist :
    lookahead := variadicFormalType(astNode)
    
    (* ARRAY *)
  | Token.Array :
    lookahead := simpleFormalType(astNode)
    
    (* CAST | StdIdent *)
  | Token.StdIdent :
    IF lookahead.lexeme = Resword.cast THEN
      (* TO DO : check import of module UNSAFE *)
      lookahead := castingFormalType(astNode)
    ELSE
      lookahead := simpleFormalType(astNode)
    END (* IF *)
  
    (* Qualident *)
  | Token.Qualident :
    lookahead := simpleFormalType(astNode)
    
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(FormalType))
  END; (* CASE *)
  
  RETURN lookahead
END nonAttrFormalType;


(* --------------------------------------------------------------------------
 * private function simpleFormalType(astNode)
 * --------------------------------------------------------------------------
 * Parses rule simpleFormalType, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * simpleFormalType :=
 *   ( ARRAY OF )? typeIdent
 *   ;
 *
 * astNode: 
 * --------------------------------------------------------------------------
 *)
PROCEDURE simpleFormalType ( VAR astNode : AstT ) : SymbolT;

VAR
  seenArray : BOOLEAN;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("nonAttrFormalType");
  
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* ( ARRAY OF )? *)
  IF lookahead.token = Token.Array THEN
    (* ARRAY *)
    lookahead := Lexer.consumeSym(lexer);
    seenArray := TRUE;
    
    (* OF *)
    IF matchToken(Token.Of) THEN
      lookahead := Lexer.consumeSym(lexer)
    ELSE (* resync *)
      lookahead :=
        skipToMatchSetOrSet(FIRST(Qualident), FOLLOW(SimpleFormalType))
    END (* IF *)
    
  ELSE (* no array prefix *)
    seenArray = FALSE
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
END simpleFormalType;


(* --------------------------------------------------------------------------
 * private function castingFormalType(astNode)
 * --------------------------------------------------------------------------
 * Parses rule castingFormalType, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * castingFormalType :=
 *   CAST ( OCTETSEQ | ADDRESS )
 *   ;
 *
 * astNode: (CASTP OctetSeq ) | (CASTP Address)
 * --------------------------------------------------------------------------
 *)
PROCEDURE castingFormalType ( VAR astNode : AstT ) : SymbolT;

VAR
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("castingFormalType");
  
  (* CAST *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* OCTETSEQ | ADDRESS *)
  CASE lookahead.token OF
    (* OCTETSEQ | *)
    Token.Octetseq :
    lookahead := Lexer.consumeSym(lexer);
    ftype := AST.NewNode(AstNodeType.OctetSeq);
    
    (* StdIdent="ADDRESS" *)
  | Token.StdIdent :
    IF lookahead.lexeme = Resword.address THEN
      lookahead := Lexer.consumeSym(lexer);
      ftype := AST.NewNode(AstNodeType.Address);
    END (* IF *)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(CastingFormalType))
  END; (* CASE *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.CastP, ftype)
  
  RETURN lookahead
END castingFormalType;


(* --------------------------------------------------------------------------
 * private function variadicFormalType(astNode)
 * --------------------------------------------------------------------------
 * Parses rule variadicFormalType, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * variadicFormalType :=
 *   ARGLIST OF simpleFormalType
 *   ;
 *
 * astNode: 
 * --------------------------------------------------------------------------
 *)
PROCEDURE variadicFormalType ( VAR astNode : AstT ) : SymbolT;

VAR
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("variadicFormalType");
  
  (* ARGLIST *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* OF *)
  IF matchToken(Token.Of) THEN
    lookahead := Lexer.consumeSym(lexer);
  ELSE (* resync *)
    lookahead :=
      skipToMatchSetOrSet(FIRST(Qualident), FOLLOW(VariadicFormalType))
  END (* IF *)
  
  (* simpleFormalType *)
  IF matchSet(FIRST(SimpleFormalType)) THEN
    lookahead := simpleFormalType(astNode)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(VariadicFormalType))
  END; (* IF *)
  
  RETURN lookahead
END variadicFormalType;


(* --------------------------------------------------------------------------
 * private function varDefinition(astNode)
 * --------------------------------------------------------------------------
 * Parses rule varDefinition, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * varDefinition :=
 *   identList ':' typeIdent
 *   ;
 *
 * astNode: (VARDECL idlist typeId)
 * --------------------------------------------------------------------------
 *)
PROCEDURE varDefinition ( VAR astNode : AstT ) : SymbolT;

VAR
  idlist, typeId : AstT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("varDefinition");
  
  (* identList *)
  lookahead := identList(idlist);
  
  (* ':' *)
  IF matchToken(Token.Colon) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead :=
      skipToMatchSetOrSet(FIRST(Qualident), FOLLOW(VarDefinition)))
  END; (* IF *)
  
  (* typeIdent *)
  IF matchSet(FIRST(Qualident)) THEN
    lookahead := qualident(typeId)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(VarDefinition))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.VarDecl, idlist, typeId);
  
  RETURN lookahead
END varDefinition;


(* --------------------------------------------------------------------------
 * private function procedureHeader(astNode)
 * --------------------------------------------------------------------------
 * Parses rule procedureHeader, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * procedureHeader :=
 *   PROCEDURE ( '[' bindingSpecifier ']' )? procedureSignature
 *   ;
 *
 * .procedureSignature :=
 *   ident ( '(' formalParams ( ';' formalParams )* ')' )?
 *   ( ':' returnedType )?
 *   ;
 *
 * astNode: (PROC bindSpec procId fplist retType)
 * --------------------------------------------------------------------------
 *)
PROCEDURE procedureHeader ( VAR astNode : AstT ) : SymbolT;

VAR
  bindSpec, procId, fpList, retType : AstT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("procedureHeader");
    
  (* PROCEDURE *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* ( '[' bindingSpecifier ']' )? *)
  IF lookahead.token = Token.LeftBracket THEN
    (* '[' *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* bindingSpecifier *)
    IF matchSet(FIRST(BindingSpecifier) THEN
      lookahead := bindingSpecifier(bindSpec)
    ELSE (* resync *)
      lookahead :=
        skipToMatchTokenOrSet(Token.RightBracket, FIRST(ProcedureSignature));
      bindSpec := AST.emptyNode()
    END; (* IF *)
    
    (* ']' *)
    IF matchToken(Token.RightBracket) THEN
      lookahead := Lexer.consumeSym(lexer)
    ELSE (* resync *)
      lookahead := skipToMatchSet(FIRST(ProcedureSignature))
    END (* IF *)
    
  ELSE (* no binding specifier *)
    bindSpec := AST.emptyNode()
  END; (* IF *)
  
  (* procedureSignature *)
  IF matchSet(FIRST(ProcedureSignature)) THEN
    (* ident *)
    IF matchSet(FIRST(Ident)) THEN
      lookahead := ident(procId)
    ELSE (* resync *)
      lookahead := skipToMatchTokenOrTokenOrSet
        (Token.LeftParen, Token.Colon, FOLLOW(ProcedureHeader))
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
          skipToMatchTokenOrSet(Token.Colon, FOLLOW(ProcedureHeader))
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
        lookahead := skipToMatchSet(FOLLOW(ProcedureHeader))
      END (* IF *)
      
    ELSE (* no return type *)
      retType := AST.emptyNode()
    END (* IF *)
    
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(ProcedureHeader))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.Proc, bindSpec, procId, fplist, retType);
  
  RETURN lookahead
END procedureHeader;


(* --------------------------------------------------------------------------
 * private function bindingSpecifier(astNode)
 * --------------------------------------------------------------------------
 * Parses rule bindingSpecifier, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * bindingSpecifier :=
 *   NEW ( argListFlag | capacityFlag )? | RETAIN | RELEASE |
 *   READ allocFlag? | WRITE formatFlag? | bindableIdent
 *   ;
 *
 * astNode: TO DO
 * --------------------------------------------------------------------------
 *)
PROCEDURE bindingSpecifier ( VAR astNode : AstT ) : SymbolT;

VAR
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("bindingSpecifier");
  
  lookahead := Lexer.lookaheadSym(lexer);
  
  CASE lookahead.Token OF
    (* NEW *)
    Token.New :
      lookahead := Lexer.consumeSym(lexer);
    
      (* ( arglistFlag | capacityFlag )? *)
      IF lookahead.token = Token.Plus THEN
        (* arglistFlag | *)
        lookahead := Lexer.consumeSym(lexer)
        
      (* capacityFlag *)
      ELSIF lookahead.token = Token.Asterisk THEN
        (* capacityFlag *)
        lookahead := Lexer.consumeSym(lexer)
            
      END; (* IF *)
      
      astNode := TO DO
      
    (* READ *)
  | Token.Read :
      lookahead := Lexer.consumeSym(lexer);
      
      (* allocFlag? *)
      IF token.lookahead = Token.Asterisk THEN
        lookahead := Lexer.consumeSym(lexer);
        
      END; (* IF *)
      
      astNode := TO DO
    
    (* RELEASE *)
  | Token.Release :
      lookahead := Lexer.consumeSym(lexer);
      
      astNode := TO DO
      
    (* RETAIN *)
  | Token.Retain :
      lookahead := Lexer.consumeSym(lexer);
      
      astNode := TO DO
      
    (* WRITE *)
  | Token.Write :
      lookahead := Lexer.consumeSym(lexer);
      
      (* formatFlag? *)
      IF token.lookahead = Token.Octothorpe THEN
        lookahead := Lexer.consumeSym(lexer);
        
      END; (* IF *)
      
      astNode := TO DO
      
    (* StdIdent *)
  | Token.StdIdent :
      lookahead := bindableIdent(astNode)
      
    (* Primitive *)
  | Token.Primitive :
      lookahead := bindableIdent(astNode)
    
  ELSE (* pre-conditions not met *)
    (* if we ever get here, then this will be a parser bug *)
    Halt
  END; (* CASE *)
  
  RETURN lookahead
END bindingSpecifier;


(* --------------------------------------------------------------------------
 * private function formalParams(astNode)
 * --------------------------------------------------------------------------
 * Parses rule formalParams, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * formalParams :=
 *   ( CONST | VAR )? identList ':' nonAttrFormalType
 *   ;
 *
 * astNode: (FPARAMS attr idlist ftype)
 * --------------------------------------------------------------------------
 *)
PROCEDURE formalParams ( VAR astNode : AstT ) : SymbolT;

VAR
  attr, idlist, ftype : AstT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("formalParams");
  
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* ( CONST | VAR )? *)
  CASE lookahead.token OF
    (* CONST | *)
    Token.Const :
      lookahead := Lexer.consumeSym(lexer);
      attr := TO DO
      
    (* VAR *)
  | Token.Var :
      lookahead := Lexer.consumeSym(lexer);
      attr := TO DO
      
  ELSE (* no attribute *)
    attr := AST.NewNode.emptyNode()
  END; (* CASE *)
  
  (* identList *)
  IF matchSet(FIRST(IdentList)) THEN
    lookahead := identList(idlist)
  ELSE (* resync *)
    idlist := NIL;
    lookahead := skipToMatchTokenOrSet(Token.Colon, FIRST(NonAttrFormalType))
  END; (* IF *)
  
  (* ':' *)
  IF matchToken(Token.Colon) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead :=
      skipToMatchSetOrSet(FIRST(NonAttrFormalType), FOLLOW(NonAttrFormalType))
  END; (* IF *)
  
  (* nonAttrFormalType *)
  IF matchSet(FIRST(NonAttrFormalType)) THEN
    lookahead := nonAttrFormalType(ftype)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(NonAttrFormalType))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.FParams, attr, idlist, ftype);
  
  RETURN lookahead
END formalParams;


(* --------------------------------------------------------------------------
 * private function programModule(astNode)
 * --------------------------------------------------------------------------
 * Parses rule programModule, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * programModule :=
 *   MODULE moduleIdent ';'
 *   privateImport* block moduleIdent '.'
 *   ;
 *
 * astNode: (PGMMOD moduleIdent implist blockNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE programModule ( VAR astNode : AstT ) : SymbolT;

VAR
  moduleIdent, implist, blockNode : AstT;
  ident1, ident2 : StringT;
  tmplist : AstQueueT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("programModule");
  
  (* MODULE *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* moduleIdent *)
  IF matchToken(Token.StdIdent) THEN
    ident1 := lookahead.lexeme;
    lookahead := ident(moduleident)
    
  ELSE (* resync *)
    lookahead :=
      skipToMatchTokenOrTokenOrSet
        (Token.Semicolon, Token.Import, FIRST(Block))
  END; (* IF *)
  
  (* ';' *)
  IF matchToken(Token.Semicolon) THEN
    lookahead := Lexer.consumeSym(lexer)
    
  ELSE (* resync *)
    lookahead := SkipToMatchTokenOrSet(Token.Import, FIRST(Block))
  END; (* IF *)
  
  tmplist := AstQueue.New();
  
  (* privateImport* *)
  WHILE lookahead.token = Token.Import DO
    (* function import parses both import and privateImport *)
    lookahead := import(implist);
    AstQueue.Enqueue(tmplist, implist)
  END; (* WHILE *)
  
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
  IF matchToken(Token.Ident) THEN
    ident2 := lookahead.lexeme;
    lookahead := ident(moduleIdent)

    IF ident1 # ident2 THEN
      (* TO DO: report error -- module identifiers don't match *) 
    END (* IF *)
    
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrToken(Token.Period, Token.EOF)
  END; (* IF *)
  
  (* '.' *)
  IF matchToken(Token.Period) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE
    lookahead := skipToMatchToken(Token.EOF)
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.PgmMod, moduleIdent, implist, blockNode)
  
  RETURN lookahead
END programModule;


(* --------------------------------------------------------------------------
 * private function block(astNode)
 * --------------------------------------------------------------------------
 * Parses rule block, constructs its AST node, passes the node back in
 * out-parameter astNode and returns the new lookahead symbol.
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

VAR
  decllist, stmtSeq : AstT;
  tmplist : AstQueueT;
  lookahead : SymbolT;
  
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
  
  (* BEGIN statementSequence *)
  IF matchToken(Token.Begin) THEN
    (* BEGIN *)
    lookahead := Lexer.consumeSym(lexer)
        
    (* statementSequence *)
    IF matchSet(FIRST(StatementSequence)) THEN
      lookahead := statementSequence(stmtSeq)
    ELSE (* resync *)
      lookahead := skipToMatchTokenOrSet(Token.End, FOLLOW(Block))
    END (* IF *)
    
  ELSE (* resync *)
    stmtSeq := NIL;
    lookahead := skipToMatchTokenOrSet(Token.End, FOLLOW(Block))
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
  
  RETURN lookahead
END block;


(* --------------------------------------------------------------------------
 * private function implementationModule(astNode)
 * --------------------------------------------------------------------------
 * Parses rule implementationModule, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * implementationModule :=
 *   IMPLEMENTATION MODULE moduleIdent ';'
 *   privateImport* possiblyEmptyBlock moduleIdent '.'
 *   ;
 *
 * alias privateImport = import ;
 *
 * astNode: (IMPMOD moduleIdent implist blockNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE implementationModule ( VAR astNode : AstT ) : SymbolT;

VAR
  moduleIdent, implist, blockNode : AstT;
  ident1, ident2 : StringT;
  tmplist : AstQueueT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("implementationModule");
  
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* IMPLEMENTATION *)
  IF matchToken(Token.Implementation) THEN
    lookahead := Lexer.consumeSym(lexer)
    
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrToken(TokenModule, Token.StdIdent)
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
    lookahead := skipToMatchTokenOrSet(Token.Semicolon, FIRST(PrivateImport))
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
    (* function import parses both import and privateImport *)
    lookahead := import(implist);
    AstQueue.Enqueue(tmplist, implist)
  END (* WHILE *)
  
  implist := AST.NewListNode(AstNodeType.ImpList, tmplist);
  tmplist:= AstQueue.ResetQueue(tmplist);
    
  (* possiblyEmptyBlock *)
  IF matchSet(FIRST(PossiblyEmptyBlock)) THEN
    lookahead := possiblyEmptyBlock(blockNode)
  ELSE (* resync *)
    lookahead :=
      skipToMatchTokenOrSet(Token.StdIdent, FIRST(PossiblyEmptyBlock));
    (* retry *)
    IF inFIRST(PossiblyEmptyBlock, lookahead.token) THEN
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
END implementationModule;


(* --------------------------------------------------------------------------
 * private function possiblyEmptyBlock(astNode)
 * --------------------------------------------------------------------------
 * Parses rule possiblyEmptyBlock, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * possiblyEmptyBlock :=
 *   declaration*
 *   ( BEGIN statementSequence )? END
 *   ;
 *
 * astNode: (BLOCK declListNode stmtSeqNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE possiblyEmptyBlock ( VAR astNode : AstT ) : SymbolT;

VAR
  decllist, stmtSeq : AstT;
  tmplist : AstQueueT;
  lookahead : SymbolT;
  
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
  
  RETURN lookahead
END possiblyEmptyBlock;


(* --------------------------------------------------------------------------
 * private function declaration(astNode)
 * --------------------------------------------------------------------------
 * Parses rule declaration, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * declaration :=
 *   CONST ( constDeclaration ';' )+ |
 *   TYPE ( typeDeclaration ';' )+ |
 *   VAR ( varDeclaration ';' )+ |
 *   procedureHeader ';' block ident ';' |
 *   aliasDeclaration ';' |
 *   toDoList ';'
 *   ;
 *
 * alias constDeclaration = constDefinition ;
 *
 * alias varDeclaration = fieldList ;
 *
 * astNode: (DECLLIST declNode1 declNode2 ... declNodeN)
 * --------------------------------------------------------------------------
 *)
PROCEDURE declaration ( VAR astNode : AstT ) : SymbolT;

VAR
  lookahead : SymbolT;

BEGIN
  PARSER_DEBUG_INFO("declaration");

  lookahead := Lexer.lookaheadSym(lexer);
  
  CASE lookahead.token OF
  (* CONST *)
    Token.Const :
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
      lookahead := procDeclaration(astNode)
  
  (* aliasDeclaration ';' | *)
  | Token.Unqualified :
    (* aliasDeclaration *)
    lookahead := aliasDeclaration(astNode)
    
    (* ';' *)
    IF matchToken(Token.Semicolon) THEN
      lookahead := Lexer.consumeSym(lexer)
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(Declaration))
    END (* IF *)
    
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
 * private function typeDeclaration(astNode)
 * --------------------------------------------------------------------------
 * Parses rule typeDeclaration, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * typeDeclaration :=
 *   ident '=' privateType
 *   ;
 *
 * astNode: (TYPEDECL identNode typeNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE typeDeclaration ( VAR astNode : AstT ) : SymbolT;

VAR
  typeId, typeDecl : AstT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("typeDeclaration");
  
  (* ident *)
  lookahead := ident(typeId);
  
  (* '=' *)
  IF matchToken(Token.Equal) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    (* we assume '=' is simply missing *)
  END; (* IF *)
  
  (* privateType *)
  IF matchSet(FIRST(PrivateType)) THEN
    (* function type parses both type and privateType *)
    lookahead := type(typeDecl)
    
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(TypeDeclaration))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.TypeDecl, typeId, typeDecl);
  
  RETURN lookahead
END typeDeclaration;


(* --------------------------------------------------------------------------
 * private function octetSeqType(astNode)
 * --------------------------------------------------------------------------
 * Parses rule octetSeqType, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * octetSeqType :=
 *   OCTETSEQ '[' valueCount ']'
 *   ;
 *
 * astNode: TO DO
 * --------------------------------------------------------------------------
 *)
PROCEDURE octetSeqType ( VAR astNode : AstT ) : SymbolT;

VAR
  lookahead : SymbolT;

BEGIN
  PARSER_DEBUG_INFO("octetSeqType");
  
  (* OCTETSEQ *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* '[' *)
  IF matchToken(Token.LeftBracket) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead :=
      skipToMatchSetOrSet(FIRST(ConstExpression), FOLLOW(TypeDeclaration))
  END; (* IF *)
  
  (* valueCount *)
  IF matchSet(FIRST(ConstExpression)) THEN
    lookahead := constExpression(astNode)
  ELSE (* resync *)
    lookahead :=
      skipToMatchTokenOrSet(Token.RightBracket, FOLLOW(TypeDeclaration))
  END; (* IF *)
  
  (* ']' *)
  IF matchToken(Token.RightBracket) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(TypeDeclaration))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := TO DO
  
  RETURN lookahead
END octetSeqType;


(* --------------------------------------------------------------------------
 * private function indeterminateTarget(astNode)
 * --------------------------------------------------------------------------
 * Parses rule indeterminateTarget, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * indeterminateTarget :=
 *   RECORD ( fieldList ';' )* indeterminateField END
 *   ;
 *
 * astNode: (INREC fieldListNode identNode identNode qualidentNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE indeterminateTarget ( VAR astNode : AstT ) : SymbolT;

VAR
  listseq, inField : AstT;
  lookahead : SymbolT;

BEGIN
  PARSER_DEBUG_INFO("indeterminateTarget");
    
  (* RECORD *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* ( fieldList ';' )* *)
  IF inFIRST(FieldList, lookahead.token) THEN
    lookahead :=
      parseListWTerminator(fieldList, Token.Semicolon,
        FIRST(FieldList), FIRST(IndeterminateField),
        AstNodeType.FieldListSeq, listseq)
  END; (* IF *)
  
  (* indeterminateField *)
  IF matchSet(FIRST(IndeterminateField)) THEN
    lookahead := indeterminateField(inField)
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrSet(Token.End, FOLLOW(IndeterminateTarget))
  END; (* IF *)
  
  (* END *)
  IF matchToken(Token.End) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(IndeterminateTarget))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.InRec, listseq, inField);
  
  RETURN lookahead
END indeterminateTarget;


(* --------------------------------------------------------------------------
 * private function indeterminateField(astNode)
 * --------------------------------------------------------------------------
 * Parses rule indeterminateField, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * indeterminateField :=
 *   '+' ident ':' ARRAY capacityField OF typeIdent
 *   ;
 *
 * alias capacityField = ident ;
 *
 * astNode: (TERMLIST identNode identNode qualidentNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE indeterminateField ( VAR astNode : AstT ) : SymbolT;

VAR
  fieldId, capacityId, typeId : AstT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("indeterminateField");
  
  (* '+' *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* ident *)
  IF matchSet(FIRST(Ident)) THEN
    lookahead := ident(fieldId)
  ELSE (* resync *)
    lookahead :=
      skipToMatchTokenOrSet(Token.Colon, FOLLOW(IndeterminateField))
  END; (* IF *)

  (* ':' *)
  IF matchToken(Token.Colon) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead :=
      skipToMatchTokenOrSet(Token.Array, FIRST(Ident))
  END; (* IF *)
  
  (* ARRAY *)
  IF matchToken(Token.Array) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead :=
      skipToMatchTokenOrSet(Token.Of, FIRST(Ident))
  END; (* IF *)
  
  (* discriminantFieldIdent *)
  IF matchSet(FIRST(Ident)) THEN
    lookahead := ident(capacityId)
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
      skipToMatchTokenOrSet(Token.Of, FOLLOW(IndeterminateField))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.InField, fieldId, capacityId, typeId);
  
  RETURN lookahead
END indeterminateField;


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
  lookahead : SymbolT;
  
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
    lookahead :=
      skipToMatchTokenOrSet(Token.Semicolon, FOLLOW(ProcDeclaration))
  END (* IF *)
  
  (* ';' *)
  IF matchToken(Token.Semicolon, FOLLOW(ProcDeclaration)) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(ProcDeclaration))
  END (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.ProcDecl, procHeader, blockNode);
  
  RETURN lookahead
END procDeclaration;


(* --------------------------------------------------------------------------
 * private function aliasDeclaration(astNode)
 * --------------------------------------------------------------------------
 * Parses rule aliasDeclaration, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * aliasDeclaration :=
 *   UNQUALIFIED nameSelector ( ',' nameSelector )*
 *   ;
 *
 * astNode:
 *  (ALIASDECL identNode qualidentNode) |
 *  (ALIASDECLLIST aliasDeclNode1 aliasDeclNode2 ... aliasDeclNodeN)
 * --------------------------------------------------------------------------
 *)
PROCEDURE aliasDeclaration ( VAR astNode : AstT ) : SymbolT;

VAR
  name : AstT;
  tmplist : AstQueueT;
  lookahead : SymbolT;

BEGIN
  PARSER_DEBUG_INFO("aliasDeclSection");
  
  AstQueue.New(tmplist);
  
  (* UNQUALIFIED *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* nameSelector *)
  IF matchToken(Token.Qualident) THEN
    lookahead := nameSelector(name);
    AstQueue.Enqueue(tmplist, name)
    
    (* ( ',' nameSelector )* *)
    WHILE lookahead.token = Token.Comma DO
      (* ',' *)
      lookahead := Lexer.consumeSym(lexer);
      
      (* nameSelector *)
      IF matchToken(Token.Qualident) THEN
        lookahead := nameSelector(name);
        AstQueue.Enqueue(tmplist, name)
        
      ELSE (* resync *)
        lookahead :=
          skipToMatchTokenOrSetOrSet
            (Token.Comma), FIRST(Declaration), FOLLOW(Declaration)
      END (* IF *)
    END; (* WHILE *)
    
  ELSE (* resync *)
    lookahead := skipToMatchSetOrSet(FIRST(Declaration), FOLLOW(Declaration))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewListNode(AstNodeType.AliasDeclList, tmplist)

  RETURN lookahead
END aliasDeclaration;


(* --------------------------------------------------------------------------
 * private function nameSelector(astNode)
 * --------------------------------------------------------------------------
 * Parses rule nameSelector, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * nameSelector :=
 *   qualident wildcard?
 *   ;
 *
 * alias wildcard = '.*' ;
 *
 * astNode: (ALIASDECL (IDENT "*") qualidentNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE nameSelector ( VAR astNode : AstT ) : SymbolT;

VAR
  name : AstT;
  wildcard : BOOLEAN;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("nameSelector");

  (* qualident *)
  lookahead := qualident(name);
  
  (* wildcard? *)
  IF lookahead.token := Token.DotStar THEN
    wildcard := TRUE
  ELSE
    wildcard := FALSE
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(NodeType.AliasDecl, name, wildcard);
  
  RETURN lookahead
END nameSelector;


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
  lookahead : SymbolT;
  
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
 *   memMgtOperation | updateOrProcCall | returnStatement |
 *   copyStatement | readStatement | writeStateent | ifStatement |
 *   caseStatement | loopStatement | whileStatement | repeatStatement |
 *   forStatement | toDoList | EXIT | NOP
 *   ;
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
  
  (* case labels in order of their ordinal values *)
  CASE lookahead.token OF
  (* caseStatement | *)
    Token.Case : lookahead := caseStatement(astNode)
    
  (* copyStatement | *)
  | Token.Copy : lookahead := copyStatement(astNode)
    
  (* EXIT | *)
  | Token.Exit :
      lookahead := Lexer.consumeSym(lexer);
      astNode := AST.NewNode(AstNodeType.Exit)
      
  (* forStatement | *)
  | Token.For : lookahead := forStatement(astNode)
    
  (* ifStatement | *)
  | Token.If : lookahead := ifStatement(astNode)
    
  (* loopStatement | *)
  | Token.Loop : lookahead := loopStatement(astNode)
    
  (* newStatement | *)
  | Token.New : lookahead := newStatement(astNode)
    
  (* NOP | *)
  | Token.Nop :
      lookahead := Lexer.consumeSym(lexer);
      astNode := AST.NewNode(AstNodeType.Nop)
      
  (* readStatement | *)
  | Token.Read : lookahead := readStatement(astNode)
    
  (* releaseStatement | *)
  | Token.Release : lookahead := releaseStatement(astNode)
    
  (* repeatStatement | *)
  | Token.Repeat : lookahead := repeatStatement(astNode)
    
  (* retainStatement | *)
  | Token.Retain : lookahead := retainStatement(astNode)
    
  (* returnStatement | *)
  | Token.Return : lookahead := returnStatement(astNode)
    
  (* toDoList | *)
  | Token.To : lookahead := toDoList(astNode)
      
  (* whileStatement | *)
  | Token.While : lookahead := whileStatement(astNode)
    
  (* writeStatement | *)
  | Token.Write : lookahead := writeStatement(astNode)
    
  ELSE
    (* updateOrProcCall *)
    IF matchSet(FIRST(Designator) THEN
      lookahead := updateOrProcCall(astNode)
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(Statement))
    END (* IF *)
  END; (* CASE *)
  
  RETURN lookahead
END statement;


(* --------------------------------------------------------------------------
 * private function newStatement(astNode)
 * --------------------------------------------------------------------------
 * Parses rule newStatement, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * newStatement :=
 *   NEW designator ( ':=' expression | CAPACITY expression )?
 *   ;
 *
 * astNode: (NEW desigNode exprNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE newStatement ( VAR astNode : AstT ) : SymbolT;

VAR
  desig, expr : AstT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("newStatement");
  
  (* NEW *)
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* designator *)
  IF matchSet(FIRST(Designator) THEN
    lookahead := designator(desig)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(Statement))
  END; (* IF *)
  
  (* ( ':=' expression )? | *)
  IF lookahead.token = Token.Assign THEN
    (* ':=' *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* expression *)
    IF matchSet(FIRST(Expression) THEN
      lookahead := expression(expr)
    ELSE (* resync *)
      lookahead := skipToMatch(FOLLOW(Statement))
    END (* IF *)
    
  (* ( CAPACITY expression )? *)
  ELSIF (lookahead.token = Token.StdIdent) AND
        (lookahead.lexeme = Resword.Capacity) THEN
    (* CAPACITY *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* expression *)
    IF matchSet(FIRST(Expression) THEN
      lookahead := expression(expr)
    ELSE (* resync *)
      lookahead := skipToMatch(FOLLOW(Statement))
    END (* IF *)
    
  ELSE (* no tail component *)
    expr := AST.emptyNode()
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(NodeType.New, desig, expr);
  
  RETURN lookahead
END newStatement;


(* --------------------------------------------------------------------------
 * private function retainStatement(astNode)
 * --------------------------------------------------------------------------
 * Parses rule retainStatement, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * retainStatement :=
 *   RETAIN designator
 *   ;
 *
 * astNode: (RETAIN desigNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE retainStatement ( VAR astNode : AstT ) : SymbolT;

VAR
  desig : AstT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("retainStatement");
  
  (* RETAIN *)
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* designator *)
  IF matchSet(FIRST(Designator) THEN
    lookahead := designator(desig)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(Statement))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(NodeType.Retain, desig);
  
  RETURN lookahead
END retainStatement;


(* --------------------------------------------------------------------------
 * private function releaseStatement(astNode)
 * --------------------------------------------------------------------------
 * Parses rule releaseStatement, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * releaseStatement :=
 *   RELEASE designator
 *   ;
 *
 * astNode: (RELEASE desigNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE releaseStatement ( VAR astNode : AstT ) : SymbolT;

VAR
  desig : AstT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("releaseStatement");
  
  (* RELEASE *)
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* designator *)
  IF matchSet(FIRST(Designator) THEN
    lookahead := designator(desig)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(Statement))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(NodeType.Release, desig);
  
  RETURN lookahead
END releaseStatement;


(* --------------------------------------------------------------------------
 * private type DesignatorVariant
 * --------------------------------------------------------------------------
 *)
TYPE DesignatorVariant = ( Common, Target );


(* --------------------------------------------------------------------------
 * private function updateOrProcCall(astNode)
 * --------------------------------------------------------------------------
 * Parses rule updateOrProcCall, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * updateOrProcCall :=
 *   designator
 *     ( IncOrDecSuffix | '(' expressionList ')' | ':=' expression )? |
 *   targetDesignator
 *     ':=' expression
 *   ;
 *
 * .IncOrDecSuffix := '++' | '--' ;
 *
 * astNode: (ASSIGN desigNode exprNode) | (PCALL exprListNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE updateOrProcCall ( VAR astNode : AstT ) : SymbolT;

VAR
  desig, expr, exprList : AstT;
  variant : DesignatorVariant;
  lookahead : SymbolT;

BEGIN
  PARSER_DEBUG_INFO("updateOrProcCall");
  
  (* designator | targetDesignator *)
  lookahead := targetDesignator(variant, desig);
  
  CASE variant OF
  (* designator *) 
    Common :
    
    (* ( '++' | '--' | '(' expressionList ')' )? *)
    CASE lookahead.token OF
    (* '++' | *)
      Token.PlusPlus :
      lookahead := Lexer.consumeSym(lexer);
            
      (* build AST node and pass it back in astNode *)
      astNode := AST.NewNode(AstNodeType.Incr, desig)
      
    (* '--' | *)
    | Token.MinusMinus :
      lookahead := Lexer.consumeSym(lexer);
      
      (* build AST node and pass it back in astNode *)
      astNode := AST.NewNode(AstNodeType.Decr, desig)
      
    (* '(' expressionList ')' *)
    | Token.LeftParen :
      (* '(' *)
      lookahead := Lexer.consumeSym(lexer);
      
      (* expressionList *)
      IF matchSet(FIRST(ExpressionList)) THEN
        lookahead := expressionList(exprList)
      ELSE (* resync *)
        lookahead :=
          skipToMatchTokenOrSet(Token.RightParen, FOLLOW(Statement))
      END; (* IF *)
      
      (* ')' *)
      IF matchToken(Token.RightParen) THEN
        lookahead := Lexer.consumeSym(lexer)
      ELSE (* resync *)
        lookahead := skipToMatchSet(FOLLOW(Statement))
      END; (* IF *)
      
      (* build AST node and pass it back in astNode *)
      astNode := AST.NewNode(AstNodeType.PCall, desig, exprList)
      
    (* ':=' expression *)
    | Token.Assign :
      lookahead := assignmentTail(expr)
      
      (* build AST node and pass it back in astNode *)
      astNode := AST.NewNode(AstNodeType.Assign, desig, expr)
      
    ELSE (* sole designator *)
      astNode := desig
    END (* CASE *)
    
  (* targetDesignator *)
  | Target :
    
    (* ':=' expression *)
    IF matchToken(Token.Assign) THEN
      lookahead := assignmentTail(expr)
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(Statement))
    END; (* IF *)

    (* build AST node and pass it back in astNode *)
    astNode := AST.NewNode(AstNodeType.Assign, desig, expr)
    
  END; (* CASE *)
    
  RETURN lookahead
END updateOrProcCall;


(* --------------------------------------------------------------------------
 * private function assignmentTail(astNode)
 * --------------------------------------------------------------------------
 * Parses rule assignmentTail, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * assignmentTail :=
 *   ':=' expression
 *   ;
 *
 * astNode: exprNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE assignmentTail ( astNode : AstT ) : SymbolT;

VAR
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("assignmentTail");
  
  (* ':=' *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* expression *)
    IF matchSet(FIRST(Expression)) THEN
      lookahead := expression(astNode)
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(Statement))
    END; (* IF *)
    
  RETURN lookahead
END assignmentTail;


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
  ELSE (* no return value *)
    expr := AST.emptyNode()
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.Return, expr);
  
  RETURN lookahead
END returnStatement;


(* --------------------------------------------------------------------------
 * private function copyStatement(astNode)
 * --------------------------------------------------------------------------
 * Parses rule copyStatement, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * copyStatement :=
 *   COPY targetDesignator ':=' expression
 *   ;
 *
 * astNode: (COPY desigNode exprNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE copyStatement ( VAR astNode : AstT ) : SymbolT;

VAR
  desig, expr : AstT;
  lookahead : SymbolT;
  variant : DesignatorVariant;
  
BEGIN
  PARSER_DEBUG_INFO("copyStatement");
  
  (* COPY *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* targetDesignator *)
  IF matchSet(FIRST(Designator)) THEN
    lookahead := targetDesignator(variant, desig)
  ELSE (* resync *)
    lookahead := skipToMatchSetOrSet(FIRST(Designator), FOLLOW(Designator))
  END; (* IF *)
  
  (* ':=' *)
  IF matchToken(Token.Assign) THEN
    lookahead := assignmentTail(expr)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(Statement))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.Copy, desig, expr);
  
  RETURN lookahead
END copyStatement;


(* --------------------------------------------------------------------------
 * private function readStatement(astNode)
 * --------------------------------------------------------------------------
 * Parses rule readStatement, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * readStatement :=
 *   READ ( '@' chan ':' )?
 *   inputArg ( ',' inputArg )*
 *   ;
 *
 * alias chan = designator ;
 *
 * astNode: (READ desigNode desigListNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE readStatement ( VAR astNode : AstT ) : SymbolT;

VAR
  chan, arg : AstT;
  arglist : AstQueueT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("readStatement");
  
  (* READ *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* ( '@' chan ':' )? *)
  IF lookahead.token = Token.Octothorpe THEN
    lookahead := designator(Common, chan)
  ELSE (* no channel *)
    chan := AST.emptyNode()
  END; (* IF *)
  
  AstQueue.New(arglist);
  
  (* inputArg *)
  IF matchSet(FIRST(InputArg)) THEN
    lookahead := inputArg(arg);
    AstQueue.Enqueue(arglist, arg)
  ELSE (* resync *)
    lookahead :=
      skipToMatchTokenOrSetOrSet
        (Token.Comma, FIRST(InputArg), FOLLOW(InputArg))
  END; (* IF *)
  
  (* ( ',' inputArg )* *)
  WHILE lookahead.token = Token.Comma DO
    (* ',' *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* inputArg *)
    IF matchSet(FIRST(InputArg)) THEN
      lookahead := inputArg(arg);
      AstQueue.Enqueue(arglist, arg)
    ELSE (* resync *)
      lookahead :=
        skipToMatchTokenOrSetOrSet
          (Token.Comma, FIRST(InputArg), FOLLOW(InputArg))
    END (* IF *)
  END; (* WHILE *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.Read, chan, arglist);
  
  RETURN lookahead
END readStatement;


(* --------------------------------------------------------------------------
 * private function inputArg(astNode)
 * --------------------------------------------------------------------------
 * Parses rule inputArg, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * inputArg := NEW? designator ; *   ;
 *
 * astNode: TO DO
 * --------------------------------------------------------------------------
 *)
PROCEDURE inputArg ( VAR astNode : AstT ) : SymbolT;

VAR
  desig : AstT;
  newAttr : BOOLEAN;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("inputArg");
  
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* NEW? *)
  IF lookahead.token = Token.New THEN
    lookahead := Lexer.consumeSym(lexer);
    newAttr := TRUE
  ELSE
    newAttr := FALSE
  END; (* IF *)
  
  (* designator *)
  IF matchSet(FIRST(Designator)) THEN
    lookahead := designator(Common, desig)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(InputArg))
  END; (* IF *)
  
  astNode := TO DO
  
  RETURN lookahead
END inputArg;


(* --------------------------------------------------------------------------
 * private function writeStatement(astNode)
 * --------------------------------------------------------------------------
 * Parses rule writeStatement, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * writeStatement :=
 *   WRITE ( '@' chan ':' )?
 *   outputArgs ( ',' outputArgs )*
 *   ;
 *
 * alias chan = designator ;
 *
 * astNode: (WRITE desigNode desigListNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE writeStatement ( VAR astNode : AstT ) : SymbolT;

VAR
  chan, arg : AstT;
  arglist : AstQueueT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("writeStatement");
  
  (* READ *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* ( '@' chan ':' )? *)
  IF lookahead.token = Token.Octothorpe THEN
    lookahead := designator(Common, chan)
  ELSE (* no channel *)
    chan := AST.emptyNode()
  END; (* IF *)
  
  AstQueue.New(arglist);
  
  (* outputArgs *)
  IF matchSet(FIRST(OutputArgs)) THEN
    lookahead := outputArgs(arg);
    AstQueue.Enqueue(arglist, arg)
  ELSE (* resync *)
    lookahead :=
      skipToMatchTokenOrSetOrSet
        (Token.Comma, FIRST(OutputArgs), FOLLOW(OutputArgs))
  END; (* IF *)
  
  (* ( ',' outputArgs )* *)
  WHILE lookahead.token = Token.Comma DO
    (* ',' *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* outputArgs *)
    IF matchSet(FIRST(OutputArgs)) THEN
      lookahead := outputArgs(arg);
      AstQueue.Enqueue(arglist, arg)
    ELSE (* resync *)
      lookahead :=
        skipToMatchTokenOrSetOrSet
          (Token.Comma, FIRST(OutputArgs), FOLLOW(OutputArgs))
    END (* IF *)
  END; (* WHILE *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.Write, chan, arglist);
  
  RETURN lookahead
END writeStatement;


(* --------------------------------------------------------------------------
 * private function outputArgs(astNode)
 * --------------------------------------------------------------------------
 * Parses rule outputArgs, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * outputArgs := formattedArgs | unformattedArg ;
 *
 * alias unformattedArg = expression ;
 *
 * astNode: TO DO
 * --------------------------------------------------------------------------
 *)
PROCEDURE outputArgs ( VAR astNode : AstT ) : SymbolT;

VAR
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("outputArgs");
  
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* formattedArgs *)
  IF lookahead.token = Token.Octothorpe THEN
    lookahead := formattedArgs(astNode)
    
  (* unformattedArg *)
  ELSE (* lookahead.token # Octothorpe *)
    lookahead := expression(astNode)
  END; (* IF *)
  
  RETURN lookahead
END outputArgs;


(* --------------------------------------------------------------------------
 * private function formattedArgs(astNode)
 * --------------------------------------------------------------------------
 * Parses rule formattedArgs, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * formattedArgs :=
 *   '#' '(' fmtStr, unformattedArg ')'
 *   ;
 *
 * alias fmtStr = expression ;
 *
 * astNode: TO DO
 * --------------------------------------------------------------------------
 *)
PROCEDURE formattedArgs ( VAR astNode : AstT ) : SymbolT;

VAR
  fmtstr, arglist : AstT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("formattedArgs");
  
  (* '#' *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* '(' *)
  IF matchToken(Token.LeftParen) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead :=
      skipToMatchTokenOrSetOrSet
        (Token.Comma, FIRST(Expression), FOLLOW(OutputArgs))
  END; (* IF *)
  
  (* fmtStr *)
  IF matchSet(FIRST(Expression)) THEN
    lookahead := expression(fmtstr)
  ELSE (* resync *)
    lookahead :=
      skipToMatchTokenOrSetOrSet
        (Token.Comma, FIRST(Expression), FOLLOW(OutputArgs))
  END; (* IF *)
  
  (* ',' *)
  IF matchToken(Token.Comma) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead :=
      skipToMatchTokenOrSetOrSet
        (Token.RightParen, FIRST(Expression), FOLLOW(OutputArgs))
  END; (* IF *)
  
  (* unformattedArgs *)
  IF matchSet(FIRST(Expression) THEN
    lookahead := expressionList(arglist)
  
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrSet(Token.RightParen, FOLLOW(OutputArgs))
  END; (* IF *)
  
  (* ')' *)
  IF matchToken() THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead := skipToMatchSet(FOLLOW(OutputArgs))
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.FmtArgs, fmtstr, arglist);
  
  RETURN lookahead
END formattedArgs;


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
      lookahead := skipToMatchSet(FOLLOW(CaseLabels))
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
 *   accessor descender? ( ',' value )?
 *   ;
 *
 * alias accessor, value = ident ;
 *
 * alias descender = '--' ;
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
  
  (* descender? *)
  IF lookahead.token # Token.MinusMinus THEN
    accessor := AST.NewNode(AstNodeType.Asc, accessor)
  ELSE (* '--' *)
    lookahead := Lexer.consumeSym(lexer);
    accessor := AST.NewNode(AstNodeType.Desc, accessor)
  END; (* IF *)
  
  (* ( ',' value )? *)
  IF lookahead.token = Token.Comma THEN
    (* ',' *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* value *)
    IF matchSet(FIRST(Ident)) THEN
      lookahead := ident(value)
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(ForLoopVariants))
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
 *   collectionOrType valueRange?
 *   ;
 *
 * alias collectionOrType = qualident ;
 *
 * astNode: desigNode | rangeNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE iterableExpr ( VAR astNode : AstT ) : SymbolT;

VAR
  identNode, rangeNode : AstT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("iterableExpr");
  
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* collectionOrType *)
  IF matchSet(FIRST(Qualident)) THEN
    lookahead := qualident(identNode)
  ELSE (* resync *)
    lookahead := skipToMatch()
  END; (* IF *)
  
  (* valueRange? *)
  IF lookahead.token = Token.LeftBracket THEN
    lookahead := valueRange(rangeNode)
  ELSE (* no value range *)
    rangeNode := AST.emptyNode()
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.Iter, identNode, rangeNode);
  
  RETURN lookahead
END iterableExpr;


(* --------------------------------------------------------------------------
 * private function designator(astNode)
 * --------------------------------------------------------------------------
 * Parses rule designator, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * designator :=
 *   qualident ( derefTail | subscriptTail )?
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
  
  (* qualident *)
  lookahead := qualident(head);
    
  (* ( derefTail | subscriptTail )? *)
  IF lookahead.token = Token.Deref THEN
    lookahead := derefTail(tail)
    
  ELSIF lookahead.token = Token.LeftBracket THEN
    lookahead := subscriptTail(tail)
    
  ELSE (* no tail *)
    tail := AST.emptyNode()
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.Desig, head, tail);
  
  RETURN lookahead
END designator;


(* --------------------------------------------------------------------------
 * private function derefTail(mode, astNode)
 * --------------------------------------------------------------------------
 * Parses rule derefTail, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * derefTail :=
 *   deref ( '.' designator | subscriptTail )?
 *   ;
 *
 * astNode: (DESIG head tail)
 * --------------------------------------------------------------------------
 *)
PROCEDURE derefTail ( VAR astNode : AstT ) : SymbolT;

VAR
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("derefTail");
  
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* deref *)
  WHILE lookahead.token = Token.Deref DO
    lookahead := Lexer.consumeSym(lexer);
    
  END; (* WHILE *)
  
  (* ( '.' designator | subscriptTail )? *)
  IF lookahead.token = Token.Period THEN
    (* '.' *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* designator *)
    IF matchSet(FIRST(Designator)) THEN
      lookahead := designator(astNode)
    ELSE (* resync *)
      lookahead := skipToMatchTokenOrSet(Token.Period, FOLLOW(DerefTail))
    END (* IF *)
    
  (* subscriptTail *)
  ELSIF lookahead.token = Token.LeftBracket THEN
    lookahead := subscriptTail(astNode)
  ELSE (* no tail *)
    astNode := TO DO
  END; (* IF *)
  
  RETURN lookahead
END derefTail;


(* --------------------------------------------------------------------------
 * private function subscriptTail(astNode)
 * --------------------------------------------------------------------------
 * Parses rule subscriptTail, constructs its AST node, passes the node back
 * in out-parameter astNode and returns the new lookahead symbol.
 *
 * subscriptTail :=
 *   '[' expression ']' ( '.' designator | derefTail )?
 *   ;
 *
 * astNode: (SUBSCR subscript tail)
 * --------------------------------------------------------------------------
 *)
PROCEDURE subscriptTail ( VAR astNode : AstT ) : SymbolT;

VAR
  subscript, tail : AstT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("subscriptTail");
    
  (* '[' *)
  lookahead := Lexer.consumeSym(lexer);
    
  (* expression *)
  IF matchSet(FIRST(Expression)) THEN
    lookahead := expression(subscript)
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrToken(Token.RightBracket, Token.DotDot)
  END; (* IF *)
  
  (* ']' *)
  IF matchToken(Token.RightBracket) THEN
    lookahead := Lexer.consumeSym(lexer)
  ELSE (* resync *)
    lookahead :=
      skipToMatchTokenOrTokenOrSet
        (Token.Period, Token.Deref, FOLLOW(SubscriptTail))
  END; (* IF *)
    
  (* ( '.' designator | derefTail )? *)
  CASE lookahead.token OF
  (* '.' designator | *)
    Token.Period :
      lookahead := Lexer.consumeSym(lexer);
      
      (* designator *)
      IF matchSet(FIRST(Designator)) THEN
        lookahead := designator(mode, tail)
      ELSE (* resync *)
        lookahead := skipToMatchSet(FOLLOW(SubscriptTail)
      END (* IF *)
    
  (* derefTail *)
  | Token.Deref :
      lookahead := derefTail(tail)
  END (* CASE *)  
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.SubTail, subscript, tail)
  
  RETURN lookahead
END subscriptTail;


(* --------------------------------------------------------------------------
 * private function targetDesignator(variant, astNode)
 * --------------------------------------------------------------------------
 * Parses rule targetDesignator, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * Upon return, out-parameter variant indicates the exit path taken:
 * (1) value Common indicates that the '[' expression ']' path was taken
 * (2) value Target indicates that the '[' expression '..' path was taken
 *
 * targetDesignator :=
 *   qualident ( derefTargetTail | bracketTargetTail )?
 *   ;
 *
 * astNode: (DESIG head tail)
 * --------------------------------------------------------------------------
 *)
PROCEDURE targetDesignator
  ( VAR (*OUT*) variant : DesignatorVariant; VAR astNode : AstT ) : SymbolT;
  
VAR
  head, tail : AstT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("targetDesignator");
  
  (* default variant *)
  variant := Common;
  
  (* qualident *)
  lookahead := qualident(head);
    
  (* ( derefTargetTail | bracketTargetTail )? *)
  IF lookahead.token = Token.Deref THEN
    lookahead := derefTargetTail(variant, tail)
    
  ELSIF lookahead.token = Token.LeftBracket THEN
    lookahead := bracketTargetTail(variant, tail)
    
  ELSE (* no tail *)
    tail := AST.emptyNode()
  END; (* IF *)
  
  (* build AST node and pass it back in astNode *)
  astNode := AST.NewNode(AstNodeType.Desig, head, tail);
    
  RETURN lookahead
END targetDesignator;


(* --------------------------------------------------------------------------
 * private function derefTargetTail(variant, astNode)
 * --------------------------------------------------------------------------
 * Parses rule derefTargetTail, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * Upon return, out-parameter variant indicates the exit path taken:
 * (1) value Common indicates that the '[' expression ']' path was taken
 * (2) value Target indicates that the '[' expression '..' path was taken
 *
 * derefTargetTail :=
 *   deref ( '.' targetDesignator | bracketTargetTail )?
 *   ;
 *
 * astNode: (DESIG head tail)
 * --------------------------------------------------------------------------
 *)
PROCEDURE derefTargetTail
  ( VAR (*OUT*) variant : DesignatorVariant; VAR astNode : AstT ) : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("derefTargetTail");
  
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* deref *)
  WHILE lookahead.token = Token.Deref DO
    lookahead := Lexer.consumeSym(lexer);
    
  END; (* WHILE *)
  
  (* ( '.' targetDesignator | bracketTargetTail )? *)
  IF lookahead.token = Token.Period THEN
    (* '.' *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* targetDesignator *)
    IF matchSet(FIRST(Designator)) THEN
      lookahead := targetDesignator(variant, astNode)
    ELSE (* resync *)
      lookahead := skipToMatchTokenOrSet(Token.Period, FOLLOW(DerefTail))
    END (* IF *)
    
  (* bracketTargetTail *)
  ELSIF lookahead.token = Token.LeftBracket THEN
    lookahead := bracketTargetTail(variant, astNode)
  ELSE (* no tail *)
    astNode := TO DO
  END; (* IF *)
  
  RETURN lookahead
END derefTargetTail;


(* --------------------------------------------------------------------------
 * private function bracketTargetTail(variant, astNode)
 * --------------------------------------------------------------------------
 * Parses rule bracketTargetTail, constructs its AST node, passes the node
 * back in out-parameter astNode and returns the new lookahead symbol.
 *
 * Upon return, out-parameter variant indicates the exit path taken:
 * (1) value Common indicates that the '[' expression ']' path was taken
 * (2) value Target indicates that the '[' expression '..' path was taken
 *
 * bracketTargetTail :=
 *   '[' expression
 *     ( ']' ( '.' targetDesignator | derefTargetTail )? |
 *       '..' expression? ']' )
 *   ;
 *
 * astNode: (SUBSCR subscript tail) | (INS index) | (SLICE start end)
 * --------------------------------------------------------------------------
 *)
PROCEDURE bracketTargetTail
  ( VAR (*OUT*) variant : DesignatorVariant; VAR astNode : AstT ) : SymbolT;

VAR
  expr1, expr2, tail : AstT;
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("bracketTargetTail");
    
  (* '[' *)
  lookahead := Lexer.consumeSym(lexer);
    
  (* expression *)
  IF matchSet(FIRST(Expression)) THEN
    lookahead := expression(expr1)
  ELSE (* resync *)
    lookahead := skipToMatchTokenOrToken(Token.RightBracket, Token.DotDot)
  END; (* IF *)
  
  (* ']' ( '.' targetDesignator | derefTargetTail )? | '..' expression? ']' *)
  CASE lookahead.token OF
  (* ']' *)
  | Token.RightBracket :
    (* ']' *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* '.' designator *)
    CASE lookahead.token OF
    (* '.' *)
    | Token.Period :
      lookahead := Lexer.consumeSym(lexer);
      
      (* targetDesignator *)
      IF matchSet(FIRST(Designator)) THEN
        lookahead := targetDesignator(variant, tail)
      ELSE (* resync *)
        lookahead := skipToMatchSet(FOLLOW(SubscriptTail)
      END (* IF *)
    
    (* derefTargetTail *)
    | Token.Deref :
      lookahead := derefTargetTail(variant, tail);
      
    ELSE (* bypass *)
      (* input also matches subscriptTail *)
      variant := Common;
    END; (* CASE *)
    
    (* build AST node and pass it back in astNode *)
    astNode := AST.NewNode(AstNodeType.SubTail, expr1, tail)
    
  (* '..' expression? ']' *)
  | Token.DotDot :
    (* '..' *)
    lookahead := Lexer.consumeSym(lexer);
    
    (* expression? *)
    IF inFIRST(Expression, lookahead.Token) THEN
      lookahead := expression(expr2);
    ELSE
      expr2 := NIL
    END; (* IF *)
    
    (* ']' *)
    IF matchToken(Token.RightBracket) THEN
      lookahead := Lexer.consumeSym(lexer)
    ELSE (* resync *)
      lookahead := skipToMatchSet(FOLLOW(BracketTargetTail))
    END (* IF *)
    
    (* input only matches bracketTargetTail *)
    variant := Target;
      
    (* build AST node and pass it back in astNode *)
    IF expr2 = NIL THEN (* create insertion node *)
      astNode := AST.NewNode(AstNodeType.Ins, expr1)
    ELSE (* create slice node *)
      astNode := AST.NewNode(AstNodeType.Slice, expr1, expr2)
    END (* IF *)
  END; (* CASE *)
    
  RETURN lookahead
END bracketTargetTail;


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
  lookahead : SymbolT;
  
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
 *       IF matchSet(FIRST(Term)) THEN
 *         lookahead := term(rightNode);
 *         leftNode := AST.NewBinaryNode(nodeType, leftNode, rightNode)
 *       END
 *     END
 *   END
 * ------------------------------------------------------------------------ *)

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
  lookahead : SymbolT;
  
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
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("simpleExpression");
    
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
      IF matchSet(FIRST(Term)) THEN
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
  lookahead : SymbolT;
  
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
  lookahead : SymbolT;
  
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
    lookahead := skipToMatchSet(FOLLOW(SimpleTerm))
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
  lookahead : SymbolT;
  
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
  lookahead : SymbolT;
  
BEGIN
  PARSER_DEBUG_INFO("simpleFactor");
  
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
          skipToMatchTokenOrSet(Token.RightParen, FOLLOW(SimpleFactor))
      END; (* IF *)
      
      (* ')' *)
      IF matchToken(Token.RightParen) THEN
        lookahead := Lexer.consumeSym(lexer)
      ELSE
        lookahead := skipToMatchSet(FOLLOW(SimpleFactor))
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
  lookahead : SymbolT;

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
  lookahead : SymbolT;

BEGIN
  PARSER_DEBUG_INFO("structuredValue");
  
  AstQueue.New(tmplist);
  
  (* '{' *)
  lookahead := Lexer.consumeSym(lexer);
    
  (* valueComponent *)
  lookahead := valueComponent(value);
  AstQueue.Enqueue(tmplist, value);
  
  (* ( ',' valueComponent )* *)
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
  lookahead : SymbolT;
  
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
  lookahead : SymbolT;
  
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
END parseListWTerminator;


(* ************************************************************************ *
 * Token Matching and Resynchronisation                                     *
 * ************************************************************************ *)

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
    statistics.syntaxErrors := statistics.syntaxErrors + 1;
    
    RETURN FALSE
  END (* IF *)
END matchToken;


(* --------------------------------------------------------------------------
 * private function matchSet(expectedSet)
 * --------------------------------------------------------------------------
 * Matches the lookahead symbol to set expectedSet and returns TRUE if it
 * matches any token in the set.  If there is no match, a syntax error is
 * reported, the error count is incremented and FALSE is returned.
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
    statistics.syntaxErrors := statistics.syntaxErrors + 1;
    
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
  
  (* check if lookahead matches token or any token in expectedSet *)
  IF expectedToken = lookahead.token OR 
    TokenSet.isElement(expectedSet, lookahead.token) THEN
    RETURN TRUE
    
  ELSE (* no match *)
    (* report error *)
    EmitSyntaxErrorWTokenAndSet(expectedToken, expectedSet, lookahead);
    
    (* print source line *)
    Source.PrintLineAndMarkColumn(source, lookahead.line, lookahead.col);
        
    (* update error count *)
    statistics.syntaxErrors := statistics.syntaxErrors + 1;
    
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
  
  (* check if lookahead matches any token in expectedSet1 or expectedSet2 *)
  IF TokenSet.isElement(expectedSet1, lookahead.token) OR 
    TokenSet.isElement(expectedSet2, lookahead.token) THEN
    RETURN TRUE
    
  ELSE (* no match *)
    (* report error *)
    EmitSyntaxErrorWSetAndSet(expectedSet1, expectedSet2, lookahead);
    
    (* print source line *)
    Source.PrintLineAndMarkColumn(source, lookahead.line, lookahead.col);
        
    (* update error count *)
    statistics.syntaxErrors := statistics.syntaxErrors + 1;
    
    RETURN FALSE
  END (* IF *)
END matchSetOrSet;


(* --------------------------------------------------------------------------
 * private function skipToMatchTokenOrToken(token1, token2)
 * --------------------------------------------------------------------------
 * Consumes symbols until the lookahead symbol's token matches token1 or
 * token2.  Returns the new lookahead symbol.
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
 * private function skipToMatchSet(set)
 * --------------------------------------------------------------------------
 * Consumes symbols until the lookahead symbol's token matches any token in
 * set.  Returns the new lookahead symbol.
 * --------------------------------------------------------------------------
 *)
PROCEDURE skipToMatchSet ( set : TokenSetT ) : SymbolT;

VAR
  lookahead : SymbolT;
  
BEGIN
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* skip symbols until lookahead matches any token in set *)
  WHILE NOT TokenSet.isElement(set, lookahead.token) DO
    lookahead = Lexer.consumeSym(lexer)
  END; (* WHILE *)
  
  RETURN lookahead
END skipToMatchSet;


(* --------------------------------------------------------------------------
 * private function skipToMatchSetOrSet(set1, set2)
 * --------------------------------------------------------------------------
 * Consumes symbols until the lookahead symbol's token matches any token in
 * set1 or set2.  Returns the new lookahead symbol.
 * --------------------------------------------------------------------------
 *)
PROCEDURE skipToMatchSetOrSet ( set1, set2 : TokenSetT ) : SymbolT;

VAR
  lookahead : SymbolT;
  
BEGIN
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* skip symbols until lookahead matches any token in set1 or set2 *)
  WHILE NOT TokenSet.isElement(set1, lookahead.token) AND
    NOT TokenSet.isElement(set2, lookahead.token) DO
    lookahead = Lexer.consumeSym(lexer)
  END; (* WHILE *)
  
  RETURN lookahead
END skipToMatchSetOrSet;


(* --------------------------------------------------------------------------
 * private function skipToMatchTokenOrSet(token, set)
 * --------------------------------------------------------------------------
 * Consumes symbols until the lookahead symbol's token matches token or any
 * token in set.  Returns the new lookahead symbol.
 * --------------------------------------------------------------------------
 *)
PROCEDURE skipToMatchTokenOrSet ( token : TokenT; set : TokenSetT ) : SymbolT;

VAR
  lookahead : SymbolT;
  
BEGIN
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* skip symbols until lookahead matches token or any token in set *)
  WHILE (lookahead.token # token) AND
    NOT TokenSet.isElement(set, lookahead.token) DO
    lookahead = Lexer.consumeSym(lexer)
  END; (* WHILE *)
  
  RETURN lookahead
END skipToMatchTokenOrSet;


(* --------------------------------------------------------------------------
 * private function skipToMatchTokenOrTokenOrSet(token1, token2, set)
 * --------------------------------------------------------------------------
 * Consumes symbols until the lookahead symbol's token matches token1, token2
 * or any token in set.  Returns the new lookahead symbol.
 * --------------------------------------------------------------------------
 *)
PROCEDURE skipToMatchTokenOrTokenOrSet
  ( token1, token2 : TokenT; set : TokenSetT ) : SymbolT;

VAR
  lookahead : SymbolT;
  
BEGIN
  lookahead := Lexer.lookaheadSym(lexer);
  
  (* skip symbols until lookahead token matches token1, token2 or set *)
  WHILE (lookahead.token # token1) AND (lookahead.token # token2) AND
    NOT TokenSet.isElement(set, lookahead.token) DO
    lookahead = Lexer.consumeSym(lexer)
  END; (* WHILE *)
  
  RETURN lookahead
END skipToMatchTokenOrTokenOrSet;

(* TO DO: ParserDebugInfo() *)

END Parser.