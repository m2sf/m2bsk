(*!m2pim*) (* Copyright (c) 2015 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE Parser;

(* Parser for Modula-2 R10 Bootstrap Kernel *)

IMPORT
  AST, AstNodeType, AstQueue,
  Lexer, LexQueue, Symbol, Token, TokenSet, String;

FROM AST IMPORT AstT;
FROM AstNodeType IMPORT AstNodeTypeT;
FROM AstQueue IMPORT AstQueueT;
FROM Lexer IMPORT LexerT;
FROM LexQueue IMPORT LexQueueT;
FROM Symbol IMPORT SymbolT;
FROM Token IMPORT TokenT;
FROM TokenSet IMPORT TokenSetT;
FROM String IMPORT StringT;


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
 * private function matchToken(p, expected_Token, resyncSet)
 * --------------------------------------------------------------------------
 * Matches the lookahead symbol to expectedToken and returns TRUE if they
 * match.  If they don't match, a syntax error is reported, the error count
 * is incremented, symbols are consumed until the lookahead symbol matches
 * one of the symbols in resyncSet and FALSE is returned.
 * --------------------------------------------------------------------------
 *)
PROCEDURE matchToken
  ( expectedToken : TokenT; resyncSet : TokenSetT ) : BOOLEAN;

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
    
    (* skip symbols until lookahead matches resyncSet *)
    WHILE NOT TokenSet.isElement(resyncSet, lookahead.token) DO
      lookahead = Lexer.consumeSym(lexer);
    END; (* WHILE *)
    
    RETURN FALSE
  END (* IF *)

END matchToken;


(* --------------------------------------------------------------------------
 * private function matchSet(p, expectedSet, resyncSet)
 * --------------------------------------------------------------------------
 * Matches the lookahead symbol to set expectedSet and returns TRUE if it
 * matches any of the tokens in the set.  If there is no match, a syntax
 * error is reported, the error count is incremented, symbols are consumed
 * until the lookahead symbol matches one of the symbols in resyncSet and
 * FALSE is returned.
 * --------------------------------------------------------------------------
 *)
PROCEDURE matchSet ( expectedSet, resyncSet : TokenSetT ) : BOOLEAN;

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
    
    (* skip symbols until lookahead matches resyncSet *)
    WHILE NOT TokenSet.isElement(resyncSet, lookahead.token) DO
      lookahead = Lexer.consumeSym(lexer);
    END; (* WHILE *)
    
    RETURN FALSE
  END (* IF *)
END matchSet;


(* ************************************************************************ *
 * Syntax Analysis                                                          *
 * ************************************************************************ *)

(* --------------------------------------------------------------------------
 * private function compilation_unit()
 * --------------------------------------------------------------------------
 * compilationUnit :=
 *   definitionModule | implOrPrgmModule
 *   ;
 *
 * astnode: defModuleNode | impModuleNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE compilationUnit : SymbolT;

VAR
  lookahead : SymbolT;

BEGIN
  
  PARSER_DEBUG_INFO("compilationUnit");
  
  lookahead := Lexer.lookaheadSym(lexer);
  
  CASE lookahead.token OF
    TokenT.Definition :
      lookahead := definitionModule()
      
  | TokenT.Implementation,
    TokenT.Module :
      lookahead := implOrPrgmModule()
  END; (* CASE *)
  
  RETURN lookahead
END compilationUnit;


(* --------------------------------------------------------------------------
 * private function definition_module()
 * --------------------------------------------------------------------------
 * definitionModule :=
 *   DEFINITION MODULE moduleIdent ';'
 *   import* definition* END moduleIdent '.'
 *   ;
 *
 * moduleIdent := Ident ;
 *
 * astnode: (DEFMOD identNode implist deflist)
 * --------------------------------------------------------------------------
 *)
PROCEDURE definitionModule : SymbolT;

VAR
  id, implist, deflist : AST;
  ident1, ident2 : String;
  tmplist : AstQueue;
  lookahead : Symbol;
  
BEGIN
  PARSER_DEBUG_INFO("definitionModule");
  
  (* DEFINITION *)
  lookahead := Lexer.consumeSym(lexer);
  
  (* MODULE *)
  IF matchToken
    (TokenT.Module, NonTerminals.Resync(ImportOrDefinitionOrEnd)) THEN
    lookahead := Lexer.consumeSym(lexer);
    
    (* moduleIdent *)
    IF matchToken
      (TokenT.Identifier, NonTerminals.Resync(ImportOrDefinitionOrEnd)) THEN
      ident1 = lookahead.lexeme;
      lookahead := Lexer.consumeSym(lexer);
      
      (* ';' *)
      IF matchToken
        (TokenT.Semicolon, NonTerminals.Resync(ImportOrDefinitionOrEnd)) THEN
        lookahead := Lexer.consumeSym(lexer)
      ELSE (* resync *)
        lookahead := Lexer.lookaheadSym(lexer)
      END (* IF *)
    ELSE (* resync *)
      lookahead := Lexer.lookaheadSym(lexer)
    END (* IF *)
  ELSE (* resync *)
    lookahead := Lexer.lookaheadSym(lexer);
  END; (* IF *)
  
  tmplist := AstQueue.New();

  (* import* *)
  WHILE lookahead.token = TokenT.Import DO
    lookahead := import();
    AstQueue.Enqueue(tmplist, ast)
  END (* WHILE *)
  
  implist := AST.NewListNode(AstNodeTypeT.ImpList, tmplist);
  tmplist:= AstQueue.ResetQueue(tmplist);
  
  (* definition* *)
  while lookahead.token = TokenT.Const OR
        lookahead.token = TokenT.Type OR
        lookahead.token = TokenT.Var OR
        lookahead.token = TokenT.Procedure DO
    lookahead := definition();
    AstQueue.Enqueue(tmplist, ast)
  END (* WHILE *)
  
  deflist := AST.NewListNode(AstNodeTypeT.DefList, tmplist);
  tmplist := AstQueue.ResetQueue(tmplist);
  
  (* END *)
  IF matchToken(TokenT.End, NonTerminals.FOLLOW(DefinitionModule)) THEN
    lookahead := Lexer.consumeSym(lexer);
    
    (* moduleIdent *)
    IF matchToken
       (TokenT.OtherIdent, NonTerminals.FOLLOW(DefinitionModule)) THEN
      lookahead := Lexer.ConsumeSym(lexer);
      ident2 := lookahead.lexeme;
    
      IF ident1 # ident2 THEN
        (* TO DO: report error -- module identifiers don't match *) 
      END; (* IF *)
    
      (* '.' *)
      IF matchToken(TokenT.Period, FOLLOW(DefinitionModule)) THEN
        lookahead := Lexer.consumeSym(lexer)
      END (* IF *)
    END (* IF *)
  END (* IF *)
  
  (* build AST node and pass it back in ast *)
  id := AST.NewTerminalNode(AstNodeTypeT.Ident, ident1);
  ast := AST.NewNode(AstNodeTypeT.DefMod, id, implist, deflist);
  
  RETURN lookahead
END definitionModule;


(* --------------------------------------------------------------------------
 * private function import()
 * --------------------------------------------------------------------------
 * import :=
 *   IMPORT libIdent ( ',' libIdent )* ';'
 *   ;
 *
 * alias libIdent := StdIdent ;
 *
 * astnode: (IMPORT implist)
 * --------------------------------------------------------------------------
 *)
PROCEDURE import : SymbolT;

BEGIN
  PARSER_DEBUG_INFO("import");

  (* IMPORT *)
  lookahead := Lexer.consumeSym(lexer);
  
  templist := LexQueue.New();
  
  (* libIdent *)
  IF matchToken
      (TokenT.Identifier, NonTerminals.Resync(ImportOrDefinitionOrEnd)) THEN
    lookahead := Lexer.consumeSym(lexer);
    id := AstNewTerminalNode(AstNodeTypeT.Ident, lookahead.lexeme);
    LexQueue.Enqueue(id, templist);
        
    (* ( ',' libIdent )* *)
    WHILE lookahead.token = TokenT.Comma DO
      lookahead := Lexer.consumeSym(lexer);
      
      (* libIdent *)
      IF matchToken
        (TokenT.Identifier, NonTerminals.Resync(ImportOrDefinitionOrEnd)) THEN
        lookahead := Lexer.ConsumeSym(lexer);
        id := AstNewTerminalNode(AstNodeTypeT.Ident, lookahead.lexeme);
        LexQueue.Enqueue(id, templist)
        
      ELSE (* resync *)
        lookahead := Lexer.lookaheadSym(lexer)
      END (* IF *)
    END (* WHILE *)
  ELSE
    (* resync *)
    lookahead := Lexer.lookaheadSym(lexer)
  END; (* IF *)
  
  (* build AST node and pass it back in ast *)
  ast := AST.NewListNode(AstNodeTypeT.Implist, templist);
  AstQueue.Release(templist);
  
  RETURN lookahead
END import;


(* --------------------------------------------------------------------------
 * private function ident()
 * --------------------------------------------------------------------------
 * import :=
 *   StdIdent | ForeignIdent
 *   ;
 *
 * astnode: (IDENT "lexeme")
 * --------------------------------------------------------------------------
 *)
PROCEDURE ident : SymbolT;

BEGIN

END ident;


(* --------------------------------------------------------------------------
 * private function qualident()
 * --------------------------------------------------------------------------
 * qualident :=
 *   ident ( '.' ident )*
 *   ;
 *
 * astnode: (QUALIDENT "lexeme1" "lexeme2" ... "lexemeN" )
 * --------------------------------------------------------------------------
 *)
PROCEDURE qualident : SymbolT;

BEGIN

END qualident;


(* --------------------------------------------------------------------------
 * private function identList()
 * --------------------------------------------------------------------------
 * identList :=
 *   ident ( ',' ident )*
 *   ;
 *
 * astnode: (IDENTLIST "lexeme1" "lexeme2" ... "lexemeN" )
 * --------------------------------------------------------------------------
 *)
PROCEDURE identList : SymbolT;

BEGIN

END identList;


(* --------------------------------------------------------------------------
 * private function definition()
 * --------------------------------------------------------------------------
 * definition :=
 *   CONST ( constDefinition ';' )+ |
 *   TYPE ( typeDefinition ';' )+ |
 *   VAR ( identList ':' typeIdent ';' )+ |
 *   procedureHeader ';' |
 *   toDoList ';'
 *   ;
 *
 * astnode: CONSTDEF or TYPEDEF or VARDECL or PROCDEF or TODO
 * --------------------------------------------------------------------------
 *)
PROCEDURE definition : SymbolT;

BEGIN

END definition;


(* --------------------------------------------------------------------------
 * private function constDefinition()
 * --------------------------------------------------------------------------
 * constDefinition :=
 *   ident '=' constExpression
 *   ;
 *
 * alias constExpression = expression ;
 *
 * astnode: (CONSTDEF identNode exprNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE constDefinition : SymbolT;

BEGIN

END constDefinition;


(* --------------------------------------------------------------------------
 * private function typeDefinition()
 * --------------------------------------------------------------------------
 * typeDefinition :=
 *   ident '=' ( OPAQUE | type )
 *   ;
 *
 * alias constExpression = expression ;
 *
 * astnode: (TYPEDEF identNode typeNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE typeDefinition : SymbolT;

BEGIN

END typeDefinition;


(* --------------------------------------------------------------------------
 * private function type()
 * --------------------------------------------------------------------------
 * type :=
 *   aliasType | derivedType | subrangeType | enumType | setType |
 *   arrayType | recordType | pointerType | procedureType
 *   ;
 *
 * alias derivedType = typeIdent ;
 *
 * alias typeIdent = qualident ;
 *
 * astnode: typeNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE type : SymbolT;

BEGIN

END type;


(* --------------------------------------------------------------------------
 * private function aliasType()
 * --------------------------------------------------------------------------
 * aliasType :=
 *   ALIAS OF typeIdent
 *   ;
 *
 * alias typeIdent = qualident ;
 *
 * astnode: (ALIAS qualidentNode )
 * --------------------------------------------------------------------------
 *)
PROCEDURE aliasType : SymbolT;

BEGIN

END aliasType;


(* --------------------------------------------------------------------------
 * private function subrangeType()
 * --------------------------------------------------------------------------
 * subrangeType :=
 *   range OF ordinalType
 *   ;
 *
 * alias ordinalType = typeIdent ;
 *
 * astnode: (SUBR rangeNode identNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE subrangeType : SymbolT;

BEGIN

END subrangeType;


(* --------------------------------------------------------------------------
 * private function range()
 * --------------------------------------------------------------------------
 * range :=
 *   '[' lowerBound '..' upperBound ']'
 *   ;
 *
 * alias lowerBound = constExpression ;
 *
 * alias upperBound = constExpression ;
 *
 * astnode: (RANGE exprNode exprNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE range : SymbolT;

BEGIN

END range;


(* --------------------------------------------------------------------------
 * private function enumType()
 * --------------------------------------------------------------------------
 * enumType :=
 *   '(' ( '+' enumTypeToExtend ',' )? identList ')'
 *   ;
 *
 * alias enumTypeToExtend = enumTypeIdent ;
 *
 * alias enumTypeIdent = typeIdent ;
 *
 * astnode: (ENUM identNode identListNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE enumType : SymbolT;

BEGIN

END enumType;


(* --------------------------------------------------------------------------
 * private function setType()
 * --------------------------------------------------------------------------
 * setType :=
 *   SET OF enumTypeIdent
 *   ;
 *
 * alias enumTypeIdent = typeIdent ;
 *
 * astnode: (SET identNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE setType : SymbolT;

BEGIN

END setType;


(* --------------------------------------------------------------------------
 * private function arrayType()
 * --------------------------------------------------------------------------
 * arrayType :=
 *   ARRAY valueCount OF typeIdent
 *   ;
 *
 * alias valueCount = constExpression ;
 *
 * astnode: (ARRAY exprNode qualidentNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE arrayType : SymbolT;

BEGIN

END arrayType;


(* --------------------------------------------------------------------------
 * private function recordType()
 * --------------------------------------------------------------------------
 * recordType :=
 *   RECORD ( '(' recTypeToExtend ')' )?
 *   fieldList ( ';' fieldList )* END
 *   ;
 *
 * recTypeToExtend = typeIdent | NIL ;
 *
 * alias fieldList = varOrFieldDeclaration ;
 *
 * astnode: (RECORD fieldListNode) | (EXTREC qualidentNode fieldListNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE recordType : SymbolT;

BEGIN

END recordType;


(* --------------------------------------------------------------------------
 * private function pointerType()
 * --------------------------------------------------------------------------
 * pointerType :=
 *   POINTER TO typeIdent
 *   ;
 *
 * astnode: (POINTER qualidentNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE pointerType : SymbolT;

BEGIN

END pointerType;


(* --------------------------------------------------------------------------
 * private function procedureType()
 * --------------------------------------------------------------------------
 * procedureType :=
 *   PROCEDURE
 *   ( '(' formalType ( ',' formalType )* ')' )? ( ':' returnedType )?
 *   ;
 *
 * alias returnedType = typeIdent ;
 *
 * astnode: (PROCTYPE formalTypeListNode qualidentNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE procedureType : SymbolT;

BEGIN

END procedureType;


(* --------------------------------------------------------------------------
 * private function formalType()
 * --------------------------------------------------------------------------
 * formalType :=
 *   nonAttrFormalType | attributedFormalType
 *   ;
 *
 * astnode: formalTypeNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE formalType : SymbolT;

BEGIN

END formalType;


(* --------------------------------------------------------------------------
 * private function nonAttrFormalType()
 * --------------------------------------------------------------------------
 * nonAttrFormalType :=
 *   ( ARRAY ident? OF )? typeIdent | castingFormalType
 *   ;
 *
 * astnode:
 *  (FTYPE qualidentNode) |
 *  (ARRAYP identNode qualidentNode) |
 *  (OPENARRAYP qualidentNode) |
 *  castingFormalTypeNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE nonAttrFormalType : SymbolT;

BEGIN

END nonAttrFormalType;


(* --------------------------------------------------------------------------
 * private function castingFormalType()
 * --------------------------------------------------------------------------
 * castingFormalType :=
 *   CAST ( BARE ARRAY OF OCTET | addressTypeIdent )
 *   ;
 *
 * astnode: (CASTP formalTypeNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE castingFormalType : SymbolT;

BEGIN

END castingFormalType;


(* --------------------------------------------------------------------------
 * private function addressTypeIdent()
 * --------------------------------------------------------------------------
 * addressTypeIdent :=
 *   ( UNSAFE '.' )? ADDRESS
 *   ;
 *
 * astnode: qualidentNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE addressTypeIdent : SymbolT;

BEGIN

END addressTypeIdent;


(* --------------------------------------------------------------------------
 * private function attributedFormalType()
 * --------------------------------------------------------------------------
 * attributedFormalType :=
 *   ( CONST | VAR ) ( nonAttrFormalType | simpleVariadicFormalType )
 *   ;
 *
 * astnode: formalTypeNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE attributedFormalType : SymbolT;

BEGIN

END attributedFormalType;


(* --------------------------------------------------------------------------
 * private function simpleVariadicFormalType()
 * --------------------------------------------------------------------------
 * simpleVariadicFormalType :=
 *   ARGLIST OF nonAttrFormalType
 *   ;
 *
 * astnode: (ARGLIST formalTypeNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE simpleVariadicFormalType : SymbolT;

BEGIN

END simpleVariadicFormalType;


(* --------------------------------------------------------------------------
 * private function procedureHeader()
 * --------------------------------------------------------------------------
 * procedureHeader :=
 *   PROCEDURE procedureSignature
 *   ;
 *
 * astnode: (PROC identNode fparamsListNode qualidentNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE procedureHeader : SymbolT;

BEGIN

END procedureHeader;


(* --------------------------------------------------------------------------
 * private function procedureSignature()
 * --------------------------------------------------------------------------
 * procedureHeader :=
 *   PROCEDURE procedureSignature
 *   ;
 *
 * astnode: (PROC identNode fparamsListNode qualidentNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE procedureSignature : SymbolT;

BEGIN

END procedureSignature;


(* --------------------------------------------------------------------------
 * private function formalParams()
 * --------------------------------------------------------------------------
 * formalParams :=
 *   identList ':' ( nonAttrFormalType | simpleVariadicFormalType ) |
 *   attributedFormalParams
 *   ;
 *
 * astnode: (FPARAMS identListNode formalTypeNode) | attrFParamsNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE formalParams : SymbolT;

BEGIN

END formalParams;


(* --------------------------------------------------------------------------
 * private function attributedFormalParams()
 * --------------------------------------------------------------------------
 * attributedFormalParams :=
 *   ( CONST | VAR ) identList ':'
 *   ( nonAttrFormalType | simpleVariadicFormalType )
 *   ;
 *
 * astnode:
 *  (CONSTP identListNode formalTypeNode) |
 *  (VARP identListNode formalTypeNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE attributedFormalParams : SymbolT;

BEGIN

END attributedFormalParams;


(* --------------------------------------------------------------------------
 * private function implOrPrgmModule()
 * --------------------------------------------------------------------------
 * implOrPrgmModule :=
 *   IMPLEMENTATION MODULE moduleIdent ';'
 *   privateImport* block moduleIdent '.'
 *   ;
 *
 * alias privateImport = import ;
 *
 * astnode: (IMPMOD qualidentNode implistNode blockNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE implOrPrgmModule : SymbolT;

BEGIN

END implOrPrgmModule;


(* --------------------------------------------------------------------------
 * private function block()
 * --------------------------------------------------------------------------
 * block :=
 *   declaration*
 *   BEGIN statementSequence END
 *   ;
 *
 * astnode: (BLOCK declListNode stmtSeqNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE block : SymbolT;

BEGIN

END block;


(* --------------------------------------------------------------------------
 * private function declaration()
 * --------------------------------------------------------------------------
 * declaration :=
 *   ALIAS ( aliasDeclaration ';' )+ |
 *   CONST ( ident '=' constExpression ';' )+ |
 *   TYPE ( typeDeclaration ';' )+ |
 *   VAR ( varOrFieldDeclaration ';' )+ |
 *   procedureHeader ';' block ident ';' |
 *   toDoList ';'
 *   ;
 *
 * astnode: (DECLLIST declNode1 declNode2 ... declNodeN)
 * --------------------------------------------------------------------------
 *)
PROCEDURE declaration : SymbolT;

BEGIN

END declaration;


(* --------------------------------------------------------------------------
 * private function aliasDeclaration()
 * --------------------------------------------------------------------------
 * aliasDeclaration :=
 *   namedAliasDecl | wildcardAliasDecl
 *   ;
 *
 * astnode: aliasDeclNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE aliasDeclaration : SymbolT;

BEGIN

END aliasDeclaration;


(* --------------------------------------------------------------------------
 * private function namedAliasDecl()
 * --------------------------------------------------------------------------
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
 * astnode:
 *  (ALIASDECL identNode qualidentNode) |
 *  (ALIASDECLLIST aliasDeclNode1 aliasDeclNode2 ... aliasDeclNodeN)
 * --------------------------------------------------------------------------
 *)
PROCEDURE namedAliasDecl : SymbolT;

BEGIN

END namedAliasDecl;


(* --------------------------------------------------------------------------
 * private function wildcardAliasDecl()
 * --------------------------------------------------------------------------
 * wildcardAliasDecl :=
 *   '*' '=' qualifiedWildcard
 *   ;
 *
 * astnode: (ALIASDECL (IDENT "*") qualidentNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE wildcardAliasDecl : SymbolT;

BEGIN

END wildcardAliasDecl;


(* --------------------------------------------------------------------------
 * private function typeDeclaration()
 * --------------------------------------------------------------------------
 * typeDeclaration :=
 *   ident '=' ( indeterminateType |  type )
 *   ;
 *
 * astnode: (TYPEDECL identNode typeNode) | indeterminateTypeNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE typeDeclaration : SymbolT;

BEGIN

END typeDeclaration;


(* --------------------------------------------------------------------------
 * private function indeterminateType()
 * --------------------------------------------------------------------------
 * indeterminateType :=
 *   IN RECORD
 *     fieldDeclaration ( fieldDeclaration ';' ) indeterminateField END
 *   ;
 *
 * alias fieldDeclaration = varOrFieldDeclaration ;
 *
 * astnode: (INREC fieldListNode identNode identNode qualidentNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE indeterminateType : SymbolT;

BEGIN

END indeterminateType;


(* --------------------------------------------------------------------------
 * private function indeterminateField()
 * --------------------------------------------------------------------------
 * indeterminateField :=
 *   '+' ident ':' BARE ARRAY discriminantFieldIdent OF typeIdent
 *   ;
 *
 * alias discriminantFieldIdent = ident ;
 *
 * astnode: (TERMLIST identNode identNode qualidentNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE indeterminateField : SymbolT;

BEGIN

END indeterminateField;


(* --------------------------------------------------------------------------
 * private function varOrFieldDeclaration()
 * --------------------------------------------------------------------------
 * varOrFieldDeclaration :=
 *   identList ':' ( typeIdent | anonType )
 *   ;
 *
 * astnode: (VARDECL identListNode typeNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE varOrFieldDeclaration : SymbolT;

BEGIN

END varOrFieldDeclaration;


(* --------------------------------------------------------------------------
 * private function anonType()
 * --------------------------------------------------------------------------
 * anonType :=
 *   ARRAY valueCount OF typeIdent |
 *   subrangeType |
 *   procedureType
 *   ;
 *
 * astnode: arrayTypeNode | subrangeTypeNode | procedureTypeNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE anonType : SymbolT;

BEGIN

END anonType;


(* --------------------------------------------------------------------------
 * private function statementSequence()
 * --------------------------------------------------------------------------
 * statementSequence :=
 *   statement ( ';' statement )*
 *   ;
 *
 * astnode: (STMTSEQ stmtNode1 stmtNode2 ... stmtNodeN)
 * --------------------------------------------------------------------------
 *)
PROCEDURE statementSequence : SymbolT;

BEGIN

END statementSequence;


(* --------------------------------------------------------------------------
 * private function statement()
 * --------------------------------------------------------------------------
 * statement :=
 *   emptyStatement | memMgtOperation | updateOrProcCall | returnStatement |
 *   ifStatement | caseStatement | loopStatement | whileStatement |
 *   repeatStatement | forStatement | EXIT
 *   ;
 *
 * alias emptyStatement = toDoList ;
 *
 * astnode: statementNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE statement : SymbolT;

BEGIN

END statement;


(* --------------------------------------------------------------------------
 * private function toDoList()
 * --------------------------------------------------------------------------
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
 * astnode: (TODO intValNode exprNode quotedValNode exprNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE toDoList : SymbolT;

BEGIN

END toDoList;


(* --------------------------------------------------------------------------
 * private function memMgtOperation()
 * --------------------------------------------------------------------------
 * memMgtOperation :=
 *   NEW designator ( OF initSize )? |
 *   RELEASE designator
 *   ;
 *
 * alias initSize = expression ;
 *
 * astnode: (NEW desigNode exprNode) | (RELEASE desigNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE memMgtOperation : SymbolT;

BEGIN

END memMgtOperation;


(* --------------------------------------------------------------------------
 * private function updateOrProcCall()
 * --------------------------------------------------------------------------
 * updateOrProcCall :=
 *   designator
 *     ( IncOrDecSuffix | ':=' expression | '(' expressionList ')' )?
 *   ;
 *
 * IncOrDecSuffix := '++' | '--' ;
 *
 * astnode: (ASSIGN desigNode exprNode) | (PCALL exprListNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE updateOrProcCall : SymbolT;

BEGIN

END updateOrProcCall;


(* --------------------------------------------------------------------------
 * private function returnStatement()
 * --------------------------------------------------------------------------
 * returnStatement :=
 *   RETURN expression?
 *   ;
 *
 * astnode: (RETURN exprNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE returnStatement : SymbolT;

BEGIN

END returnStatement;


(* --------------------------------------------------------------------------
 * private function ifStatement()
 * --------------------------------------------------------------------------
 * ifStatement :=
 *   IF boolExpression THEN statementSequence
 *   ( ELSIF boolExpression THEN statementSequence )*
 *   ( ELSE statementSequence )?
 *   END
 *   ;
 *
 * alias boolExpression = expression ;
 *
 * astnode: (IF exprNode stmtSeqNode elsifSeqNode stmtSeqNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE ifStatement : SymbolT;

BEGIN

END ifStatement;


(* --------------------------------------------------------------------------
 * private function caseStatement()
 * --------------------------------------------------------------------------
 * caseStatement :=
 *   CASE expression OF ( '|' case )+ ( ELSE statementSequece )? END
 *   ;
 *
 * astnode: (SWITCH exprNode caseListNode stmtSeqNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE caseStatement : SymbolT;

BEGIN

END caseStatement;


(* --------------------------------------------------------------------------
 * private function case()
 * --------------------------------------------------------------------------
 * case :=
 *   caseLabels ( ',' caseLabels )* : StatementSequence
 *   ;
 *
 * astnode: (CASE caseLabelListNode stmtSeqNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE case : SymbolT;

BEGIN

END case;


(* --------------------------------------------------------------------------
 * private function caseLabels()
 * --------------------------------------------------------------------------
 * caseLabels :=
 *   constExpression ( .. constExpression )?
 *   ;
 *
 * astnode: (CLABELS exprNode exprNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE caseLabels : SymbolT;

BEGIN

END caseLabels;


(* --------------------------------------------------------------------------
 * private function loopStatement()
 * --------------------------------------------------------------------------
 * loopStatement :=
 *   LOOP statementSequence END
 *   ;
 *
 * astnode: (LOOP stmtSeqNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE loopStatement : SymbolT;

BEGIN

END loopStatement;


(* --------------------------------------------------------------------------
 * private function whileStatement()
 * --------------------------------------------------------------------------
 * whileStatement :=
 *   WHILE boolExpression DO statementSequence END
 *   ;
 *
 * astnode: (WHILE exprNode stmtSeqNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE whileStatement : SymbolT;

BEGIN

END whileStatement;


(* --------------------------------------------------------------------------
 * private function repeatStatement()
 * --------------------------------------------------------------------------
 * repeatStatement :=
 *   WHILE boolExpression DO statementSequence END
 *   ;
 *
 * astnode: (REPEAT stmtSeqNode exprNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE repeatStatement : SymbolT;

BEGIN

END repeatStatement;


(* --------------------------------------------------------------------------
 * private function forStatement()
 * --------------------------------------------------------------------------
 * forStatement :=
 *   FOR forLoopVariants IN iterableExpr DO statementSequence END
 *   ;
 *
 * astnode: (FOR loopVarNode iterExprNode stmtSeqNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE forStatement : SymbolT;

BEGIN

END forStatement;


(* --------------------------------------------------------------------------
 * private function forLoopVariants()
 * --------------------------------------------------------------------------
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
 * astnode: (FLV identNode ascOrDescNode identNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE forLoopVariants : SymbolT;

BEGIN

END forLoopVariants;


(* --------------------------------------------------------------------------
 * private function iterableExpr()
 * --------------------------------------------------------------------------
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
 * astnode: desigNode | rangeNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE iterableExpr : SymbolT;

BEGIN

END iterableExpr;


(* --------------------------------------------------------------------------
 * private function designator()
 * --------------------------------------------------------------------------
 * designator :=
 *   qualident designatorTail?
 *   ;
 *
 * designatorTail :=
 *   ( ( '[' exprOrSlice ']' | '^' ) ( '.' ident )* )+
 *   ;
 *
 *
 * astnode: (DESIG )
 * --------------------------------------------------------------------------
 *)
PROCEDURE designator : SymbolT;

BEGIN

END designator;


(* --------------------------------------------------------------------------
 * private function designatorTail()
 * --------------------------------------------------------------------------
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
 * astnode: (DESIG )
 * --------------------------------------------------------------------------
 *)
PROCEDURE designatorTail : SymbolT;

BEGIN

END designatorTail;


(* --------------------------------------------------------------------------
 * private function expressionList()
 * --------------------------------------------------------------------------
 * expressionList :=
 *   expression ( ',' expression )*
 *   ;
 *
 * astnode: (EXPRLIST exprNode1 exprNode2 ... exprNodeN)
 * --------------------------------------------------------------------------
 *)
PROCEDURE expressionList : SymbolT;

BEGIN

END expressionList;


(* --------------------------------------------------------------------------
 * private function expression()
 * --------------------------------------------------------------------------
 * expression :=
 *   simpleExpression ( OperL1 simpleExpression )?
 *   ;
 *
 * OperL1 :=
 *   '=' | '#' | '<' | '<=' | '>' | '>=' | IN
 *   ;
 *
 * astnode: eqNode | neqNode | ltNode | ltEqNode | gtNode | gtEqNode | inNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE expression : SymbolT;

BEGIN

END expression;


(* --------------------------------------------------------------------------
 * private function simpleExpression()
 * --------------------------------------------------------------------------
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
 * astnode:
 *  plusNode | minusNode | orNode | concatNode | setDiffNode | negNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE simpleExpression : SymbolT;

BEGIN

END simpleExpression;


(* --------------------------------------------------------------------------
 * private function term()
 * --------------------------------------------------------------------------
 * term :=
 *   simpleTerm ( OperL3 simpleTerm )*
 *   ;
 *
 * OperL3 :=
 *   '*' | '/' | DIV | MOD | AND
 *   ;
 *
 * astnode: starNode | slashNode | divNode | modNode | andNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE term : SymbolT;

BEGIN

END term;


(* --------------------------------------------------------------------------
 * private function simpleTerm()
 * --------------------------------------------------------------------------
 * simpleTerm :=
 *   NOT? factor
 *   ;
 *
 * astnode: (NOT exprNode) | factorNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE simpleTerm : SymbolT;

BEGIN

END simpleTerm;


(* --------------------------------------------------------------------------
 * private function factor()
 * --------------------------------------------------------------------------
 * factor :=
 *   simpleFactor ( TypeConvOp typeIdent )?
 *   ;
 *
 * alias TypeConvOp = '::' ;
 *
 * astnode: (CONV exprNode qualidentNode) | simpleFactorNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE factor : SymbolT;

BEGIN

END factor;


(* --------------------------------------------------------------------------
 * private function simpleFactor()
 * --------------------------------------------------------------------------
 * simpleFactor :=
 *   NumberLiteral | StringLiteral |
 *   structuredValue | designatorOrFuncCall | '(' expression ')'
 *   ;
 *
 * astnode:
 *  intValNode | quotedValNode | structValNode |
 *  desigNode | fcallNode | exprNode
 * --------------------------------------------------------------------------
 *)
PROCEDURE simpleFactor : SymbolT;

BEGIN

END simpleFactor;


(* --------------------------------------------------------------------------
 * private function designatorOrFuncCall()
 * --------------------------------------------------------------------------
 * designatorOrFuncCall :=
 *   designator ( '(' expressionList? ')' )?
 *   ;
 *
 * astnode:
 *  desigNode | (FCALL desigNode exprListNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE designatorOrFuncCall : SymbolT;

BEGIN

END designatorOrFuncCall;


(* --------------------------------------------------------------------------
 * private function structuredValue()
 * --------------------------------------------------------------------------
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
 * astnode:
 *  (STRUCTVAL exprListNode)
 * --------------------------------------------------------------------------
 *)
PROCEDURE structuredValue : SymbolT;

BEGIN

END structuredValue;


END Parser.