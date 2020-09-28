(*!m2pim*) (* Copyright (c) 2015, 2020 Modula-2 Software Foundation *)

IMPLEMENTATION MODULE AstNodeType;

(* AST Node Type Implementation for Modula-2 R10 Bootstrap Kernel *)


FROM String IMPORT StringT;


(* Table for human readable names of node types *)

VAR typeName : ARRAY [Invalid .. Qualident] OF StringT;


(* Category Tests *)

PROCEDURE isNonTerminal ( t : AstNodeType ) : BOOLEAN;
(* Returns TRUE if t is a non-terminal node type, otherwise FALSE. *)

BEGIN
  RETURN (t >= MIN(Arity0)) AND (t < MIN(Terminal1))
    OR (t >= MIN(Arity2)) AND (t < MIN(TerminalN))
END isNonTerminal;


PROCEDURE isTerminal ( t : AstNodeType ) : BOOLEAN;
(* Returns TRUE if t is a terminal node type, otherwise FALSE. *)

BEGIN
  RETURN (t >= MIN(Terminal1)) AND (t <= MAX(Terminal1))
    OR (t >= MIN(TerminalN) AND (t <= MAX(TerminalN))
END isTerminal;


(* Presentation *)

PROCEDURE name ( t : AstNodeType ) : StringT;
(* Returns a human readable name for node type t. *)

BEGIN
  RETURN typeName[t]
END name;


BEGIN (* initialise name table *)
  typeName[Invalid] := String.forArray("???");
  typeName[Empty] := String.forArray("");
  typeName[Exit] := String.forArray("EXIT");
  typeName[Nop] := String.forArray("NOP");
  typeName[Alias] := String.forArray("ALIAS");
  typeName[Set] := String.forArray("SET");
  typeName[Pointer] := String.forArray("POINTER");
  typeName[Unq] := String.forArray("UNQ");
  typeName[Loop] := String.forArray("LOOP");
  typeName[Neg] := String.forArray("NEG");
  typeName[Not] := String.forArray("NOT");
  typeName[Filename] := String.forArray("FILENAME");
  typeName[Compiled] := String.forArray("COMPILED");
  typeName[Digest] := String.forArray("DIGEST");
  typeName[Ident] := String.forArray("ID");
  typeName[Qualident] := String.forArray("QID");
  typeName[IntVal] := String.forArray("INT");
  typeName[RealVal] := String.forArray("REAL");
  typeName[ChrVal] := String.forArray("CHR");
  typeName[QuotedVal] := String.forArray("STR");
  typeName[VarDecl] := String.forArray("VAR");
  typeName[TypeDecl] := String.forArray("TYPE");
  typeName[Array] := String.forArray("ARRAY");
  typeName[Enum] := String.forArray("ENUM");
  typeName[ProcType] := String.forArray("PROCTYPE");
  typeName[Field] := String.forArray("FIELD");
  typeName[Proc] := String.forArray("PROC");
  typeName[Bind] := String.forArray("BIND");
  typeName[PCall] := String.forArray("PCALL");
  typeName[While] := String.forArray("WHILE");
  typeName[Repeat] := String.forArray("REPEAT");
  typeName[Range] := String.forArray("RANGE");
  typeName[Eq] := String.forArray("EQ");
  typeName[Neq] := String.forArray("NEQ");
  typeName[Lt] := String.forArray("LT");
  typeName[LtEq] := String.forArray("LTEQ");
  typeName[Gt] := String.forArray("GT");
  typeName[GtEq] := String.forArray("GTEQ");
  typeName[In] := String.forArray("IN");
  typeName[Plus] := String.forArray("PLUS");
  typeName[Minus] := String.forArray("MINUS");
  typeName[Or] := String.forArray("OR");
  typeName[Star] := String.forArray("STAR");
  typeName[Slash] := String.forArray("SLASH");
  typeName[Div] := String.forArray("DIV");
  typeName[Mod] := String.forArray("MOD");
  typeName[And] := String.forArray("AND");
  typeName[FCall] := String.forArray("FCALL");
  typeName[Subr] := String.forArray("SUBR");
  typeName[CompUnit] := String.forArray("COMPUNIT");
  typeName[DefMod] := String.forArray("DEFMOD");
  typeName[ImpMod] := String.forArray("IMPMOD");
  typeName[Program] := String.forArray("PROG");
  typeName[Import] := String.forArray("IMPORT");
  typeName[Reexport] := String.forArray("REEXPORT");
  typeName[StmtSeq] := String.forArray("STMT*");
  typeName[ExprList] := String.forArray("EXPR*");
  typeName[IdentList] := String.forArray("ID*");
  typeName[QualidentList] := String.forArray("ID*")
END AstNodeType.