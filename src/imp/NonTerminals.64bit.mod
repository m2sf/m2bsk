(*!m2pim*) (* Copyright (c) 2015 B.Kowarsch. All rights reserved. *)

DEFINITION MODULE NonTerminals; (* 64-bit version *)

(* FIRST/FOLLOW set database for Modula-2 R10 Bootstrap Kernel *)

IMPORT TokenSet;
FROM Token IMPORT TokenT;
FROM TokenSet IMPORT TokenSetT, NewFromRawData;


TYPE TokenSetDB = ARRAY [MIN(Production)..MAX(Production)] OF TokenSetT;

VAR first, follow : TokenSetDB;


(* Operations *)

PROCEDURE FIRST ( p : Production ) : TokenSetT;
(* Returns a reference to the FIRST set of production p. *)
BEGIN
  RETURN first[p]
END FIRST;


PROCEDURE inFIRST ( p : Production; token : TokenT ) : BOOLEAN;
(* Returns TRUE if token is an element of FIRST(p), otherwise FALSE. *)
BEGIN
  RETURN TokenSet.isElem(first[p], token)
END inFIRST;


PROCEDURE FOLLOW ( p : Production ) : TokenSetT;
(* Returns a reference to the FOLLOW set of production p. *)
BEGIN
  RETURN follow[p]
END FOLLOW;


PROCEDURE inFOLLOW ( p : Production; token : TokenT ) : BOOLEAN;
(* Returns TRUE if token is an element of FOLLOW(p), otherwise FALSE. *)
BEGIN
  RETURN TokenSet.isElem(follow[p], token)
END inFOLLOW;


BEGIN (* init DB *)

  (* TO DO : insert initialisation values *)

  (* FIRST sets *)
  NewFromRawData(first[CompilationUnit],
    0, 0 );
  NewFromRawData(first[DefintionModule],
    0, 0 );
  NewFromRawData(first[Import],
    0, 0 );
  NewFromRawData(first[Definition],
    0, 0 );
  NewFromRawData(first[ConstDefinition],
    0, 0 );
  NewFromRawData(first[SimpleConstDefinition],
    0, 0 );
  NewFromRawData(first[TypeDefinition],
    0, 0 );
  NewFromRawData(first[AliasType],
    0, 0 );
  NewFromRawData(first[Qualident],
    0, 0 );
  NewFromRawData(first[SubrangeType],
    0, 0 );
  NewFromRawData(first[Range],
    0, 0 );
  NewFromRawData(first[EnumType],
    0, 0 );
  NewFromRawData(first[IdentList],
    0, 0 );
  NewFromRawData(first[SetType],
    0, 0 );
  NewFromRawData(first[ArrayType],
    0, 0 );
  NewFromRawData(first[RecordType],
    0, 0 );
  NewFromRawData(first[FieldList],
    0, 0 );
  NewFromRawData(first[RecTypeToExtend],
    0, 0 );
  NewFromRawData(first[PointerType],
    0, 0 );
  NewFromRawData(first[ProcedureType],
    0, 0 );
  NewFromRawData(first[FormalType],
    0, 0 );
  NewFromRawData(first[NonAttrFormalType],
    0, 0 );
  NewFromRawData(first[SimpleFormalType],
    0, 0 );
  NewFromRawData(first[CastingFormalType],
    0, 0 );
  NewFromRawData(first[VariadicFormalType],
    0, 0 );
  NewFromRawData(first[ProcedureHeader],
    0, 0 );
  NewFromRawData(first[BindingSpecifier],
    0, 0 );
  NewFromRawData(first[BindableIdent],
    0, 0 );
  NewFromRawData(first[ProcedureSignature],
    0, 0 );
  NewFromRawData(first[FormalParams],
    0, 0 );
  NewFromRawData(first[ProgramModule],
    0, 0 );
  NewFromRawData(first[PrivateImport],
    0, 0 );
  NewFromRawData(first[Block],
    0, 0 );
  NewFromRawData(first[ImplementationModule],
    0, 0 );
  NewFromRawData(first[PossiblyEmptyBlock],
    0, 0 );
  NewFromRawData(first[Declaration],
    0, 0 );
  NewFromRawData(first[TypeDeclaration],
    0, 0 );
  NewFromRawData(first[PointerOrIndeterminateType],
    0, 0 );
  NewFromRawData(first[IndeterminateTarget],
    0, 0 );
  NewFromRawData(first[IndeterminateField],
    0, 0 );
  NewFromRawData(first[VarDeclaration],
    0, 0 );
  NewFromRawData(first[AliasDeclaration],
    0, 0 );
  NewFromRawData(first[NameSelector],
    0, 0 );
  NewFromRawData(first[StatementSequence],
    0, 0 );
  NewFromRawData(first[Statement],
    0, 0 );
  NewFromRawData(first[MemMgtOperation],
    0, 0 );
  NewFromRawData(first[NewStatement],
    0, 0 );
  NewFromRawData(first[RetainStatement],
    0, 0 );
  NewFromRawData(first[ReleaseStatement],
    0, 0 );
  NewFromRawData(first[UpdateOrProcCall],
    0, 0 );
  NewFromRawData(first[ReturnStatement],
    0, 0 );
  NewFromRawData(first[CopyStatement],
    0, 0 );
  NewFromRawData(first[ReadStatement],
    0, 0 );
  NewFromRawData(first[WriteStatement],
    0, 0 );
  NewFromRawData(first[InputArg],
    0, 0 );
  NewFromRawData(first[OutputArgs],
    0, 0 );
  NewFromRawData(first[FormattedArgs],
    0, 0 );
  NewFromRawData(first[IfStatement],
    0, 0 );
  NewFromRawData(first[CaseStatement],
    0, 0 );
  NewFromRawData(first[Case],
    0, 0 );
  NewFromRawData(first[CaseLabels],
    0, 0 );
  NewFromRawData(first[LoopStatement],
    0, 0 );
  NewFromRawData(first[WhileStatement],
    0, 0 );
  NewFromRawData(first[RepeatStatement],
    0, 0 );
  NewFromRawData(first[ForStatement],
    0, 0 );
  NewFromRawData(first[ForLoopVariants],
    0, 0 );
  NewFromRawData(first[IterableExpr],
    0, 0 );
  NewFromRawData(first[OrdinalRange],
    0, 0 );
  NewFromRawData(first[Designator],
    0, 0 );
  NewFromRawData(first[DesignatorTail],
    0, 0 );
  NewFromRawData(first[SubscriptOrSlice],
    0, 0 );
  NewFromRawData(first[TargetDesignator],
    0, 0 );
  NewFromRawData(first[TargetDesignatorTail],
    0, 0 );
  NewFromRawData(first[SubscriptOrSliceOrInsert],
    0, 0 );
  NewFromRawData(first[FieldSelector],
    0, 0 );
  NewFromRawData(first[ExpressionList],
    0, 0 );
  NewFromRawData(first[Expression],
    0, 0 );
  NewFromRawData(first[SimpleExpression],
    0, 0 );
  NewFromRawData(first[Term],
    0, 0 );
  NewFromRawData(first[SimpleTerm],
    0, 0 );
  NewFromRawData(first[Factor],
    0, 0 );
  NewFromRawData(first[SimpleFactor],
    0, 0 );
  NewFromRawData(first[DesignatorOrFuncCall],
    0, 0 );
  NewFromRawData(first[StructuredValue],
    0, 0 );
  NewFromRawData(first[ValueComponent],
    0, 0 );
  NewFromRawData(first[ToDoList],
    0, 0 );
  NewFromRawData(first[TrackingRef],
    0, 0 );
  NewFromRawData(first[TaskToDo],
    0, 0 );
  
  (* FOLLOW sets *)
  NewFromRawData(follow[CompilationUnit],
    0, 0 );
  NewFromRawData(follow[DefintionModule],
    0, 0 );
  NewFromRawData(follow[Import],
    0, 0 );
  NewFromRawData(follow[Definition],
    0, 0 );
  NewFromRawData(follow[ConstDefinition],
    0, 0 );
  NewFromRawData(follow[SimpleConstDefinition],
    0, 0 );
  NewFromRawData(follow[TypeDefinition],
    0, 0 );
  NewFromRawData(follow[AliasType],
    0, 0 );
  NewFromRawData(follow[Qualident],
    0, 0 );
  NewFromRawData(follow[SubrangeType],
    0, 0 );
  NewFromRawData(follow[Range],
    0, 0 );
  NewFromRawData(follow[EnumType],
    0, 0 );
  NewFromRawData(follow[IdentList],
    0, 0 );
  NewFromRawData(follow[SetType],
    0, 0 );
  NewFromRawData(follow[ArrayType],
    0, 0 );
  NewFromRawData(follow[RecordType],
    0, 0 );
  NewFromRawData(follow[FieldList],
    0, 0 );
  NewFromRawData(follow[RecTypeToExtend],
    0, 0 );
  NewFromRawData(follow[PointerType],
    0, 0 );
  NewFromRawData(follow[ProcedureType],
    0, 0 );
  NewFromRawData(follow[FormalType],
    0, 0 );
  NewFromRawData(follow[NonAttrFormalType],
    0, 0 );
  NewFromRawData(follow[SimpleFormalType],
    0, 0 );
  NewFromRawData(follow[CastingFormalType],
    0, 0 );
  NewFromRawData(follow[VariadicFormalType],
    0, 0 );
  NewFromRawData(follow[ProcedureHeader],
    0, 0 );
  NewFromRawData(follow[BindingSpecifier],
    0, 0 );
  NewFromRawData(follow[BindableIdent],
    0, 0 );
  NewFromRawData(follow[ProcedureSignature],
    0, 0 );
  NewFromRawData(follow[FormalParams],
    0, 0 );
  NewFromRawData(follow[ProgramModule],
    0, 0 );
  NewFromRawData(follow[PrivateImport],
    0, 0 );
  NewFromRawData(follow[Block],
    0, 0 );
  NewFromRawData(follow[ImplementationModule],
    0, 0 );
  NewFromRawData(follow[PossiblyEmptyBlock],
    0, 0 );
  NewFromRawData(follow[Declaration],
    0, 0 );
  NewFromRawData(follow[TypeDeclaration],
    0, 0 );
  NewFromRawData(follow[PointerOrIndeterminateType],
    0, 0 );
  NewFromRawData(follow[IndeterminateTarget],
    0, 0 );
  NewFromRawData(follow[IndeterminateField],
    0, 0 );
  NewFromRawData(follow[VarDeclaration],
    0, 0 );
  NewFromRawData(follow[AliasDeclaration],
    0, 0 );
  NewFromRawData(follow[NameSelector],
    0, 0 );
  NewFromRawData(follow[StatementSequence],
    0, 0 );
  NewFromRawData(follow[Statement],
    0, 0 );
  NewFromRawData(follow[MemMgtOperation],
    0, 0 );
  NewFromRawData(follow[NewStatement],
    0, 0 );
  NewFromRawData(follow[RetainStatement],
    0, 0 );
  NewFromRawData(follow[ReleaseStatement],
    0, 0 );
  NewFromRawData(follow[UpdateOrProcCall],
    0, 0 );
  NewFromRawData(follow[ReturnStatement],
    0, 0 );
  NewFromRawData(follow[CopyStatement],
    0, 0 );
  NewFromRawData(follow[ReadStatement],
    0, 0 );
  NewFromRawData(follow[WriteStatement],
    0, 0 );
  NewFromRawData(follow[InputArg],
    0, 0 );
  NewFromRawData(follow[OutputArgs],
    0, 0 );
  NewFromRawData(follow[FormattedArgs],
    0, 0 );
  NewFromRawData(follow[IfStatement],
    0, 0 );
  NewFromRawData(follow[CaseStatement],
    0, 0 );
  NewFromRawData(follow[Case],
    0, 0 );
  NewFromRawData(follow[CaseLabels],
    0, 0 );
  NewFromRawData(follow[LoopStatement],
    0, 0 );
  NewFromRawData(follow[WhileStatement],
    0, 0 );
  NewFromRawData(follow[RepeatStatement],
    0, 0 );
  NewFromRawData(follow[ForStatement],
    0, 0 );
  NewFromRawData(follow[ForLoopVariants],
    0, 0 );
  NewFromRawData(follow[IterableExpr],
    0, 0 );
  NewFromRawData(follow[OrdinalRange],
    0, 0 );
  NewFromRawData(follow[Designator],
    0, 0 );
  NewFromRawData(follow[DesignatorTail],
    0, 0 );
  NewFromRawData(follow[SubscriptOrSlice],
    0, 0 );
  NewFromRawData(follow[TargetDesignator],
    0, 0 );
  NewFromRawData(follow[TargetDesignatorTail],
    0, 0 );
  NewFromRawData(follow[SubscriptOrSliceOrInsert],
    0, 0 );
  NewFromRawData(follow[FieldSelector],
    0, 0 );
  NewFromRawData(follow[ExpressionList],
    0, 0 );
  NewFromRawData(follow[Expression],
    0, 0 );
  NewFromRawData(follow[SimpleExpression],
    0, 0 );
  NewFromRawData(follow[Term],
    0, 0 );
  NewFromRawData(follow[SimpleTerm],
    0, 0 );
  NewFromRawData(follow[Factor],
    0, 0 );
  NewFromRawData(follow[SimpleFactor],
    0, 0 );
  NewFromRawData(follow[DesignatorOrFuncCall],
    0, 0 );
  NewFromRawData(follow[StructuredValue],
    0, 0 );
  NewFromRawData(follow[ValueComponent],
    0, 0 );
  NewFromRawData(follow[ToDoList],
    0, 0 );
  NewFromRawData(follow[TrackingRef],
    0, 0 );
  NewFromRawData(follow[TaskToDo],
    0, 0 )
END NonTerminals.
