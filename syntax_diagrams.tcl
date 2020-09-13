#!/usr/bin/wish
#
# Syntax diagram generator for Modula-2 BSK, status July 31, 2020
#
# This script is derived from the SQLite project's bubble-generator script.
# It is quite possibly the only such tool that can wrap-around diagrams so
# that they do not go off-page when inserting them into an ordinary A4  or
# US letter document. Thanks to the folks at the SQLite project for making
# their script available to the public.
#
# The present version of the script was cleaned up,  enhanced,  documented
# and modified by B.Kowarsch to become accessible to those unfamiliar with
# TCL/TK, and in particular to generate syntax diagrams for Modula-2 (R10).
#
# Ideally the design would have been changed such that the script can read
# grammars from a text file in EBNF notation.  Ideally,  this script would
# have been rewritten in a more readable language,  Modula-2 for instance.
# Due to time constraints,  these tasks had to be left for some other time
# or for somebody else to do.  In the meantime  the documentation embedded
# herein should suffice  even for those unfamiliar with TCL  to modify the
# script to generate diagrams for their own grammars.
#
# THIS SOFTWARE COMES WITHOUT ANY WARRANTY OF ANY KIND. USE IS STRICTLY AT
# THE RISK OF THE USER.  PERSONS WHO HAVE A  LEGAL RIGHT TO SUE AUTHORS OF
# NO-WARRANTY-FREE-OF-CHARGE OPEN SOURCE SOFTWARE  ARE  SPECIFICALLY *NOT*
# GIVEN ANY PERMISSION TO USE THIS SOFTWARE.  THE BOTTOM LINE IS : YOU MAY
# USE THIS SOFTWARE WITHOUT PERMISSION ANYWAY,  BUT YOU  *CANNOT*  SUE THE
# AUTHORS FOR DAMAGES  BECAUSE  IF YOUR GOVERNMENT  GRANTS YOU SUCH RIGHTS
# THEN YOU DID  *NOT*  HAVE PERMISSION TO USE THIS SOFTWARE TO BEGIN WITH.
# THUS, NO MATTER WHAT THE CIRCUMSTANCES,  THE RISK IS ALWAYS YOURS ALONE.


#
# Top-level displays
#
toplevel .bb
canvas .c -bg white
pack .c -side top -fill both -expand 1
wm withdraw .

#  
# ===========================================================================
# D O C U M E N T A T I O N
# ===========================================================================
#
# The grammar is encoded as a nested TCL list structure of the general form:
#
#   { production1 { ... } production2 { ... } ... }
#
# Production rules can be translated from (ANTLR) EBNF to TCL list items as
# follows:
#
# Simple term
#
#   production : term ;
#   => production { line term }
#
# Sequence and group
#
#   production : term1 term2 ;
#   => production { line term1 term2 }
#
# Alternative
#
#   production : term1 | term2 ;
#   => production { or term1 term2 }
#
# Optional term
#
#   production : term? ;
#   => production { opt term }
#   => production { optx term }
#
#  opt renders the bypass line in the main path
#  optx renders the term in the main path
#
# Terms that occur one or more times
#
#   production : term+ ;
#   => production { loop { line term } {} }
#
# Terms that occur zero or more times
#
#   production : term* ;
#   => production { loop {} { nil term } }
#
# Causing diagrams to wrap-around
#
#   production : term1 /* wrap here */ term2 /* wrap here */ term3 ;
#   => production { stack {line term1} {line term2} {line term3} }
#
# Rendering of terminals, non-terminals and tokens
#
#   Symbols are rendered according to their category:
#   (1) reserved words, names in all uppercase letters
#   (2) reserved identifiers, names in all uppercase letters preceded by /
#   (3) other terminals, mixed case names with a leading uppercase letter
#   (4) non-terminals, mixed case names with a leading lowercase letter
#   (5) single letter tokens, a single letter or a range eg. a..z / A..Z
#   (6) special symbol tokens, any other characters or character sequences
#
# Special names for tokens that TCL cannot handle verbatim
#
#   BACKSLASH is rendered as \
#   SINGLE_QUOTE is rendered as '
#   DOUBLE_QUOTE is rendered as "
#   DOUBLE_DQUOTE is rendered as ""
#   LEFT_BRACE is rendered as {
#   RIGHT_BRACE is rendered as }
#
# Rendering parameters
#
#   RES_WORD_FONT - font/size/style used to render reserved words
#   RES_IDENT_FONT - font/size/style used to render reserved identifiers
#   TERM_FONT - font/size/style used to render any other terminals
#   NON_TERM_FONT - font/size/style used to render non-terminals
#   TOKEN_FONT - font/size/style used to render tokens
#   RADIUS - turn radius for arcs
#   HSEP - horizontal separation
#   VSEP - vertical separation
#   LWIDTH -line width
#
# Pre-requisites
#
#   TCL and TK need to be installed
#
# Running the script
#
#   the most fool-proof method to run this script is to call the wish shell
#   with the name of the script as an argument:
#
#   $ wish modula2_syntax_diagrams.tcl
#
#   in the window that appears, click on the top button "Draw All Diagrams"
#
#   Diagrams will be written into postscript files in the working directory
#  
# ===========================================================================
#

# ===========================================================================
# Modula-2 grammar
# ===========================================================================
#  To reuse this diagram generator for other languages, replace the following
#  section with a definition of the grammar of the target language.
#
#  Do NOT add comments or blank lines within a production rule's definition!
#

# ---------------------------------------------------------------------------
# Non-Terminal Symbols
# ---------------------------------------------------------------------------
#
set non_terminals {}

# (1) Compilation Unit
lappend non_terminals compilationUnit {
  or
    definitionModule
    implementationModule
    programModule
}


# ------------------------
# Definition Module Syntax
# ------------------------

# (2) Definition Module
lappend non_terminals definitionModule {
  stack
    {line DEFINITION MODULE moduleIdent ;}
    {line {loop nil import} {loop nil definition} END moduleIdent .}
}

# (2.1) Module Identifier
lappend non_terminals moduleIdent {
  line StdIdent
}

# (3) Import
lappend non_terminals import {
  line IMPORT {loop {line libIdent {optx reExport}} ,} ;
}

# (3.1) Library Identifier
lappend non_terminals libIdent {
  line StdIdent
}

# (3.2) Re-Export Suffix
lappend non_terminals reExport {
  line +
}

# (4) omitted

# (5) Definition
lappend non_terminals definition {
  line {
    or
      {line CONST {loop {line constDefinition ;} nil} }
      {line TYPE {loop {line typeDefinition ;} nil} }
      {line VAR {loop {line identList : typeIdent ;} nil} }
      {line procedureHeader ;}
      {line toDoList ;}
  }
}

# (6) Constant Definition
lappend non_terminals constDefinition {
  line {optx [ {or /COLLATION /TLIMIT} ]} simpleConstDefinition
}

# (6.1) Simple Constant Definition
lappend non_terminals simpleConstDefinition {
  line ident {optx : typeIdent} = constExpression
}

# (6.2) Constant Expression
lappend non_terminals constExpression {
  line expression
}

# (7) Identifier
lappend non_terminals ident {
  line StdIdent
}

# (8) Type Definition
lappend non_terminals typeDefinition {
  line ident = {
    or
      aliasType
      derivedType
      subrangeType
      enumType
      setType
      arrayType
      recordType
      pointerType
      opaqueType
      procedureType
  }
}

# (9) Alias Type
lappend non_terminals aliasType {
  line ALIAS OF typeIdent
}

# (9.1) Type Identifier
lappend non_terminals typeIdent {
  line qualident
}

# (9.2) Qualified Identifier
lappend non_terminals qualident {
  or StdIdent Qualident
}

# (10) Derived Type
lappend non_terminals derivedType {
  line typeIdent
}

# (11) Subrange Type
lappend non_terminals subrangeType {
  line constRange OF countableType
}

# (11.1) Range Expression
lappend non_terminals constRange {
  line [ lowerBound .. upperBound ]
}

# (11.2) Lower Bound
lappend non_terminals lowerBound {
  line constExpression
}

# (11.3) Upper Bound
lappend non_terminals upperBound {
  line constExpression
}

# (11.4) Countable Type
lappend non_terminals countableType {
  line typeIdent
}

# (12) Enumeration Type
lappend non_terminals enumType {
  line ( {optx + enumTypeToExtend ,} identList )
}

# (12.1) Enumeration Type To Extend
lappend non_terminals enumTypeToExtend {
  line enumTypeIdent
}

# (12.2) Enumeration Type Identifier
lappend non_terminals enumTypeIdent {
  line typeIdent
}

# (12.3) Identifier List
lappend non_terminals identList {
  loop StdIdent ,
}

# (13) Set Type
lappend non_terminals setType {
  line SET OF enumTypeIdent
}

# (14) Array Type
lappend non_terminals arrayType {
  line ARRAY valueCount OF typeIdent
}

# (14.1) Value Count
lappend non_terminals valueCount {
  line constExpression
}

# (15) Record Type
lappend non_terminals recordType {
  line RECORD {optx ( recTypeToExtend )} {loop fieldList ;} END
}

# (15.1) Record Type To Extend
lappend non_terminals recTypeToExtend {
  or /NIL typeIdent
}

# (15.2) Field List
lappend non_terminals fieldList {
  line identList : {
    or
      typeIdent
      arrayType
      subrangeType
      procedureType
  }
}

# (16) Pointer Type
lappend non_terminals pointerType {
  line POINTER TO typeIdent
}

# (17) Opaque Type
lappend non_terminals opaqueType {
  line OPAQUE {or {line [ allocSize ]} POINTER}
}

# (17.1) Allocation Size
lappend non_terminals allocSize {
  line constExpression
}

# (18) Procedure Type
lappend non_terminals procedureType {
  line PROCEDURE {optx ( {loop formalType ;} )} {optx : returnedType}
}

# (18.1) Formal Type
lappend non_terminals formalType {
  line {or CONST VAR nil} nonAttrFormalType
}

# (18.2) Non-Attributed Formal Type
lappend non_terminals nonAttrFormalType {
  or simpleFormalType castingFormalType variadicFormalType
}

# (18.3) Returned Type
lappend non_terminals returnedType {
  line typeIdent
}

# (19) Simple Formal Type
lappend non_terminals simpleFormalType {
  line {optx ARRAY OF} typeIdent
}

# (20) Casting Formal Type
lappend non_terminals castingFormalType {
  line /CAST {or OCTETSEQ /ADDRESS}
}

# (21) Variadic Formal Type
lappend non_terminals variadicFormalType {
  line ARGLIST OF simpleFormalType
}

# (22) Procedure Header
lappend non_terminals procedureHeader {
  line PROCEDURE {optx [ bindingSpecifier ]} procedureSignature
}

# (22.1) Binding Specifier
lappend non_terminals bindingSpecifier {
  or
    {line NEW {or argListFlag capacityFlag nil}}
    RETAIN
    RELEASE
    {line READ {optx allocFlag}}
    {line WRITE {optx formatFlag}}
    {line bindableIdent}
}

# (22.2) Argument List Flag
lappend non_terminals argListFlag {
  line +
}

# (22.3) Capacity Flag
lappend non_terminals capacityFlag {
  line *
}

# (22.4) Allocation Flag
lappend non_terminals allocFlag {
  line *
}

# (22.5) Format Flag
lappend non_terminals formatFlag {
  line OCTOTHORPE
}

# (22.6) Bindable Ident
lappend non_terminals bindableIdent {
  or Pervasive Primitive
}

# (23) Procedure Signature
lappend non_terminals procedureSignature {
  line ident {optx ( {loop formalParams ;} )} {optx : returnedType}
}

# (24) Formal Parameters
lappend non_terminals formalParams {
  line {or CONST VAR nil} identList : nonAttrFormalType
}


# ----------------------------------------
# Implementation and Program Module Syntax
# ----------------------------------------

# (30) Program Module
lappend non_terminals programModule {
  stack
    {line MODULE moduleIdent ;}
    {line {loop nil privateImport} block moduleIdent .}
}

# (30.1) Private Import
lappend non_terminals privateImport {
  line IMPORT {loop libIdent ,} ;
}

# (31) Block
lappend non_terminals block {
  line {loop nil declaration}
  BEGIN statementSequence END
}

# (32) Implementation Module
lappend non_terminals implementationModule {
  stack
    {line IMPLEMENTATION MODULE moduleIdent ;}
    {line {loop nil privateImport} possiblyEmptyBlock moduleIdent .}
}

# (32.1) Possibly Empty Block
lappend non_terminals possiblyEmptyBlock {
  line {loop nil declaration} {optx BEGIN statementSequence} END
}

# (33) Declaration
lappend non_terminals declaration {
  line {
    or
      {line CONST {loop {line constDeclaration ;} nil}}
      {line TYPE {loop {line typeDeclaration ;} nil}}
      {line VAR {loop {line varDeclaration ;} nil}}
      {line procedureHeader ; block ident ;}
      {line aliasDeclaration ;}
      {line toDoList ;}
  }
}

# (33.1) Constant Declaration
lappend non_terminals constDeclaration {
  line simpleConstDefinition
}

# (34) Type Declaration
lappend non_terminals typeDeclaration {
  line ident = {
    or
      aliasType
      derivedType
      subrangeType
      enumType
      setType
      arrayType
      recordType
      octetSeqType
      privatePointerType
      procedureType
  }
}

# (34.1) Octet Sequence Type
lappend non_terminals octetSeqType {
  line OCTETSEQ [ valueCount ]
}

# (34.2) Private Pointer Type
lappend non_terminals privatePointerType {
  line POINTER TO {or determinateTarget indeterminateTarget}
}

# (34.3) Determinate Target
lappend non_terminals determinateTarget {
  line typeIdent
}

# (34.4) Indeterminate Target
lappend non_terminals indeterminateTarget {
  line RECORD {optx {loop {line fieldList ;}}} indeterminateField END
}

# (34.5) Indeterminate Field Declaration
lappend non_terminals indeterminateField {
  line + ident : ARRAY capacityField OF typeIdent
}

# (34.6) Capacity Field
lappend non_terminals capacityField {
  line ident
}

# (35) Variable Declaration
lappend non_terminals varDeclaration {
  line fieldList
}

# (36) Alias Declaration
lappend non_terminals aliasDeclaration {
  line UNQUALIFIED {loop nameSelector ,}
}

# (36.1) Name Selector
lappend non_terminals nameSelector {
  line qualident {optx wildcard}
}

# (36.2) Wildcard
lappend non_terminals wildcard {
  line .*
}

# (37) Statement Sequence
lappend non_terminals statementSequence {
  loop statement ;
}

# (38) Statement
lappend non_terminals statement {
  line {
    or
      memMgtOperation
      updateOrProcCall
      returnStatement
      copyStatement
      readStatement
      writeStatement
      ifStatement
      caseStatement
      loopStatement
      whileStatement
      repeatStatement
      forStatement
      toDoList
      EXIT
      NOP
  }
}

# (39) Memory Management Operation
lappend non_terminals memMgtOperation {
  or
    newStatement
    retainStatement
    releaseStatement
}

# (39.1) NEW Statement
lappend non_terminals newStatement {
  line NEW designator {
    or
      {line := expression}
      {line /CAPACITY expression}
      nil
  }
}

# (39.2) RETAIN Statement
lappend non_terminals retainStatement {
  line RETAIN designator
}

# (39.3) RELEASE Statement
lappend non_terminals releaseStatement {
  line RELEASE designator
}


# (40) Update Or Procedure Call
lappend non_terminals updateOrProcCall {
  or
    {line designator
      {or incOrDecSuffix {line ( expressionList )} nil}}
    {line targetDesignator := expression} 
}

# (40b) LL(1) Variant of Update Or Procedure Call
#   resulting illegal combinations not in compliance
#   with rule (40) must be rejected programmatically
#lappend non_terminals updateOrProcCall {
#  line targetDesignator {
#    or
#      {line incOrDecSuffix}
#      {line := expression}
#      {line ( expressionList )}
#      nil
#    }
#}

# (40.1) Increment Or Decrement Suffix
lappend non_terminals incOrDecSuffix {
  or
    {line ++}
    {line --}
}

# (41) RETURN Statement
lappend non_terminals returnStatement {
  line RETURN {optx expression}
}

# (42) COPY Statement
lappend non_terminals copyStatement {
  line COPY targetDesignator := expression
}

# (43) READ Statement
lappend non_terminals readStatement {
  line READ {optx @ chan :}
  {loop inputArg ,}
}

# (43.1) Channel
lappend non_terminals chan {
  line designator
}

# (43.2) Input Argument
lappend non_terminals inputArg {
  line {optx NEW} designator
}

# (44) WRITE Statement
lappend non_terminals writeStatement {
  line WRITE {optx @ chan :}
  {loop outputArgs ,}
}

# 44.1) Output Arguments
lappend non_terminals outputArgs {
  or formattedArgs unformattedArg
}

# (44.2) Formatted Arguments
lappend non_terminals formattedArgs {
  line OCTOTHORPE ( fmtStr , expressionList )
}

# (44.3) Unformatted Argument
lappend non_terminals unformattedArg {
  line expression
}

# (44.4) Format String
lappend non_terminals fmtStr {
  line expression
}

# (45) IF Statement
lappend non_terminals ifStatement {
  stack
    {line IF boolExpression THEN statementSequence}
    {optx {loop {line ELSIF boolExpression THEN statementSequence} nil}}
    {line {optx ELSE statementSequence} END}
}

# (45.1) Boolean Expression
lappend non_terminals boolExpression {
  line expression
}

# (46) CASE Statement
lappend non_terminals caseStatement {
  line CASE expression OF
    {loop {line | case} nil} {optx ELSE statementSequence} END
}

# (46) CASE Statement (wrapped)
lappend non_terminals caseStatement2 {
  stack
    {line CASE expression OF {loop {line | case} nil}}
    {line {optx ELSE statementSequence} END}
}

# (46.1) Case
lappend non_terminals case {
  line {loop caseLabels ,} : statementSequence
}

# (46.2) Case Labels
lappend non_terminals caseLabels {
  line constExpression {optx .. constExpression}
}

# (47) LOOP Statement
lappend non_terminals loopStatement {
  line LOOP statementSequence END
}

# (48) WHILE Statement
lappend non_terminals whileStatement {
  line WHILE boolExpression DO statementSequence END
}

# (48) WHILE Statement (wrapped)
lappend non_terminals whileStatement2 {
  stack
    {line WHILE boolExpression DO statementSequence}
    {line {optx ELSE statementSequence} END}
}

# (49) REPEAT Statement
lappend non_terminals repeatStatement {
  line REPEAT statementSequence UNTIL boolExpression
}

# (50) FOR Statement
lappend non_terminals forStatement {
  line FOR forLoopVariants IN iterableExpr DO statementSequence END
}

# (50) FOR Statement (wrapped)
lappend non_terminals forStatement2 {
  stack
    {line FOR forLoopVariants IN iterableExpr}
    {line DO statementSequence END}
}

# (50.1) FOR Loop Variants
lappend non_terminals forLoopVariants {
  line accessor {optx descender} {optx , value}
}

# (50.2) Accessor, Value
lappend non_terminals accessor {
  line StdIdent
}

# (50.3) Descender
lappend non_terminals descender {
  line --
}

# (50.4) Iterable Expression
lappend non_terminals iterableExpr {
  line collectionOrType {optx valueRange}
}

# (50.5) Collection Or Type
lappend non_terminals collectionOrType {
  line qualident
}

# (50.6) Value Range
lappend non_terminals valueRange {
  line [ firstValue .. lastValue ]
}

# (50.7) First Value
lappend non_terminals firstValue {
  line expression
}

# (50.8) Last Value
lappend non_terminals lastValue {
  line expression
}

# (51a) Designator
lappend non_terminals designator {
  line qualident {or derefTail subscriptTail nil}
}

# (51a.1) Dereferenced Designator Tail
lappend non_terminals derefTail {
  line deref {or {line . designator} subscriptTail nil}
}

# (51a.2) Subscripted Designator Tail
lappend non_terminals subscriptTail {
  line [ expression ] {or {line . designator} derefTail nil}
}

# (51b) Target Designator
lappend non_terminals targetDesignator {
  line qualident {or derefTargetTail bracketTargetTail nil}
}

# (51b.1) Dereferenced Target Designator Tail
lappend non_terminals derefTargetTail {
  line deref {or {line . targetDesignator} bracketTargetTail nil}
}

# (51b.2) Bracketed Target Designator Tail
lappend non_terminals bracketTargetTail {
  line [ expression {
    or 
      {line ] {or {line . targetDesignator} derefTargetTail nil}}
      {line .. {optx expression} ]}
  }
}

# (51.2) Deref
lappend non_terminals deref {
  loop ^ nil
}

# (52) Expression List
lappend non_terminals expressionList {
  loop expression ,
}

# (53) Expression
lappend non_terminals expression {
  line simpleExpression {optx operL1 simpleExpression}
}

# (53.1) Level-1 Operator
lappend non_terminals operL1 {
  or
    = # < <= > >= == IN
}

# (54) Simple Expression
lappend non_terminals simpleExpression {
  or
    {loop term operL2}
    {line - factor}
}

# (54.1) Level-2 Operator
lappend non_terminals operL2 {
  or + - OR concatOp setDiffOp
}

# (54.2) Concatenation Operator
lappend non_terminals concatOp {
  line &
}

# (54.3) Set Difference Operator
lappend non_terminals setDiffOp {
  line BACKSLASH
}

# (55) Term
lappend non_terminals term {
  loop simpleTerm operL3
}

# (55.1) Level-3 Operator
lappend non_terminals operL3 {
  or * / DIV MOD AND
}

# (56) Simple Term
lappend non_terminals simpleTerm {
  line {optx NOT} factor
}

# (57) Factor
lappend non_terminals factor {
  line simpleFactor {optx typeConvOp typeIdent}
}

# (57.1) Type Conversion Operator
lappend non_terminals typeConvOp {
  line ::
}

# (58) Simple Factor
lappend non_terminals simpleFactor {
  line {
    or
      NumberLiteral
      StringLiteral
      structuredValue
      sourceDesignator
      {line ( expression )}
    }
}

# (59) Source Designator
lappend non_terminals sourceDesignator {
  line qualident
    {or functionCallTail derefSourceTail bracketSourceTail nil}
}

# (59.1) Dereferenced Source Designator Tail
lappend non_terminals derefSourceTail {
  line deref
    {or {line . sourceDesignator} functionCallTail bracketSourceTail nil}
}

# (59.2) Bracketed Source Designator Tail 
lappend non_terminals bracketSourceTail {
  line [ expression {
    or 
      {line ] {or {line . sourceDesignator} functionCallTail derefSourceTail nil}}
      {line .. expression ]}
  }
}

# (59.3) Function Call Tail
lappend non_terminals functionCallTail {
  line ( {optx expressionList} )
}

# (60) Structured Value
lappend non_terminals structuredValue {
  line LBRACE {
    or
      {loop valueComponent ,}
      *
    } RBRACE
}

# (60.1) Value Component
lappend non_terminals valueComponent {
  or
    {line constExpression {optx .. constExpression}}
    {line runtimeExpression}
}

# (60.2) Runtime Expression
lappend non_terminals runtimeExpression {
  line expression
}


# (61) TO DO List
lappend non_terminals toDoList {
  line TO DO {
    or
      {line {optx trackingRef} {loop taskToDo ;} END}
      nil
    }
}

# (61.1) Tracking Reference
lappend non_terminals trackingRef {
  line ( issueId {optx , severity , description } )
}

# (61.2) Task To Do
lappend non_terminals taskToDo {
  line description {optx , estimatedTime timeUnit }
}

# (61.3) IssueId, Severity, Estimated Time
lappend non_terminals issueId {
  line wholeNumber
}

# (61.4) Time Unit
lappend non_terminals timeUnit {
  line StdIdent
}

# (61.5) Description
lappend non_terminals description {
  line StringLiteral
}

# (61.6) Whole Number
lappend non_terminals wholeNumber {
  line NumberLiteral 
}


# ---------------------------------------------------------------------------
# Terminal Symbols
# ---------------------------------------------------------------------------
#
set terminals {}


# -----------------------
# Identifiers
# -----------------------

# (1) Standard Identifier
lappend terminals StdIdent {
  line Letter {loop nil LetterOrDigit}
}

# (1.1) Letter Or Digit
lappend terminals LetterOrDigit {
  or Letter Digit
}

# (1.2) Qualified Identifier
lappend terminals Qualident {
  loop StdIdent .
}

# (1.3) Pervasive
lappend terminals Pervasive {
  line {loop {line A..Z} nil} {optx Digit} 
}

# (1.4) Primitive
lappend terminals Primitive {
  line _ {loop {line A..Z} nil} 
}


# ------------------
# Number Literals
# ------------------
lappend terminals NumberLiteral {
  or
    {line 0 {
      or
        RealNumberTail
        {line b Base2DigitSeq}
        {line x Base16DigitSeq}
        {line u Base16DigitSeq}
        nil
      }}
    {line 1..9 {optx DecimalNumberTail}}
}

# (2.1) Decimal Number Tail
lappend terminals DecimalNumberTail {
  or
   {line {optx SINGLE_QUOTE} DigitSeq {optx RealNumberTail}}
   RealNumberTail
}

# Digit Separator
# lappend terminals DigitSep {
#   line SINGLE_QUOTE
# }

# (2.2) Real Number Tail
lappend terminals RealNumberTail {
  line . DigitSeq {optx e {or + - nil} DigitSeq }
}

# (2.3) Digit Sequence
lappend terminals DigitSeq {
  loop DigitGroup SINGLE_QUOTE
}

# (2.3b) Digit Group
lappend terminals DigitGroup {
  loop Digit nil
}

# (2.4) Base-16 Digit Sequence
lappend terminals Base16DigitSeq {
  loop Base16DigitGroup SINGLE_QUOTE
}

# (2.4b) Base-16 Digit Group
lappend terminals Base16DigitGroup {
  loop Base16Digit nil
}

# (2.5) Base-2 Digit Sequence
lappend terminals Base2DigitSeq {
  loop Base2DigitGroup SINGLE_QUOTE
}

# (2.5b) Base-2 Digit Group
lappend terminals Base2DigitGroup {
  loop Base2Digit nil
}

# (2.6) Digit
lappend terminals Digit {
  or Base2Digit 2 3 4 5 6 7 8 9
}

# (2.7) Base-16 Digit
lappend terminals Base16Digit {
  or Digit A B C D E F
}

# (2.8) Base-2 Digit
lappend terminals Base2Digit {
  or 0 1
}

# ------------------
# (3) String Literal
# ------------------
lappend terminals StringLiteral {
  or SingleQuotedString DoubleQuotedString
}

# (3.1) Single Quoted String
lappend terminals SingleQuotedString {
  line SINGLE_QUOTE
    {optx {loop {or QuotableCharacter DOUBLE_QUOTE} nil}}
  SINGLE_QUOTE
}

# (3.2) Double Quoted String
lappend terminals DoubleQuotedString {
  line DOUBLE_QUOTE
    {optx {loop {or QuotableCharacter SINGLE_QUOTE} nil}}
  DOUBLE_QUOTE
}

# (3.3) Quotable Character
lappend terminals QuotableCharacter {
  or Digit Letter Space NonAlphaNumQuotable EscapedCharacter
}

# (3.4) Letter
lappend terminals Letter {
  or A..Z a..z 
}

# (3.5) Space
# CONST Space = CHR(32);

# (3.6a) Non-Alphanumeric Quotable Character
lappend terminals NonAlphaNumQuotable1 {
  or ! # $ % & ( ) * + ,
}

# (3.6b) Non-Alphanumeric Quotable Character
lappend terminals NonAlphaNumQuotable2 {
  or - . / : ; < = > ? @
}

# (3.6c) Non-Alphanumeric Quotable Character
lappend terminals NonAlphaNumQuotable3 {
  or [ ] ^ _ ` LBRACE | RBRACE ~
}

# (3.7) Escaped Character
lappend terminals EscapedCharacter {
  line BACKSLASH {or n t BACKSLASH}
}


# ---------------------------------------------------------------------------
# Ignore Symbols
# ---------------------------------------------------------------------------
#
set ignore_symbols {}

# (1) Whitespace
lappend ignore_symbols Whitespace {
  or Space ASCII_TAB
}

# (1.1) ASCII_TAB
# CONST ASCII_TAB = CHR(9);

# (2) Line Comment
lappend ignore_symbols LineComment {
  line ! {optx {loop CommentCharacter nil}} EndOfLine
}

# (3) Block Comment
lappend ignore_symbols BlockComment {
  line (* {optx {loop {or CommentCharacter BlockComment EndOfLine} nil}} *)
}

# (3.1) Comment Character
lappend ignore_symbols CommentCharacter {
  or Digit Letter Whitespace NonAlphaNumQuotable
  BACKSLASH SINGLE_QUOTE DOUBLE_QUOTE
}

# (5) End-Of-Line Marker
lappend ignore_symbols EndOfLine {
  or
    {line ASCII_LF}
    {line ASCII_CR {optx ASCII_LF}}
}

# (5.1) ASCII_LF
# CONST ASCII_LF = CHR(10);

# (5.2) ASCII_CR
# CONST ASCII_CR = CHR(13);

# (6) UTF8 BOM
# CONST UTF8_BOM = { 0uEF, 0uBB, 0uBF };


# ---------------------------------------------------------------------------
# Pragma Grammar
# ---------------------------------------------------------------------------
#
set pragmas {}

# (1) Pragma INLINE
lappend pragmas inlinePragma {
  line <* INLINE *>
}

# (2) Pragma DEPRECATED
lappend pragmas deprecatedPragma {
  line <* DEPRECATED *>
}

# (3) Pragma PRIVATETO
lappend pragmas privatetoPragma {
  line <* PRIVATETO = identList *>
}

# (4) Pragma FFI
lappend pragmas ffiPragma2 {
  line <* FFI = {or `C `CLR `JVM } *>
}

# (5) Pragma FFIDENT
lappend pragmas ffidentPragma {
  line <* FFIDENT = StringLiteral *>
}

# (6) Implementation Defined Pragma
lappend pragmas implDefPragma {
  stack
    {line <* {optx implPrefix .} pragmaSymbol}
    {line {optx = constExpr} | ctMsgMode *> }
}

# (6.1) Compile Time Message Mode
lappend pragmas ctMsgMode {
  or INFO WARN ERROR FATAL
}

# (6.2) Implementation Prefix
lappend pragmas implPrefix {
  line _IMPLEMENTATION_DEFINED_
}

# (6.3) Pragma Symbol
lappend pragmas pragmaSymbol {
  line _IMPLEMENTATION_DEFINED_
}


# ---------------------------------------------------------------------------
# Optional Language Extensions
# ---------------------------------------------------------------------------
#

set extensions {}

# (1) Foreign Identifier
lappend extensions ForeignIdent {
  or
    {line $ {loop LetterOrDigit nil} {loop nil ForeignIdentTail}}
    {line StdIdent {loop ForeignIdentTail nil}}
}

# (1.1) Foreign Identifier Tail
lappend extensions ForeignIdentTail {
  line {or $ _} {loop LetterOrDigit nil}
}

# (2) Disabled Code Section
lappend extensions DisabledCodeSection {
  line
    StartOfLine ?<
    {loop {or AnyPrintable Whitespace EndOfLine} nil}
    EndOfLine >?
}

# (2.1) Start Of Line
# not a symbol

# (2.2) Any Printable Character
lappend extensions AnyPrintable {
  line 0u20..0u7E
}


# ---------------------------------------------------------------------------
# Legend Diagrams
# ---------------------------------------------------------------------------
#
set legend {}

# EBNF -- Terminal #
lappend legend EBNF_terminal {
  line Terminal
}

# EBNF -- Non-Terminal #
lappend legend EBNF_non_terminal {
  line nonTerminal
}

# EBNF -- Literal #
lappend legend EBNF_literal_hash {
  line #
}

# EBNF -- Literal BAZ
lappend legend EBNF_literal_BAZ {
  line BAZ
}

# EBNF -- Sequence #
lappend legend EBNF_sequence {
  line bar baz
}

# EBNF -- Alternative #
lappend legend EBNF_alternative {
  or bar baz
}

# EBNF -- Grouping #
lappend legend EBNF_grouping {
  line bar {or baz bam}
}

# EBNF -- Option #
lappend legend EBNF_option {
  optx bar
}

# EBNF -- Kleene Star #
lappend legend EBNF_kleene_star {
  loop nil bar
}

# EBNF -- One Or More #
lappend legend EBNF_one_or_more {
  loop bar nil
}

# EBNF -- (bar baz)+ #
lappend legend EBNF_bar_baz_plus {
  loop {line bar baz} nil
}

# EBNF -- (bar baz) | (bar bam) #
lappend legend EBNF_bar_baz_or_bar_bam {
  line {or {line bar baz} {line bar bam}}
}

# EBNF -- bar (baz| bam) #
lappend legend EBNF_bar_or_baz_bam {
  line bar {or baz bam}
}

# EBNF -- bar (baz| bam) #
lappend legend EBNF_code_option {
  line {optx bar} baz
}

# EBNF -- bar (baz| bam) #
lappend legend EBNF_code_kleene_star {
  line {loop nil bar} baz
}

# EBNF -- bar (baz| bam) #
lappend legend EBNF_code_plus {
  line {loop bar nil} baz
}

# Legend -- Reserved Word
lappend legend legendReservedWord {
  line RESERVED
}

# Legend -- Terminal Symbol, Example 1
lappend legend legendTerminal1 {
  line #
}

# Legend -- Terminal Symbol, Example 2
lappend legend legendTerminal2 {
  line Terminal
}

# Legend -- Identifier
lappend legend legendIdentifier {
  line /IDENTIFIER
}

# Legend -- Non-Terminal Symbol
lappend legend legendNonTerminal {
  line nonTerminal
}

# end Modula-2 grammar


#  
# ===========================================================================
# C O D E   S T A R T S   H E R E
# ===========================================================================
#


# ---------------------------------------------------------------------------
# Draw the button box
# ---------------------------------------------------------------------------
#
set bn 0
set b .bb.b$bn
wm title .bb "Modula-2"

# Menu: All Diagrams
#
button $b -text "Draw All Diagrams" -command {draw_all_graphs}
pack $b -side top -fill x -expand 0 -pady 0

# Menu: Non-Terminals
#
incr bn
set b .bb.b$bn
button $b -text "Draw Non-Terminals" -command {draw_graphs $non_terminals}
pack $b -side top -fill x -expand 0 -pady 0

# Menu: Terminals
#
incr bn
set b .bb.b$bn
button $b -text "Draw Terminals" -command {draw_graphs $terminals}
pack $b -side top -fill x -expand 0 -pady 0

# Menu: Pragmas
#
incr bn
set b .bb.b$bn
button $b -text "Draw Pragmas" -command {draw_graphs $pragmas}
pack $b -side top -fill x -expand 0 -pady 0

# Menu: Ignore Symbols
#
incr bn
set b .bb.b$bn
button $b -text "Draw Ignore Symbols" -command {draw_graphs $ignore_symbols}
pack $b -side top -fill x -expand 0 -pady 0

# Menu: Extensions 
#
incr bn
set b .bb.b$bn
button $b -text "Draw Extensions" -command {draw_graphs $extensions}
pack $b -side top -fill x -expand 0 -pady 0


# Menu: Legend
#
incr bn
set b .bb.b$bn
button $b -text "Draw Legend" -command {draw_graphs $legend}
pack $b -side top -fill x -expand 0 -pady 0

# Menu: Quit
#
incr bn
set b .bb.b$bn
button $b -text "Quit" -command {exit}
pack $b -side top -fill x -expand 0 -pady {0 14}


# ---------------------------------------------------------------------------
# L a y o u t - P a r a m e t e r s
# ---------------------------------------------------------------------------
#
set RES_WORD_FONT {Helvetica 12 bold};           # reserved word font
set RES_IDENT_FONT {Helvetica 12 bold italic};   # reserved identifier font
set TERM_FONT {Helvetica 12 bold};               # font for other terminals
set NON_TERM_FONT {Helvetica 12};                # non-terminal font
set TOKEN_FONT {Monaco 12 bold};                 # special symbol token font
set STRING_FONT {Courier 12 bold};               # quoted string font
set RADIUS 9;                                    # default turn radius
set HSEP 15;                                     # horizontal separation
set VSEP 7;                                      # vertical separation
set LWIDTH 3;                                    # line width
set DPI 96;                                      # dots per inch for GIFs

set tagcnt 0; # tag counter - don't modify this


# ---------------------------------------------------------------------------
# Draw a right-hand turn around.  Approximately a ")"
# ---------------------------------------------------------------------------
#
proc draw_right_turnback {tag x y0 y1} {
  global RADIUS
  global LWIDTH
  if {$y0 + 2*$RADIUS < $y1} {
    set xr0 [expr {$x-$RADIUS}]
    set xr1 [expr {$x+$RADIUS}]
    .c create arc $xr0 $y0 $xr1 [expr {$y0+2*$RADIUS}] \
          -width $LWIDTH -start 90 -extent -90 -tags $tag -style arc
    set yr0 [expr {$y0+$RADIUS}]
    set yr1 [expr {$y1-$RADIUS}]
    if {abs($yr1-$yr0)>$RADIUS*2} {
      set half_y [expr {($yr1+$yr0)/2}]
      .c create line $xr1 $yr0 $xr1 $half_y -width $LWIDTH -tags $tag -arrow last
      .c create line $xr1 $half_y $xr1 $yr1 -width $LWIDTH -tags $tag
    } else {
      .c create line $xr1 $yr0 $xr1 $yr1 -width $LWIDTH -tags $tag
    }
    .c create arc $xr0 [expr {$y1-2*$RADIUS}] $xr1 $y1 \
          -width $LWIDTH -start 0 -extent -90 -tags $tag -style arc
  } else { 
    set r [expr {($y1-$y0)/2.0}]
    set x0 [expr {$x-$r}]
    set x1 [expr {$x+$r}]
    .c create arc $x0 $y0 $x1 $y1 \
          -width $LWIDTH -start 90 -extent -180 -tags $tag -style arc
  }
} ;# end draw_right_turnback


# ---------------------------------------------------------------------------
# Draw a left-hand turn around.  Approximatley a "("
# ---------------------------------------------------------------------------
#
proc draw_left_turnback {tag x y0 y1 dir} {
  global RADIUS
  global LWIDTH
  if {$y0 + 2*$RADIUS < $y1} {
    set xr0 [expr {$x-$RADIUS}]
    set xr1 [expr {$x+$RADIUS}]
    .c create arc $xr0 $y0 $xr1 [expr {$y0+2*$RADIUS}] \
          -width $LWIDTH -start 90 -extent 90 -tags $tag -style arc
    set yr0 [expr {$y0+$RADIUS}]
    set yr1 [expr {$y1-$RADIUS}]
    if {abs($yr1-$yr0)>$RADIUS*3} {
      set half_y [expr {($yr1+$yr0)/2}]
      if {$dir=="down"} {
        .c create line $xr0 $yr0 $xr0 $half_y -width $LWIDTH -tags $tag -arrow last
        .c create line $xr0 $half_y $xr0 $yr1 -width $LWIDTH -tags $tag
      } else {
        .c create line $xr0 $yr1 $xr0 $half_y -width $LWIDTH -tags $tag -arrow last
        .c create line $xr0 $half_y $xr0 $yr0 -width $LWIDTH -tags $tag
      }
    } else {
      .c create line $xr0 $yr0 $xr0 $yr1 -width $LWIDTH -tags $tag
    }
    # .c create line $xr0 $yr0 $xr0 $yr1 -width $LWIDTH -tags $tag
    .c create arc $xr0 [expr {$y1-2*$RADIUS}] $xr1 $y1 \
          -width $LWIDTH -start 180 -extent 90 -tags $tag -style arc
  } else { 
    set r [expr {($y1-$y0)/2.0}]
    set x0 [expr {$x-$r}]
    set x1 [expr {$x+$r}]
    .c create arc $x0 $y0 $x1 $y1 \
          -width $LWIDTH -start 90 -extent 180 -tags $tag -style arc
  }
} ;# end draw_left_turnback


# ---------------------------------------------------------------------------
# Draw a bubble containing $txt. 
# ---------------------------------------------------------------------------
#
proc draw_bubble {txt} {
  global LWIDTH
  global tagcnt
  incr tagcnt
  set tag x$tagcnt
  if {$txt=="nil"} {
    .c create line 0 0 1 0 -width $LWIDTH -tags $tag
    return [list $tag 1 0]
  } elseif {$txt=="bullet"} {
    .c create oval 0 -3 6 3 -width $LWIDTH -tags $tag
    return [list $tag 6 0]
  }
# special name replacements
  set isQuotedString 0
  if {$txt=="SPACE"} {
    set label "' '"
  } elseif {$txt=="OCTOTHORPE"} {
    set label "\#"
  } elseif {$txt=="BACKSLASH"} {
    set label "\\"
  } elseif {$txt=="SINGLE_QUOTE"} {
    set label "\'"
  } elseif {$txt=="DOUBLE_QUOTE"} {
    set label "\""
  } elseif {$txt=="DOUBLE_DQUOTE"} {
    set label "\"\""
  } elseif {$txt=="LBRACE" || $txt=="LEFT_BRACE"} {
    set label "\{"
  } elseif {$txt=="RBRACE" || $txt=="RIGHT_BRACE"} {
    set label "\}"
  } elseif {$txt=="_IMPLEMENTATION_DEFINED_"} {
    set label " implementation defined "
  } else {
    set label $txt
  }
# initialise symbol flags
  set isReservedIdent 0
  set isNonTerminal 0
  set isTerminal 0
  set isToken 0
# determine type of symbol
  if {[regexp {^[A-Z][A-Z]+$} $label]} {
    # reserved word
    set isTerminal 1
    set isReservedWord 1
    set font $::RES_WORD_FONT
    set label " $label "
    set baseline [expr {$LWIDTH/2}]
  } elseif {[regexp {^/[A-Z][A-Z]+$} $label]} {
    set label [string range $label 1 end]
    # reserved identifier
    set isTerminal 1
    set isReservedIdent 1
    set font $::RES_IDENT_FONT
    set label " $label "
    set baseline [expr {$LWIDTH/2}]
  } elseif {[regexp {^[A-Z][a-z0-9][a-zA-Z0-9]*$} $label]} {
    # other terminal
    set isTerminal 1
    set font $::TERM_FONT
    set label " $label "
    set baseline [expr {$LWIDTH/2}]
  } elseif {[regexp {^[a-z][a-zA-Z0-9]+$} $label]} {
    # non-terminal
    set isNonTerminal 1
    set font $::NON_TERM_FONT
    set label "  $label  "
    set baseline 0
 } elseif {[regexp {^`[a-zA-Z0-9]+$} $label]} {
    # quoted string literal
    set label [string range $label 1 end]
    set isToken 1
    set font $::STRING_FONT
    set label " \"$label\" "
    set baseline [expr {$LWIDTH/2}]
 } elseif {[regexp {^[a-zA-Z]$} $label]} {
    # single letter token
    set isToken 1
    set font $::TOKEN_FONT
    set baseline [expr {$LWIDTH/2}]
 } elseif {[regexp {^[a-zA-Z0-9]\.\.[a-zA-Z0-9]$} $label]} {
    # letter or digit range
    set isToken 1
    set font $::TOKEN_FONT
    set baseline [expr {$LWIDTH/2}]
 } else {
    # anything else is a token
    set isToken 1
    set font $::TOKEN_FONT
    set baseline [expr {$LWIDTH/2}]
  }
  set id1 [.c create text 0 $baseline -anchor c -text $label -font $font -tags $tag]
# lassign [.c bbox $id1] x0 y0 x1 y1 # to do: replace all foreach with lassign
  foreach {x0 y0 x1 y1} [.c bbox $id1] break
# move parentheses, brackets, braces and underscore up by one pixel
  if {$label in {( ) [ ] \{ \} _ }} { .c move $id1 0 -1 }
# move the asterisk down by one pixel
  if {$label=="*"} { .c move $id1 0 1 }
# move label left by one pixel if font is italic
  set slantAttr [font actual $font -slant]
  if {$slantAttr eq "italic"} { .c move $id1 -1 0 }
  set h [expr {$y1-$y0+$LWIDTH}]
  set rad [expr {($h+1)/2}]
  if {$isNonTerminal} {
    set top [expr {$y0-$LWIDTH}]
    set btm [expr {$y1+$LWIDTH}]
    set left [expr {$x0-$LWIDTH}]
    set right [expr {$x1+$LWIDTH}]
    .c create rect $left $top $right $btm -width $LWIDTH -tags $tag
  } else {
    set top [expr {$y0-$LWIDTH}]
    set btm [expr {$y1+1}]
    set left [expr {$x0+$LWIDTH}]
    set right [expr {$x1-$LWIDTH}]
    if {$left>$right} {
      set left [expr {($x0+$x1)/2}]
      set right $left
    }
    .c create arc [expr {$left-$rad}] $top [expr {$left+$rad}] $btm \
         -width $LWIDTH -start 90 -extent 180 -style arc -tags $tag
    .c create arc [expr {$right-$rad}] $top [expr {$right+$rad}] $btm \
         -width $LWIDTH -start -90 -extent 180 -style arc -tags $tag
    if {$left<$right} {
      .c create line $left $top $right $top -width $LWIDTH -tags $tag
      .c create line $left $btm $right $btm -width $LWIDTH -tags $tag
    }
  }
  foreach {x0 y0 x1 y1} [.c bbox $tag] break
  set width [expr {$x1-$x0}]
  .c move $tag [expr {-$x0}] 0

  # Entry is always 0 0
  # Return:  TAG EXIT_X EXIT_Y
  #
  return [list $tag $width 0]
} ;# end draw_bubble


# ---------------------------------------------------------------------------
# Draw a sequence of terms from left to write.
# ---------------------------------------------------------------------------
# Each element of $lx describes a single term.
#
proc draw_line {lx} {
  global LWIDTH
  global tagcnt
  incr tagcnt
  set tag x$tagcnt

  set sep $::HSEP
  set exx 0
  set exy 0
  foreach term $lx {
    set m [draw_diagram $term]
    foreach {t texx texy} $m break
    if {$exx>0} {
      set xn [expr {$exx+$sep}]
      .c move $t $xn $exy
      .c create line [expr {$exx-1}] $exy $xn $exy \
         -tags $tag -width $LWIDTH -arrow last
      set exx [expr {$xn+$texx}]
    } else {
      set exx $texx
    }
    set exy $texy
    .c addtag $tag withtag $t
    .c dtag $t $t
  }
  if {$exx==0} {	
    set exx [expr {$sep*2}]
    .c create line 0 0 $sep 0 -width $LWIDTH -tags $tag -arrow last
    .c create line $sep 0 $exx 0 -width $LWIDTH -tags $tag
    set exx $sep
  }
  return [list $tag $exx $exy]
} ;# end draw_line


# ---------------------------------------------------------------------------
# Draw a sequence of terms from right to left.
# ---------------------------------------------------------------------------
#
proc draw_backwards_line {lx} {
  global LWIDTH
  global tagcnt
  incr tagcnt
  set tag x$tagcnt

  set sep $::HSEP
  set exx 0
  set exy 0
  set lb {}
  set n [llength $lx]
  for {set i [expr {$n-1}]} {$i>=0} {incr i -1} {
    lappend lb [lindex $lx $i]
  }
  foreach term $lb {
    set m [draw_diagram $term]
    foreach {t texx texy} $m break
    foreach {tx0 ty0 tx1 ty1} [.c bbox $t] break
    set w [expr {$tx1-$tx0}]
    if {$exx>0} {
      set xn [expr {$exx+$sep}]
      .c move $t $xn 0
      .c create line $exx $exy $xn $exy -tags $tag -width $LWIDTH -arrow first
      set exx [expr {$xn+$texx}]
    } else {
      set exx $texx
    }
    set exy $texy
    .c addtag $tag withtag $t
    .c dtag $t $t
  }
  if {$exx==0} {
    .c create line 0 0 $sep 0 -width $LWIDTH -tags $tag
    set exx $sep
  }
  return [list $tag $exx $exy]
} ;# end draw_backwards_line


# ---------------------------------------------------------------------------
# Draw a sequence of terms from top to bottom.
# ---------------------------------------------------------------------------
#
proc draw_stack {indent lx} {
  global tagcnt RADIUS VSEP LWIDTH
  incr tagcnt
  set tag x$tagcnt

  set sep [expr {$VSEP*2}]
  set btm 0
  set n [llength $lx]
  set i 0
  set next_bypass_y 0

  foreach term $lx {
    set bypass_y $next_bypass_y
    if {$i>0 && $i<$n && [llength $term]>1 &&
        ([lindex $term 0]=="opt" || [lindex $term 0]=="optx")} {
      set bypass 1
      set term "line [lrange $term 1 end]"
    } else {
      set bypass 0
      set next_bypass_y 0
    }
    set m [draw_diagram $term]
    foreach {t exx exy} $m break
    foreach {tx0 ty0 tx1 ty1} [.c bbox $t] break
    if {$i==0} {
      set btm $ty1
      set exit_y $exy
      set exit_x $exx
    } else {
      set enter_y [expr {$btm - $ty0 + $sep*2 + 2}]
      if {$bypass} {set next_bypass_y [expr {$enter_y - $RADIUS}]}
      set enter_x [expr {$sep + $indent}]
      set back_y [expr {$btm + $sep + 1}]
      if {$bypass_y>0} {
         set mid_y [expr {($bypass_y+$RADIUS+$back_y)/2}]
         .c create line $bypass_x $bypass_y $bypass_x $mid_y \
            -width $LWIDTH -tags $tag -arrow last
         .c create line $bypass_x $mid_y $bypass_x [expr {$back_y+$RADIUS}] \
             -tags $tag -width $LWIDTH
      }
      .c move $t $enter_x $enter_y
      set e2 [expr {$exit_x + $sep}]
      .c create line $exit_x $exit_y $e2 $exit_y \
            -width $LWIDTH -tags $tag
      draw_right_turnback $tag $e2 $exit_y $back_y
      set e3 [expr {$enter_x-$sep}]
      set bypass_x [expr {$e3-$RADIUS}]
      set emid [expr {($e2+$e3)/2}]
      .c create line $e2 $back_y $emid $back_y \
                 -width $LWIDTH -tags $tag -arrow last
      .c create line $emid $back_y $e3 $back_y \
                 -width $LWIDTH -tags $tag
      set r2 [expr {($enter_y - $back_y)/2.0}]
      draw_left_turnback $tag $e3 $back_y $enter_y down
      .c create line $e3 $enter_y $enter_x $enter_y \
                 -arrow last -width $LWIDTH -tags $tag
      set exit_x [expr {$enter_x + $exx}]
      set exit_y [expr {$enter_y + $exy}]
    }
    .c addtag $tag withtag $t
    .c dtag $t $t
    set btm [lindex [.c bbox $tag] 3]
    incr i
  }
  if {$bypass} {
    set fwd_y [expr {$btm + $sep + 1}]
    set mid_y [expr {($next_bypass_y+$RADIUS+$fwd_y)/2}]
    set descender_x [expr {$exit_x+$RADIUS}]
    .c create line $bypass_x $next_bypass_y $bypass_x $mid_y \
        -width $LWIDTH -tags $tag -arrow last
    .c create line $bypass_x $mid_y $bypass_x [expr {$fwd_y-$RADIUS}] \
        -tags $tag -width $LWIDTH
    .c create arc $bypass_x [expr {$fwd_y-2*$RADIUS}] \
                  [expr {$bypass_x+2*$RADIUS}] $fwd_y \
        -width $LWIDTH -start 180 -extent 90 -tags $tag -style arc
    .c create arc [expr {$exit_x-$RADIUS}] $exit_y \
                  $descender_x [expr {$exit_y+2*$RADIUS}] \
        -width $LWIDTH -start 90 -extent -90 -tags $tag -style arc
    .c create arc $descender_x [expr {$fwd_y-2*$RADIUS}] \
                  [expr {$descender_x+2*$RADIUS}] $fwd_y \
        -width $LWIDTH -start 180 -extent 90 -tags $tag -style arc
    set exit_x [expr {$exit_x+2*$RADIUS}]
    set half_x [expr {($exit_x+$indent)/2}]
    .c create line [expr {$bypass_x+$RADIUS}] $fwd_y $half_x $fwd_y \
        -width $LWIDTH -tags $tag -arrow last
    .c create line $half_x $fwd_y $exit_x $fwd_y \
        -width $LWIDTH -tags $tag
    .c create line $descender_x [expr {$exit_y+$RADIUS}] \
                   $descender_x [expr {$fwd_y-$RADIUS}] \
        -width $LWIDTH -tags $tag -arrow last
    set exit_y $fwd_y
  }
  set width [lindex [.c bbox $tag] 2]
  return [list $tag $exit_x $exit_y]
} ;# end draw_stack


# ---------------------------------------------------------------------------
# Draw a loop
# ---------------------------------------------------------------------------
#
proc draw_loop {forward back} {
  global LWIDTH
  global tagcnt
  incr tagcnt
  set tag x$tagcnt
  set sep $::HSEP
  set vsep $::VSEP
  if {$back in {. , ; |}} {
    set vsep 0
  } elseif {$back=="SINGLE_QUOTE"} {
    set vsep 0
  } elseif {$back=="nil"} {
    set vsep [expr {$vsep/2}]
  }

  foreach {ft fexx fexy} [draw_diagram $forward] break
  foreach {fx0 fy0 fx1 fy1} [.c bbox $ft] break
  set fw [expr {$fx1-$fx0}]
  foreach {bt bexx bexy} [draw_backwards_line $back] break
  foreach {bx0 by0 bx1 by1} [.c bbox $bt] break
  set bw [expr {$bx1-$bx0}]
  set dy [expr {$fy1 - $by0 + $vsep}]
  .c move $bt 0 $dy
  set biny $dy
  set bexy [expr {$dy+$bexy}]
  set by0 [expr {$dy+$by0}]
  set by1 [expr {$dy+$by1}]

  if {$fw>$bw} {
    if {$fexx<$fw && $fexx>=$bw} {
      set dx [expr {($fexx-$bw)/2}]
      .c move $bt $dx 0
      set bexx [expr {$dx+$bexx}]
      .c create line 0 $biny $dx $biny -width $LWIDTH -tags $bt
      .c create line $bexx $bexy $fexx $bexy -width $LWIDTH -tags $bt -arrow first
      set mxx $fexx
    } else {
      set dx [expr {($fw-$bw)/2}]
      .c move $bt $dx 0
      set bexx [expr {$dx+$bexx}]
      .c create line 0 $biny $dx $biny -width $LWIDTH -tags $bt
      .c create line $bexx $bexy $fx1 $bexy -width $LWIDTH -tags $bt -arrow first
      set mxx $fexx
    }
  } elseif {$bw>$fw} {
    set dx [expr {($bw-$fw)/2}]
    .c move $ft $dx 0
    set fexx [expr {$dx+$fexx}]
    .c create line 0 0 $dx $fexy -width $LWIDTH -tags $ft -arrow last
    .c create line $fexx $fexy $bx1 $fexy -width $LWIDTH -tags $ft
    set mxx $bexx
  }
  .c addtag $tag withtag $bt
  .c addtag $tag withtag $ft
  .c dtag $bt $bt
  .c dtag $ft $ft
  .c move $tag $sep 0
  set mxx [expr {$mxx+$sep}]
  .c create line 0 0 $sep 0 -width $LWIDTH -tags $tag
  draw_left_turnback $tag $sep 0 $biny up
  draw_right_turnback $tag $mxx $fexy $bexy
  foreach {x0 y0 x1 y1} [.c bbox $tag] break
  set exit_x [expr {$mxx+$::RADIUS}]
  .c create line $mxx $fexy $exit_x $fexy -width $LWIDTH -tags $tag
  return [list $tag $exit_x $fexy]
} ;# end draw_loop


# ---------------------------------------------------------------------------
# Draw a top-loop
# ---------------------------------------------------------------------------
#
proc draw_toploop {forward back} {
  global LWIDTH
  global tagcnt
  incr tagcnt
  set tag x$tagcnt
  set sep $::VSEP
  set vsep [expr {$sep/2}]

  foreach {ft fexx fexy} [draw_diagram $forward] break
  foreach {fx0 fy0 fx1 fy1} [.c bbox $ft] break
  set fw [expr {$fx1-$fx0}]
  foreach {bt bexx bexy} [draw_backwards_line $back] break
  foreach {bx0 by0 bx1 by1} [.c bbox $bt] break
  set bw [expr {$bx1-$bx0}]
  set dy [expr {-($by1 - $fy0 + $vsep)}]
  .c move $bt 0 $dy
  set biny $dy
  set bexy [expr {$dy+$bexy}]
  set by0 [expr {$dy+$by0}]
  set by1 [expr {$dy+$by1}]

  if {$fw>$bw} {
    set dx [expr {($fw-$bw)/2}]
    .c move $bt $dx 0
    set bexx [expr {$dx+$bexx}]
    .c create line 0 $biny $dx $biny -width $LWIDTH -tags $bt
    .c create line $bexx $bexy $fx1 $bexy -width $LWIDTH -tags $bt -arrow first
    set mxx $fexx
  } elseif {$bw>$fw} {
    set dx [expr {($bw-$fw)/2}]
    .c move $ft $dx 0
    set fexx [expr {$dx+$fexx}]
    .c create line 0 0 $dx $fexy -width $LWIDTH -tags $ft
    .c create line $fexx $fexy $bx1 $fexy -width $LWIDTH -tags $ft
    set mxx $bexx
  }
  .c addtag $tag withtag $bt
  .c addtag $tag withtag $ft
  .c dtag $bt $bt
  .c dtag $ft $ft
  .c move $tag $sep 0
  set mxx [expr {$mxx+$sep}]
  .c create line 0 0 $sep 0 -width $LWIDTH -tags $tag
  draw_left_turnback $tag $sep 0 $biny down
  draw_right_turnback $tag $mxx $fexy $bexy
  foreach {x0 y0 x1 y1} [.c bbox $tag] break
  .c create line $mxx $fexy $x1 $fexy -width $LWIDTH -tags $tag
  return [list $tag $x1 $fexy]
} ;# end draw_toploop


# ---------------------------------------------------------------------------
# Draw alternative branches
# ---------------------------------------------------------------------------
#
proc draw_or {lx} {
  global LWIDTH
  global tagcnt
  incr tagcnt
  set tag x$tagcnt
  set sep $::VSEP
  set vsep [expr {$sep/2}]
  set n [llength $lx]
  set i 0
  set mxw 0
  foreach term $lx {
    set m($i) [set mx [draw_diagram $term]]
    set tx [lindex $mx 0]
    foreach {x0 y0 x1 y1} [.c bbox $tx] break
    set w [expr {$x1-$x0}]
    if {$i>0} {set w [expr {$w+20+2*$LWIDTH-1}]}  ;# extra space for arrowheads
    if {$w>$mxw} {set mxw $w}
    incr i
  }

  set x0 0                        ;# entry x
  set x1 $sep                     ;# decender 
  set x2 [expr {$sep*2}]          ;# start of choice
  set xc [expr {$mxw/2}]          ;# center point
  set x3 [expr {$mxw+$x2}]        ;# end of choice
  set x4 [expr {$x3+$sep}]        ;# accender
  set x5 [expr {$x4+$sep}]        ;# exit x

  for {set i 0} {$i<$n} {incr i} {
    foreach {t texx texy} $m($i) break
    foreach {tx0 ty0 tx1 ty1} [.c bbox $t] break
    set w [expr {$tx1-$tx0}]
    set dx [expr {($mxw-$w)/2 + $x2}]
    if {$w>10 && $dx>$x2+10} {set dx [expr {$x2+10}]}
    .c move $t $dx 0
    set texx [expr {$texx+$dx}]
    set m($i) [list $t $texx $texy]
    foreach {tx0 ty0 tx1 ty1} [.c bbox $t] break
    if {$i==0} {
      if {$dx>$x2} {set ax last} {set ax none}
      .c create line 0 0 $dx 0 -width $LWIDTH -tags $tag -arrow $ax
      .c create line $texx $texy [expr {$x5+1}] $texy -width $LWIDTH -tags $tag
      set exy $texy
      .c create arc -$sep 0 $sep [expr {$sep*2}] \
         -width $LWIDTH -start 90 -extent -90 -tags $tag -style arc
      set btm $ty1
    } else {
      set dy [expr {$btm - $ty0 + $vsep}]
      if {$dy<2*$sep} {set dy [expr {2*$sep}]}
      .c move $t 0 $dy
      set texy [expr {$texy+$dy}]
      if {$dx>$x2} {
        .c create line $x2 $dy $dx $dy -width $LWIDTH -tags $tag -arrow last
        if {$dx<$xc-2} {set ax last} {set ax none}
        .c create line $texx $texy $x3 $texy -width $LWIDTH -tags $tag -arrow $ax
      }
      set y1 [expr {$dy-2*$sep}]
      .c create arc $x1 $y1 [expr {$x1+2*$sep}] $dy \
          -width $LWIDTH -start 180 -extent 90 -style arc -tags $tag
      set y2 [expr {$texy-2*$sep}]
      .c create arc [expr {$x3-$sep}] $y2 $x4 $texy \
          -width $LWIDTH -start 270 -extent 90 -style arc -tags $tag
      if {$i==$n-1} {
        .c create arc $x4 $exy [expr {$x4+2*$sep}] [expr {$exy+2*$sep}] \
           -width $LWIDTH -start 180 -extent -90 -tags $tag -style arc
        .c create line $x1 [expr {$dy-$sep}] $x1 $sep -width $LWIDTH -tags $tag
        .c create line $x4 [expr {$texy-$sep}] $x4 [expr {$exy+$sep}] \
               -width $LWIDTH -tags $tag
      }
      set btm [expr {$ty1+$dy}]
    }
    .c addtag $tag withtag $t
    .c dtag $t $t
  }
  return [list $tag $x5 $exy]   
} ;# end draw_or


# ---------------------------------------------------------------------------
# Draw a tail-branch
# ---------------------------------------------------------------------------
#
proc draw_tail_branch {lx} {
  global LWIDTH
  global tagcnt
  incr tagcnt
  set tag x$tagcnt
  set sep $::VSEP
  set vsep [expr {$sep/2}]
  set n [llength $lx]
  set i 0
  foreach term $lx {
    set m($i) [set mx [draw_diagram $term]]
    incr i
  }

  set x0 0                        ;# entry x
  set x1 $sep                     ;# decender 
  set x2 [expr {$sep*2}]          ;# start of choice

  for {set i 0} {$i<$n} {incr i} {
    foreach {t texx texy} $m($i) break
    foreach {tx0 ty0 tx1 ty1} [.c bbox $t] break
    set dx [expr {$x2+10}]
    .c move $t $dx 0
    foreach {tx0 ty0 tx1 ty1} [.c bbox $t] break
    if {$i==0} {
      .c create line 0 0 $dx 0 -width $LWIDTH -tags $tag -arrow last
      .c create arc -$sep 0 $sep [expr {$sep*2}] \
         -width $LWIDTH -start 90 -extent -90 -tags $tag -style arc
      set btm $ty1
    } else {
      set dy [expr {$btm - $ty0 + $vsep}]
      if {$dy<2*$sep} {set dy [expr {2*$sep}]}
      .c move $t 0 $dy
      if {$dx>$x2} {
        .c create line $x2 $dy $dx $dy -width $LWIDTH -tags $tag -arrow last
      }
      set y1 [expr {$dy-2*$sep}]
      .c create arc $x1 $y1 [expr {$x1+2*$sep}] $dy \
          -width $LWIDTH -start 180 -extent 90 -style arc -tags $tag
      if {$i==$n-1} {
        .c create line $x1 [expr {$dy-$sep}] $x1 $sep -width $LWIDTH -tags $tag
      }
      set btm [expr {$ty1+$dy}]
    }
    .c addtag $tag withtag $t
    .c dtag $t $t
  }
  return [list $tag 0 0]
} ;# end draw_tail_branch


# ---------------------------------------------------------------------------
# Draw a single diagram body
# ---------------------------------------------------------------------------
#
proc draw_diagram {spec} {
  set n [llength $spec]
  if {$n==1} {
    return [draw_bubble $spec]
  }
  if {$n==0} {
    return [draw_bubble nil]
  }
  set cmd [lindex $spec 0]
  if {$cmd=="line"} {
    return [draw_line [lrange $spec 1 end]]
  }
  if {$cmd=="stack"} {
    return [draw_stack 0 [lrange $spec 1 end]]
  }
  if {$cmd=="indentstack"} {
    return [draw_stack $::HSEP [lrange $spec 1 end]]
  }
  if {$cmd=="loop"} {
    return [draw_loop [lindex $spec 1] [lindex $spec 2]]
  }
  if {$cmd=="toploop"} {
    return [draw_toploop [lindex $spec 1] [lindex $spec 2]]
  }
  if {$cmd=="or"} {
    return [draw_or [lrange $spec 1 end]]
  }
  if {$cmd=="opt"} {
    set args [lrange $spec 1 end]
    if {[llength $args]==1} {
      return [draw_or [list nil [lindex $args 0]]]
    } else {
      return [draw_or [list nil "line $args"]]
    }
  }
  if {$cmd=="optx"} {
    set args [lrange $spec 1 end]
    if {[llength $args]==1} {
      return [draw_or [list [lindex $args 0] nil]]
    } else {
      return [draw_or [list "line $args" nil]]
    }
  }
  if {$cmd=="tailbranch"} {
    # return [draw_tail_branch [lrange $spec 1 end]]
    return [draw_or [lrange $spec 1 end]]
  }
  error "unknown operator: $cmd"
} ;# end draw_diagram


# ---------------------------------------------------------------------------
# Draw a single production
# ---------------------------------------------------------------------------
#
proc draw_graph {name spec {do_xv 1}} {
  .c delete all
  wm deiconify .
  wm title . $name
  draw_diagram "line bullet [list $spec] bullet"
  foreach {x0 y0 x1 y1} [.c bbox all] break
  .c move all [expr {2-$x0}] [expr {2-$y0}]
  foreach {x0 y0 x1 y1} [.c bbox all] break
  .c config -width $x1 -height $y1
  update
  .c postscript -file $name.ps -width [expr {$x1+2}] -height [expr {$y1+2}]
#
#  uncomment to enable GIF output (this may not work on all systems) ...
#
#  global DPI
#  exec convert -density ${DPI}x$DPI -antialias $name.ps $name.gif
#  if {$do_xv} {
#    exec xv $name.gif &
#  }
#
} ;# end draw_graph


# ---------------------------------------------------------------------------
#  Draw group of productions
# ---------------------------------------------------------------------------
#
proc draw_graphs {group} {
  set f [open all.html w]
  foreach {name graph} $group {
    if {[regexp {^X-} $name]} continue
    puts $f "<h3>$name:</h3>"
    puts $f "<img src=\"$name.gif\">"
    draw_graph $name $graph 0
    set img($name) 1
    set children($name) {}
    set parents($name) {}
  }
  close $f
  set order {}
  foreach {name graph} $group {
    lappend order $name
    unset -nocomplain v
    walk_graph_extract_names $group v
    unset -nocomplain v($name)
    foreach x [array names v] {
      if {![info exists img($x)]} continue
      lappend children($name) $x
      lappend parents($x) $name
    }
  }
  set f [open syntax_linkage.tcl w]
  foreach name [lsort [array names img]] {
    set cx [lsort $children($name)]
    set px [lsort $parents($name)]
    puts $f [list set syntax_linkage($name) [list $cx $px]]
  }
  puts $f [list set syntax_order $order]
  close $f
  wm withdraw .
} ;# end draw_graphs


# ---------------------------------------------------------------------------
#  Draw all productions
# ---------------------------------------------------------------------------
#
proc draw_all_graphs {} {
  global non_terminals
  global terminals
  global pragmas
  global ignore_symbols
  global extensions
  global aliases
  global legend
  draw_graphs $non_terminals
  draw_graphs $terminals
  draw_graphs $pragmas
  draw_graphs $ignore_symbols
  draw_graphs $extensions
  draw_graphs $legend
} ;# end draw_all_graphs


# ---------------------------------------------------------------------------
# Obtain the names of all productions
# ---------------------------------------------------------------------------
#
proc walk_graph_extract_names {graph varname} {
  upvar 1 $varname v
  foreach x $graph {
    set n [llength $x]
    if {$n>1} {
      walk_graph_extract_names $x v
    } elseif {[regexp {^[a-z]} $x]} {
      set v($x) 1
    }
  }
} ;# end walk_graph_extract_names

#
# END OF FILE
