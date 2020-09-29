(*!m2pim*) (* Copyright (c) 2015, 2020 Modula-2 Software Foundation *)

IMPLEMENTATION MODULE AST;

(* Abstract Syntax Tree Implementation for Modula-2 R10 Bootstrap Kernel *)


IMPORT AstNodeType, AstQueue, LexQueue;

FROM AstNodeType IMPORT AstNodeTypeT;
FROM AstQueue IMPORT AstQueueT;
FROM LexQueue IMPORT LexQueueT;


(* Generic Ast Node *)

TYPE AstT = POINTER TO AstNode;

TYPE AstNode = RECORD
  type : AstNodeType;
END; (* AstNode *)


(* Ast Node of Arity 0 *)

TYPE Ast0 = POINTER TO AstNode0;

TYPE AstNode0 = RECORD
  type : AstNodeType.Arity0
END; (* AstNode0 *)


(* Ast Node of Arity 1 *)

TYPE Ast1 = POINTER TO AstNode1;

TYPE AstNode1 = RECORD
  type : AstNodeType.Arity1;
  subnode : AstT
END; (* AstNode1 *)


(* Ast Node of Arity 2 *)

TYPE Ast2 = POINTER TO AstNode2;

TYPE AstNode2 = RECORD
  type : AstNodeType.Arity2;
  subnode : ARRAY [0 .. 1] OF AstT
END; (* AstNode2 *)


(* Ast Node of Arity 3 *)

TYPE Ast3 = POINTER TO AstNode3;

TYPE AstNode3 = RECORD
  type : AstNodeType.Arity3;
  subnode : ARRAY [0 .. 2] OF AstT
END; (* AstNode3 *)


(* Ast Node of Arity 4 *)

TYPE Ast4 = POINTER TO AstNode4;

TYPE AstNode4 = RECORD
  type : AstNodeType.Arity4;
  subnode : ARRAY [0 .. 3] OF AstT
END; (* AstNode4 *)


(* Ast Node of Arity N *)

TYPE AstN = POINTER TO AstNodeN;

TYPE AstNodeN = RECORD
  type : AstNodeType.Variadic;
  arity : CARDINAL;
  subnode : ADDRESS
END; (* AstNodeN *)


(* Terminal Node of Arity 1 *)

TYPE Term1 = POINTER TO TermNode1;

TYPE TermNode1 = RECORD
  type : AstNodeType.Terminal1;
  value : LexemeT
END; (* TermNode1 *)


(* Terminal Node of Arity N *)

TYPE TermN = POINTER TO TermNodeN;

TYPE TermNodeN = RECORD
  type : AstNodeType.TerminalN;
  value : ADDRESS
END; (* TermNodeN *)


(* --------------------------------------------------------------------------
 * public function new0aryNode(type)
 * --------------------------------------------------------------------------
 * Allocates and returns a new nullary AST node of the given type.
 * --------------------------------------------------------------------------
 *)
PROCEDURE new0aryNode ( type : AstNodeType.Arity0 ) : AstT;

VAR
  newNode : Ast0;
  
BEGIN
  Storage.Allocate(newNode, TSIZE(AstNode0));
  newNode^.type := type;
  RETURN AstT(newNode)
END new0aryNode;


(* --------------------------------------------------------------------------
 * public procedure newUnaryNode(type, subnode)
 * --------------------------------------------------------------------------
 * Allocates and returns a new unary AST node of the given type.
 * --------------------------------------------------------------------------
 *)
PROCEDURE newUnaryNode ( type : AstNodeType.Arity1; subnode : AstT ) : AstT;

VAR
  newNode : Ast1;
  
BEGIN
  Storage.Allocate(newNode, TSIZE(AstNode1));
  newNode^.type := type;
  newNode^.subnode := subnode;
  RETURN AstT(newNode)
END newUnaryNode;


(* --------------------------------------------------------------------------
 * public function newBinaryNode(type, subnode0, subnode1)
 * --------------------------------------------------------------------------
 * Allocates and returns a new binary AST node of the given type.
 * --------------------------------------------------------------------------
 *)
PROCEDURE newBinaryNode
  ( VAR node : type : AstNodeType.Arity2; subnode0, subnode1 : AstT );

VAR
  newNode : Ast2;
  
BEGIN
  Storage.Allocate(newNode, TSIZE(AstNode2));
  newNode^.type := type;
  newNode^.subnode[0] := subnode0;
  newNode^.subnode[1] := subnode1;
  RETURN AstT(newNode)
END newBinaryNode;


(* --------------------------------------------------------------------------
 * public function new3naryNode(type, subnode0, subnode1, subnode2)
 * --------------------------------------------------------------------------
 * Allocates and returns a new ternary AST node of the given type.
 * --------------------------------------------------------------------------
 *)
PROCEDURE new3aryNode
  ( type : AstNodeType.Arity3; subnode0, subnode1, subnode2 : AstT ) : AstT;

VAR
  newNode : Ast3;
  
BEGIN
  Storage.Allocate(newNode, TSIZE(AstNode3));
  newNode^.type := type;
  newNode^.subnode[0] := subnode0;
  newNode^.subnode[1] := subnode1;
  newNode^.subnode[2] := subnode2;
  RETURN AstT(newNode)
END new3aryNode;


(* --------------------------------------------------------------------------
 * public function new4aryNode(type, subnode0, subnode1, subnode2)
 * --------------------------------------------------------------------------
 * Allocates and returns a new quaternary AST node of the given type.
 * --------------------------------------------------------------------------
 *)
PROCEDURE new4aryNode
  ( type : AstNodeType.Arity4;
    subnode0, subnode1, subnode2, subnode3 : AstT ) : AstT;

VAR
  newNode : Ast4;
  
BEGIN
  Storage.Allocate(newNode, TSIZE(AstNode4));
  newNode^.type := type;
  newNode^.subnode[0] := sub0;
  newNode^.subnode[1] := sub1;
  newNode^.subnode[2] := sub2;
  newNode^.subnode[3] := sub3;
  RETURN AstT(newNode)
END new4aryNode;


(* --------------------------------------------------------------------------
 * public function newVariadicNode(type, nodeList)
 * --------------------------------------------------------------------------
 * Allocates and returns a new variadic AST node of the given type.
 * --------------------------------------------------------------------------
 *)
PROCEDURE newVariadicNode
  ( type : AstNodeType.Variadic; nodeList : AstQueueT ) : AstT;

VAR
  subnodeCount, index, maxIndex, offset : CARDINAL;
  addr : ADDRESS;
  newNode, subnode : AstT;
  
BEGIN
  subnodeCount := AstQueue.count(nodeList);
  maxIndex := subnodeCount - 1;
  offset := TSIZE(ADDRESS);
  Storage.Allocate(newNode, TSIZE(Ast1) + maxIndex*offset);
  newNode^.type := type;
  newNode^.arity := subnodeCount;
  addr := SYSTEM.ADR(newNode^.subnode);
  FOR index := 0 TO maxIndex DO
    subnode := AstQueue.dequeue(nodeList);
    (* newNode^.subnode[index] := subnode *)
    AstT(addr + ADDRESS(index*offset))^ := subnode
  END; (*FOR*)
  
  RETURN newNode
END newVariadicNode;


(* --------------------------------------------------------------------------
 * public function newUnaryNode(type, value)
 * --------------------------------------------------------------------------
 * Allocates and returns a new terminal AST node of the given type.
 * --------------------------------------------------------------------------
 *)
PROCEDURE newTerminalNode
  ( type : AstNodeType.Terminal1; value : LexemeT ) : AstT;

VAR
  newNode : Ast1;
  
BEGIN
  Storage.Allocate(newNode, TSIZE(TermNode1));
  newNode^.type := type;
  newNode^.value := value;
  RETURN AstT(newNode)
END newUnaryNode;


(* --------------------------------------------------------------------------
 * public function newTermListNode(type, valueList)
 * --------------------------------------------------------------------------
 * Allocates and returns a new terminal list AST node of the given type.
 * --------------------------------------------------------------------------
 *)
PROCEDURE newTermListNode
  ( type : AstNodeType.TerminalN; valueList : LexQueueT ) : AstT;

VAR
  valueCount, index, maxIndex, offset : CARDINAL;
  addr : ADDRESS;
  value : LexemeT;
  newNode : AstT;
  
BEGIN
  valueCount := LexQueue.count(valueList);
  maxIndex := valueCount - 1;
  offset := TSIZE(ADDRESS);
  Storage.Allocate(newNode, TSIZE(Ast1) + maxIndex*offset);
  newNode^.type := type;
  newNode^.arity := valueCount;
  addr := SYSTEM.ADR(newNode^.value);
  FOR index := 0 TO maxIndex DO
    value := LexQueue.dequeue(valueList);
    (* newNode^.value[index] := value *)
    AstT(addr + ADDRESS(index*offset))^ := value
  END; (* FOR *)
  
  RETURN newNode
END newTermListNode;


(* --------------------------------------------------------------------------
 * public function newModuleNode(astNode)
 * --------------------------------------------------------------------------
 * Allocates and returns a new module type AST node.
 * --------------------------------------------------------------------------
 *)
PROCEDURE newModuleNode
  ( type : AstNodeType.Modules;
    identNode, impNode, rxpNode : AstT; defnOrDeclList : AstQueueT ) : AstT;

VAR
  subnodeCount, index, maxIndex, offset : CARDINAL;
  addr : ADDRESS;
  newNode, subnode : AstT;

BEGIN
  (* determine allocation parameters and allocate node *)
  subnodeCount := AstQueue.count(defnOrDeclList) + 3;
  maxIndex := subnodeCount - 1;
  offset := TSIZE(ADDRESS);
  Storage.Allocate(newNode, TSIZE(Ast1) + maxIndex*offset);
  
  (* set node type and arity *)
  newNode^.type := type;
  newNode^.arity := subnodeCount;
  
  (* newNode^.subnode[0] := identNode *)
  addr := SYSTEM.ADR(newNode^.subnode);
  AstT(addr)^ := identNode; 
  
  (* newNode^.subnode[1] := impNode *)
  AstT(addr + ADDRESS(offset))^ := impNode; 
  
  (* newNode^.subnode[2] := rxpNode *)
  AstT(addr + ADDRESS(2*offset))^ := rxpNode; 
  
  (* definition or declaration subnodes *)
  FOR index := 3 TO maxIndex DO
    subnode := AstQueue.dequeue(defnOrDeclList);
    (* newNode^.subnode[index] := subnode *)
    AstT(addr + ADDRESS(index*offset))^ := subnode
  END; (* FOR *)
  
  RETURN newNode
END newModuleNode;
    

(* --------------------------------------------------------------------------
 * public function nodeType(astNode)
 * --------------------------------------------------------------------------
 * Returns the type of the given AST node. Returns Invalid if astNode is NIL.
 * --------------------------------------------------------------------------
 *)
PROCEDURE nodeType ( astNode : AstT ) : AstNodeType;

BEGIN
  (* verify preconditions *)
  IF astNode = NIL THEN
    RETURN AstNodeType.Invalid
  END; (* IF *)
  
  RETURN astNode^.type
END nodeType;


(* --------------------------------------------------------------------------
 * public function arity(astNode)
 * --------------------------------------------------------------------------
 * Returns the arity of the given AST node. Returns 0 if astNode is NIL.
 * --------------------------------------------------------------------------
 *)
PROCEDURE arity ( astNode : AstT ) : CARDINAL;

BEGIN
  (* verify preconditions *)
  IF astNode = NIL THEN
    RETURN 0
  END; (* IF *)
  
  (* variadic nodes have an arity field *)
  IF astNode^.type IN AstNodeType.ArityN THEN
    RETURN AstNodeN(astNode)^.arity
  END; (* IF *)
  
  (* the arity of any other nodes is implicit in the node type *)
  CASE astNode^.type OF
    AstNodeType.Invalid,
    MIN(AstNodeType.Arity0) .. MAX(AstNodeType.Arity0) : RETURN 0
  | MIN(AstNodeType.Arity1) .. MAX(AstNodeType.Arity1) : RETURN 1
  | MIN(AstNodeType.Arity2) .. MAX(AstNodeType.Arity2) : RETURN 2
  | MIN(AstNodeType.Arity3) .. MAX(AstNodeType.Arity3) : RETURN 3
  | MIN(AstNodeType.Arity4) .. MAX(AstNodeType.Arity4) : RETURN 4
  ELSE (* unreachable assert *)
    Halt
  END (* CASE *)  
END arity;


(* --------------------------------------------------------------------------
 * public function subnodeAtIndex(astNode, atIndex)
 * --------------------------------------------------------------------------
 * Returns the subnode at the given index of the given AST node.
 * Returns NIL if astNode is NIL or if index >= arity(astNode).
 * --------------------------------------------------------------------------
 *)
PROCEDURE subnodeAtIndex ( astNode : AstT; index : CARDINAL ) : AstT;

VAR
  addr : ADDRESS;
  offset : CARDINAL;
  type : AstNodeType;
  
BEGIN
  (* verify preconditions *)
  
  IF astNode = NIL THEN (* invalid reference *)
    RETURN NIL
  END; (* IF *)
  
  type := nodeType(astNode);
  
  IF AstNodeType.isTerminal(type)
    OR (type IN AstNodeType.Arity0) THEN (* invalid type *)
    RETURN NIL
  END; (* IF *)
    
  IF index >= arity(astNode) THEN (* index out of bounds *)
    RETURN NIL
  END; (* IF *)
  
  (* RETURN astNode^.subnode[index] *)
  offset := TSIZE(ADDRESS);
  addr := SYSTEM.ADR(AstNodeN(astNode)^.subnode);  
  RETURN AstT(addr + ADDRESS(index*offset))^
END subnodeAtIndex;


(* --------------------------------------------------------------------------
 * public function subnodeAtIndex(astNode, index)
 * --------------------------------------------------------------------------
 * Returns the subnode at the given index of the given AST node.
 * Returns NIL if astNode is NIL or if index >= arity(astNode).
 * --------------------------------------------------------------------------
 *)
PROCEDURE valueAtIndex ( astNode : AstT; index : CARDINAL ) : LexemeT;

VAR
  addr : ADDRESS;
  offset : CARDINAL;
  type : AstNodeType;
  
BEGIN
  (* verify preconditions *)
  
  IF astNode = NIL THEN (* invalid reference *)
    RETURN NIL
  END; (*IF*)
    
  IF AstNodeType.isNonTerminal(type) THEN (* invalid type *)
    RETURN NIL
  END; (*IF*)
  
  type := nodeType(astNode);
  
  IF index >= arity(astNode) THEN (* index out of bounds *)
    RETURN NIL
  END; (*IF*)
  
  (* RETURN astNode^.value[index] *)
  offset := TSIZE(ADDRESS);
  addr := SYSTEM.ADR(TermNodeN(astNode)^.value);  
  RETURN AstT(addr + ADDRESS(index*offset))^
END valueAtIndex;


(* --------------------------------------------------------------------------
 * public procedure Release(astNode)
 * --------------------------------------------------------------------------
 * Releases astNode and passes back NIL if successful.
 * --------------------------------------------------------------------------
 *)
PROCEDURE Release ( VAR ast : AST );

BEGIN

  TO DO
  
END Release;


END AST.