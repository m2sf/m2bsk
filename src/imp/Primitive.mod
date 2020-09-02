(*!m2pim*) (* Copyright (c) 2020 Modula-2 Software Foundation *)

IMPLEMENTATION MODULE Primitive;

FROM String IMPORT StringT;


PROCEDURE isPrimitive ( lexeme : StringT ) : BOOLEAN;
(* Returns TRUE if lexeme represents a primitive identifier, else FALSE. *)

BEGIN
  IF lexeme = NIL THEN
    (* TO DO : error message *)
    HALT
  END; (* IF *)

  CASE String.length(lexeme) OF
    6 :
    CASE String.charAtIndex(lexeme, 1) OF
      'A' :
    
      (* '_ALLOC' *)
      
      IF lexeme = alloc THEN
        RETURN TRUE
      END (* IF *)
    
    | 'S' :
    
      (* '_STDIN' *)
      
      IF lexeme = stdin THEN
        RETURN TRUE
      END (* IF *)
    
    END (* CASE *)

  | 7 :
    CASE String.charAtIndex(lexeme, 1) OF
      'K' :
    
      (* '_KVALUE' *)
      
      IF lexeme = kvalue THEN
        RETURN TRUE
      END (* IF *)
      
    | 'S' :
    
      (* '_STDOUT' *)
      
      IF lexeme = stdout THEN
        RETURN TRUE
      END (* IF *)
    
    END (* CASE *)
    
  | 8 :
    CASE String.charAtIndex(lexeme, 2) OF
      'T' :
    
      (* '_ATSTORE' *)
      
      IF lexeme = atstore THEN
        RETURN TRUE
      
      (* '_ATVALUE' *)
      
      ELSIF lexeme = atvalue THEN
        RETURN TRUE
      END (* IF *)
      
    | 'E' :
    
      (* '_DEALLOC' *)
      
      IF lexeme = dealloc THEN
        RETURN TRUE
      END (* IF *)
    
    | 'V' :
    
      (* '_KVSTORE' *)
      
      IF lexeme = kvstore THEN
        RETURN TRUE
      END (* IF *)
    
     END (* CASE *)
    
  | 9 :
    CASE String.charAtIndex(lexeme, 3) OF
      'I' :
    
      (* '_ATINSERT' *)
      
      IF lexeme = atinsert THEN
        RETURN TRUE
      END (* IF *)
      
    | 'R' :
    
      (* '_ATREMOVE' *)
      
      IF lexeme = atremove THEN
        RETURN TRUE
      END (* IF *)
    
    END (* CASE *)
  
  END; (* CASE *)

  (* no match *)
  RETURN FALSE
END isPrimitive;


BEGIN (* initialise lexemes *)
  alloc := String.forArray("_ALLOC");
  atinsert := String.forArray("_ATINSERT");
  atremove := String.forArray("_ATREMOVE");
  atstore := String.forArray("_ATSTORE");
  atvalue := String.forArray("_ATVALUE");
  dealloc := String.forArray("_DEALLOC");
  kvalue := String.forArray("_KVALUE");
  kvstore := String.forArray("_KVSTORE");
  stdin := String.forArray("_STDIN");
  stdout := String.forArray("_STDOUT")
END Primitive.
