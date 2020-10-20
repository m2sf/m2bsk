(*!m2pim*) (* Copyright (c) 2017, 2020 Modula-2 Software Foundation *)

IMPLEMENTATION MODULE Infile;

(* I/O library for reading text files with line and column counters *)

IMPORT String, BasicFileIO, Hash;

FROM String IMPORT StringT; (* alias for String.String *)

FROM SYSTEM IMPORT TSIZE;
FROM ISO646 IMPORT NUL, LF, CR;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;


(* ---------------------------------------------------------------------------
 * type LexemeBuffer
 * ---------------------------------------------------------------------------
 * Stores length and up to MaxLineLength characters plus NUL terminator.
 * ------------------------------------------------------------------------ *)

TYPE LexemeBuffer = RECORD
  length : CARDINAL;
  array  : ARRAY [0..MaxLineLength] OF CHAR
END; (* LexemeBuffer *)


(* ---------------------------------------------------------------------------
 * hidden declaration of opaque type
 * ------------------------------------------------------------------------ *)

TYPE Infile = POINTER TO InfileDescriptor;

TYPE InfileDescriptor = RECORD
  file : BasicFileIO.File;
  line,
  column : CARDINAL;
  digestMode : DigestMode;
  digestValue : Hash.Key;
  lexbuf : LexemeBuffer
END; (* InfileDescriptor *)


(* ---------------------------------------------------------------------------
 * procedure Open(infile, path, status )
 * ---------------------------------------------------------------------------
 * Opens the file at path and passes a newly allocated and initialised infile
 * object back in out-parameter infile. Passes Infile.Nil on failure.
 * ------------------------------------------------------------------------ *)

PROCEDURE Open
 ( VAR (* NEW *) infile : Infile;
   VAR (* CONST *) path : ARRAY OF CHAR;
   VAR           status : BasicFileIO.Status );

VAR
  file : BasicFileIO.File;
  s : BasicFileIO.Status;
  
BEGIN
  BasicFileIO.Open(file, path, BasicFileIO.Read, s);
  
  IF s # BasicFileIO.Success THEN
    status := s;
    RETURN
  END; (* IF *)
  
  ALLOCATE(infile, TSIZE(InfileDescriptor));
  
  infile^.file := file;
  infile^.line := 1;
  infile^.column := 1;
  infile^.digestMode := NoDigest;
  infile^.digestValue := Hash.InitialValue;
  Clear(infile^.lexbuf)
END Open;


(* ---------------------------------------------------------------------------
 * procedure Close(infile)
 * ---------------------------------------------------------------------------
 * Closes the file associated with infile and passes Infile.Nil in infile.
 * ------------------------------------------------------------------------ *)

PROCEDURE Close ( VAR infile : Infile );

VAR
  s : BasicFileIO.Status;
  
BEGIN
  IF infile = NIL THEN
    RETURN
  END; (* IF *)
  
  BasicFileIO.Close(infile^.file, s);
  
  IF s = BasicFileIO.Success THEN
    infile := Infile.Nil
  END (* IF *)
END Close;


(* ---------------------------------------------------------------------------
 * function consumeChar(infile)
 * ---------------------------------------------------------------------------
 * Consumes the current lookahead character in infile, updates infile's digest
 * value depending on its current digest mode setting,  and returns the
 * resulting new lookahead character without consuming it.
 * ------------------------------------------------------------------------ *)

PROCEDURE consumeChar ( infile : Infile ) : CHAR;

VAR
  ch : CHAR;
  
BEGIN
  BasicFileIO.ReadChar(infile^.file, ch);
  
  (* printables *)
  IF (ch > ISO646.SP) AND (ch # ISO646.DEL) THEN
    (* append to lexeme buffer *)
    AppendChar(infile^.lexbuf, ch);
    
    (* update column counter *)
    infile^.column := infile^.column + 1;
    
    (* update digest *)
    IF infile^.digestMode # NoDigest THEN
      infile^.digestValue := Hash.valueForNextChar(infile^.digestValue, ch)
    END (* IF *)
    
  (* whitespace *)
  ELSIF (ch = ISO646.SP) OR (ch = ISO646.TAB) THEN
    (* append to lexeme buffer *)
    AppendChar(infile^.lexbuf, ch);
    
    (* update column counter *)
    infile^.column := infile^.column + 1;
    
    (* update digest *)
    IF infile^.digestMode = Quotables THEN
      infile^.digestValue := Hash.valueForNextChar(infile^.digestValue, ch)
    END (* IF *)
    
  (* proper Linefeed *)
  ELSIF ch = LF THEN
    (* newline terminates symbols *)
    Clear(infile^.lexbuf);
    
    (* update line and column counters *)
    infile^.column := 1;
    infile^.line := infile^.line + 1
    
  (* improper Linefeed *)
  ELSIF ch = CR THEN
    (* newline terminates symbols *)
    Clear(infile^.lexbuf);
    
    (* update line and column counters *)
    infile^.column := 1;
    infile^.line := infile^.line + 1;
    
    (* get next character *)
    BasicFileIO.ReadChar(infile^.file, ch);
    
    (* any LF following a CR is ignored *)
    IF ch # LF THEN
      BasicFileIO.InsertChar(infile^.file, ch)
    END (* IF *)
      
  ELSE (* non-printables *)
    (* update column counter *)
    infile^.column := infile^.column + 1
  END; (* IF *)
  
  RETURN lookaheadChar(infile)
END consumeChar;


(* ---------------------------------------------------------------------------
 * function lookaheadChar(infile)
 * ---------------------------------------------------------------------------
 * Returns the current lookahead char in infile without consuming any char.
 * ------------------------------------------------------------------------ *)

PROCEDURE lookaheadChar ( infile : Infile ) : CHAR;

VAR
  ch : CHAR;
  
BEGIN
  BasicFileIO.ReadChar(infile^.file, ch);
  BasicFileIO.InsertChar(infile^.file, ch);
  
  (* CR is always interpreted as LF *)
  IF ch = CR THEN
    RETURN LF
  ELSE
    RETURN ch
  END (* IF *)
END lookaheadChar;


(* ---------------------------------------------------------------------------
 * function la2Char(infile)
 * ---------------------------------------------------------------------------
 * Returns the 2nd lookahead char in infile without consuming any character.
 * ------------------------------------------------------------------------ *)

PROCEDURE la2Char ( infile : Infile ) : CHAR;

VAR
  la1, la2 : CHAR;
  
BEGIN
  (* read the next two characters *)
  BasicFileIO.ReadChar(infile^.file, la1);
  BasicFileIO.ReadChar(infile^.file, la2);
  
  (* read one further if CR LF found *)
  IF (la1 = CR) AND (la2 = LF) THEN
    la1 := LF;
    BasicFileIO.ReadChar(infile^.file, la2)
  END; (* IF *)
  
  (* put both characters back *)
  BasicFileIO.InsertChar(infile^.file, la1);
  BasicFileIO.InsertChar(infile^.file, la2);
  
  (* CR is always interpreted as LF *)
  IF la2 = CR THEN
    RETURN LF
  ELSE
    RETURN la2
  END (* IF *)
END la2Char;


(* ---------------------------------------------------------------------------
 * function status()
 * ---------------------------------------------------------------------------
 * Returns status of last operation.
 * ------------------------------------------------------------------------ *)

PROCEDURE status ( infile : Infile ) : BasicFileIO.Status;

BEGIN
  RETURN BasicFileIO.status(infile^.file)
END status;


(* ---------------------------------------------------------------------------
 * function eof()
 * ---------------------------------------------------------------------------
 * Returns TRUE if infile has reached the end of the file, else FALSE.
 * ------------------------------------------------------------------------ *)

PROCEDURE eof ( infile : Infile ) : BOOLEAN;

BEGIN
  RETURN BasicFileIO.eof(infile^.file)
END eof;


(* ---------------------------------------------------------------------------
 * function line(infile)
 * ---------------------------------------------------------------------------
 * Returns the line number of the current reading position of infile.
 * ------------------------------------------------------------------------ *)

PROCEDURE line ( infile : Infile ) : CARDINAL;

BEGIN
  RETURN infile^.line
END line;


(* ---------------------------------------------------------------------------
 * function column(infile)
 * ---------------------------------------------------------------------------
 * Returns the column number of the current reading position of infile.
 * ------------------------------------------------------------------------ *)

PROCEDURE column ( infile : Infile ) : CARDINAL;

BEGIN
  RETURN infile^.column
END column;


(* ---------------------------------------------------------------------------
 * procedure SetDigestMode(infile, mode)
 * ---------------------------------------------------------------------------
 * Changes the current digest mode of infile.
 * ------------------------------------------------------------------------ *)

PROCEDURE SetDigestMode ( infile : Infile; mode : DigestMode );

BEGIN
  infile^.digestMode := mode
END SetDigestMode;


(* ---------------------------------------------------------------------------
 * function digest(infile)
 * ---------------------------------------------------------------------------
 * Returns the incrementally computed digest of infile.
 * ------------------------------------------------------------------------ *)

PROCEDURE digest ( infile : Infile ) : Hash.Key;

BEGIN
  RETURN infile^.digestValue
END digest;


(* ---------------------------------------------------------------------------
 * procedure MarkLexeme(infile)
 * ---------------------------------------------------------------------------
 * Marks the current lookahead character as the start of a lexeme.
 * ------------------------------------------------------------------------ *)

PROCEDURE MarkLexeme( infile : Infile );

BEGIN
  Clear(infile^.lexbuf)
END MarkLexeme;


(* ---------------------------------------------------------------------------
 * function lexeme(infile ch)
 * ---------------------------------------------------------------------------
 * Returns the current lexeme.  Returns NIL if no lexeme has been marked, or
 * if no characters have been consumed since MarkLexeme() has been called.
 * ------------------------------------------------------------------------ *)

PROCEDURE lexeme ( infile : Infile ) : StringT;

CONST Separator = ISO646.US; (* Unit Separator *)

BEGIN
  (* for Printables mode inject separator into digest computation *)
  IF infile^.digestMode = Printables THEN
    infile^.digestValue :=
      Hash.valueForNextChar(infile^.digestValue, Separator)
  END; (* IF *)
  
  (* intern lexeme and return its handle *)
  RETURN stringForLexeme(infile^.lexbuf)
END lexeme;


(* ************************************************************************ *
 * Private Operations                                                       *
 * ************************************************************************ *)

(* ---------------------------------------------------------------------------
 * private procedure Clear(lexbuf)
 * ---------------------------------------------------------------------------
 * Resets and clears lexeme buffer lexbuf.
 * ------------------------------------------------------------------------ *)

PROCEDURE Clear ( VAR lexbuf : LexemeBuffer );

BEGIN
  lexbuf.length := 0;
  lexbuf.array[0] := NUL
END Clear;


(* ---------------------------------------------------------------------------
 * private procedure AppendChar(lexbuf, ch)
 * ---------------------------------------------------------------------------
 * Appends ch to lexeme buffer lexbuf. Excess characters are ignored.
 * ------------------------------------------------------------------------ *)

PROCEDURE AppendChar ( VAR lexbuf : LexemeBuffer; ch : CHAR );

BEGIN
  (* ignore any characters in excess of maximum line length *)
  IF lexbuf.length >= MaxLineLength THEN
    RETURN
  END; (* IF *)
  
  (* append ch to lexeme buffer *)
  lexbuf.array[lexbuf.length] := ch;
  lexbuf.length := lexbuf.length + 1;
  lexbuf.array[lexbuf.length] := NUL
END AppendChar;


(* ---------------------------------------------------------------------------
 * private function stringForLexeme(lexbuf)
 * ---------------------------------------------------------------------------
 * Returns interned string for contents of lexeme buffer, or NIL if empty.
 * ------------------------------------------------------------------------ *)

PROCEDURE stringForLexeme ( VAR lexbuf : LexemeBuffer ) : StringT;

VAR
  string : StringT;
  
BEGIN
  IF (lexbuf.length = 0) OR (lexbuf.array[0] = NUL) THEN
    RETURN String.Nil
  END; (* IF *)
  
  (* obtain interned string for array in lexbuf and return it *)
  string := String.forArray(lexbuf.array);
  Clear(lexbuf);
  RETURN string
END stringForLexeme;


END Infile.