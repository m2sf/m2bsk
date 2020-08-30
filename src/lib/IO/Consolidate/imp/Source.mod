(*!m2pim*) (* Copyright (c) 2015 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE Source;

(* Source File Reader for Modula-2 R10 Bootstrap Kernel *)

IMPORT ASCII, SimpleFileIO, BuildParams, LexTab;


(* Source Type *)

TYPE Source = POINTER TO SourceDescriptor;


(* Source Descriptor *)

TYPE SourceDescriptor = RECORD
  index,                        (* lookahead index in source buffer *)
  endPos,                       (* end position index in source buffer *)
  lexPos : CARDINAL;            (* marked position index in source buffer *)
  buffer : SourceBuffer;        (* source buffer with entire source *)
  line : BuildParams.LineCounter;    (* lookahead position line counter *)
  column : BuildParams.ColumnCounter (* lookahead position column counter *)
END;


(* Source Buffer *)

TYPE SourceBuffer = ARRAY [0..BuildParams.MaxSourceFileSize + 1] OF CHAR;
(* always to be terminated by ASCII.NUL, therefore max index = size + 1 *)


(* ---------------------------------------------------------------------------
 *  Definitions
 *
 *  start position :
 *    the position of the first character in the source.
 *
 *  end position :
 *    the position of the last character in the source.
 *
 *  lookahead position :
 *    the position of the character to be consumed next.
 *
 *  second lookahead position :
 *    the position immediately following the lookahead position.
 *
 *  marked position :
 *    a position recorded as the start of a lexeme.
 *    it is end position + 1 if no marker has been set.
 *
 *  lookahead character :
 *    the character at the lookahead position,
 *    it is ASCII.NUL if its position > end position or if eof is set.
 *
 *  second lookahead character :
 *    the character at the second lookahead position,
 *    it is ASCII.NUL if its position > end position or if eof is set.
 *
 *  marked lexeme :
 *    a character sequence that starts at the marked position (inclusively)
 *    and ends at the lookahead position (exclusively).
 *
 *  character consumption :
 *    a character is consumed by advancing the lookahead position
 *    to the character's second lookahead position or by setting eof.
 *
 *  end-of-line marker:
 *    an ASCII.LF,
 *    or a sequence consisting of an ASCII.CR followed by an ASCII.LF,
 *    or a sole ASCII.CR that is not immediately followed by ASCII.LF.
 *
 *    The lookahead position of an end-of-line marker is the position
 *    following the last character of the end-of-line marker.
 *
 *  end-of-file flag:
 *    abbreviated as eof flag, is a boolean value that is set when
 *    the character at the end position has been consumed.
 *
 * ---------------------------------------------------------------------------
 *)


(* Operations *)

(* ---------------------------------------------------------------------------
 * procedure New ( source, filename, status )
 *  creates a new source instance, associated with filename
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE New
  ( VAR s : SourceT; filename : Filename; VAR status : Status );
(* Passes back a newly allocated source instance associated with name in s.
   The associated file is opened for reading and the lookahead position is
   set to the start position.  Passes back NIL in s if unsuccessful.
   The status of the operation is passed back in status. *)

VAR
  file : FileIO.File;
  source : Source;
  octetsRead : CARDINAL;
  
BEGIN
  
  (* source must be NIL *)
  IF s # NIL THEN
    status := Status.AllocTargetNotNil;
    RETURN
  END; (* IF *)
    
  IF FileSizeOf(filename) > MaxSourceFileSize THEN
    status := Status.SourceExceedsMaxFileSize;
    RETURN
  END; (* IF *)
  
  (* allocate source instance *)
  NEW(source);
  
  (* read source file contents into buffer *)
  SimpleFileIO.Open(file, filename);
  SimpleFileIO.ReadOctets(file, source^.buffer, octetsRead);
  SimpleFileIO.Close(file);
    
  (* TO DO : check for and handle file IO errors *)
  
  (* set start and end position *)
  source^.index := 0;
  source^.endPos := octetsRead - 1;
  
  (* clear lexeme marker by setting it beyond end position *)
  source^.lexPos := source^.endPos + 1;
  
  (* terminate buffer *)
  source^.buffer[source^.endPos+1] := ASCII.NUL;
  
  (* initialise line and column counters *)
  source^.line := 1; source^.column := 1;
  
  (* pass back status and source *)
  status := Status.Success;
  s := source

END New;


(* ---------------------------------------------------------------------------
 * procedure GetChar ( source, ch, next )
 *  consumes current lookahead character, passes back new lookahead character
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE GetChar ( s : SourceT; VAR ch, next : CHAR );
(* Passes back the lookahead character in ch and consumes it.
   Passes back the new lookahead character in next without consuming it. *)

BEGIN

  (* pass current lookahead character *)
  ch := s^.buffer[s^.index];
  
  (* pass LF instead of CR *)
  IF ch = ASCII.CR THEN
    ch := ASCII.LF
  END; (* IF *)
  
  (* consume current lookahead and pass new lookahead *)
  next := consumeChar(s);
  
  (* pass LF instead of CR *)
  IF next := ASCII.CR THEN
    next := ASCII.LF
  END (* IF *)
  
END GetChar;
  

(* ---------------------------------------------------------------------------
 * procedure consumeChar ( source )
 *  consumes current lookahead character, returns new lookahead character
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE consumeChar ( s : SourceT ) : CHAR;

VAR
  ch : CHAR;

BEGIN
  
  (* remember the lookahead character *)
  ch := s^.buffer[s^.index];
  
  (* ... and consume it *)
  IF s^.index <= s^.endPos THEN
    s^.index++
    
  END; (* IF *)
  
  (* check for new line *)
  IF (* new line *) (ch = ASCII.LF) OR (ch = ASCII.CR) THEN
    (* update line and column counters *)
    s^.line++; s^.column := 1;
    
    (* check for CR LF sequence *)
    IF (ch = ASCII.CR) AND (s^.buffer[s^.index] = ASCII.LF) THEN
      (* consume trailing LF *)
      s^.index++
      
    END (* IF *)
    
  ELSE (* no new line *)
    (* update column counter only *)
    s^.column++
    
  END (* IF *)
  
  (* return new lookahead *)
  RETURN ch
END consumeChar;


(* ---------------------------------------------------------------------------
 * procedure lookaheadChar ( source ) : CHAR
 *  returns current lookahead character
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE lookaheadChar ( s : SourceT ) : CHAR;
(* Returns the lookahead character of s.
   Does not consume any character and does not set eof. *)

VAR
  next : CHAR;
  
BEGIN

  (* get lookahead character *)
  next := s^.buffer[s^.index];
  
  (* return LF instead of CR *)
  IF next = ASCII.CR THEN
    next := ASCII.LF
  END; (* IF *)
    
  RETURN next
END lookaheadChar;


(* ---------------------------------------------------------------------------
 * procedure la2Char ( source ) : CHAR
 *  returns second lookahead character
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE la2Char ( s : SourceT ) : CHAR;
(* Returns the second lookahead character of s.
   Does not consume any character and does not set eof. *)
   
VAR
  next, la2 : CHAR;
  
BEGIN
  
  (* return ASCII.NUL if lookahead is last character or beyond eof *)
  IF s^.index >= s^.endPos THEN
    RETURN ASCII.NUL
  END; (* IF *)
  
  (* get lookahead and tentative second lookahead *)
  next := s^.buffer[s^.index];
  la2 := s^.buffer[s^.index+1];
  
  (* check if lookahead is CR LF sequence *)
  IF (next = ASCII.CR) AND (la2 = ASCII.LF) THEN
  
    (* return ASCII.NUL if CR LF is at the very end of source *)
    IF s^.index+1 >= s^.endPos THEN
      RETURN ASCII.NUL
    END; (* IF *)
    
    (* otherwise second lookahead is character after CR LF sequence  *)
    la2 := s^.buffer[s^.index+2]
  END (* IF *)
  
  (* return LF instead of CR *)
  IF la2 = ASCII.CR THEN
    la2 := ASCII.LF
  END; (* IF *)
  
  RETURN la2
END la2Char;


(* ---------------------------------------------------------------------------
 * procedure MarkLexeme ( source, line, col )
 *  marks current lookahead position as the start of a lexeme
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE MarkLexeme ( s : SourceT; VAR line, col : CARDINAL );
(* Marks the lookahead position in s as the start of the marked lexeme.
   Passes back lookahead position line and column counters in line and col. *)

BEGIN

  s^.lexPos := s^.index;
  line := s^.line;
  col := s^.column

END MarkLexeme;


(* ---------------------------------------------------------------------------
 * procedure CopyLexeme ( source, dict, handle )
 *  adds a marked lexeme to lexeme dictionary dict
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE CopyLexeme ( s : SourceT; dict : LexDict; VAR handle : DictHandle );
(* Adds the marked lexeme in s to lexeme dictionary dict, passes its access
   handle back in handle and clears the lexeme marker.  If no lexeme marker
   has been set, no content is copied and zero is passed back in handle. *)

VAR
  length : CARDINAL;
  
BEGIN
  
  (* return zero handle if no lexeme marker is set *)
  IF s^.lexPos >= s^.index THEN
    handle := 0;
    RETURN
  END;
  
  (* store marked lexeme in lexeme dictionary *)
  length := s^.index - s^.lexPos;
  store(dict, s^.buffer, s^.lexPos, length, handle);
  
  (* clear lexeme marker by setting it beyond end position *)
  s^.lexPos := s^.endPos + 1
  
END CopyLexeme;


(* ---------------------------------------------------------------------------
 * procedure GetLineAndColumn ( source, line, column )
 *  passes back line and column counters for current lookahead position
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE GetLineAndColumn ( s : SourceT; VAR line, col : CARDINAL );
(* Passes back the current line and column counters of s in line and col. *)

BEGIN

  line := s^.line;
  col := s^.column

END GetLineAndColumn;


(* ---------------------------------------------------------------------------
 * procedure eof ( source ) : BOOLEAN
 *  returns TRUE if last character in source has been consumed, else FALSE
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  TO DO
 *
 * post-conditions:
 *  TO DO
 *
 * error-conditions:
 *  TO DO
 * ---------------------------------------------------------------------------
 *)
PROCEDURE eof ( s : SourceT ) : BOOLEAN;

BEGIN
  (* eof is set if lookahead position is greater than end position *)
  RETURN s^.index > s^.endPos
END eof;


(* ---------------------------------------------------------------------------
 * procedure Release ( source )
 *  releases source instance
 * ---------------------------------------------------------------------------
 * pre-conditions:
 *  (1) source must not be NIL
 *
 * post-conditions:
 *  (1) lexer is deallocated
 *  (2) NIL is passed back in source
 *
 * error-conditions:
 *  (1) reference to source remains unmodified
 * ---------------------------------------------------------------------------
 *)
PROCEDURE Release ( VAR s : SourceT; VAR status : Status );

BEGIN

  IF s # NIL THEN
    RELEASE(s);
    status := Status.Success;
    s := NIL
  ELSE
    status := invalidReference
  END (* IF *)
  
END Release;


END Source.
