(*!m2pim*) (* Copyright (c) 2016 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE CompilerOptions;

IMPORT String, Console;

FROM String IMPORT StringT; (* alias for String.String *)


(* Properties *)

VAR
  options : SET OF Option;
  optionStr : ARRAY Option OF StringT;


(* Operations *)

(* ---------------------------------------------------------------------------
 * procedure SetOption(option, value)
 * ---------------------------------------------------------------------------
 * Sets the given option to the given boolean FALSE.
 * ------------------------------------------------------------------------ *)

PROCEDURE SetOption ( option : Option; value : BOOLEAN );

BEGIN
  IF value = TRUE THEN
    INCL(options, value)
  ELSE (* value = FALSE *)
    EXCL(options, value)
  END
END SetOption;


(* ---------------------------------------------------------------------------
 * function verbose()
 * ---------------------------------------------------------------------------
 * Returns TRUE if option --verbose is turned on, else FALSE.
 * ------------------------------------------------------------------------ *)

PROCEDURE verbose () : BOOLEAN;

BEGIN
  RETURN (Verbose IN options)
END verbose;


(* ---------------------------------------------------------------------------
 * function lexerDebug()
 * ---------------------------------------------------------------------------
 * Returns TRUE if option --lexer-debug is turned on, else FALSE.
 * ------------------------------------------------------------------------ *)

PROCEDURE lexerDebug () : BOOLEAN;

BEGIN
  RETURN (LexerDebug IN options)
END lexerDebug;


(* ---------------------------------------------------------------------------
 * function parserDebug()
 * ---------------------------------------------------------------------------
 * Returns TRUE if option --parser-debug is turned on, else FALSE.
 * ------------------------------------------------------------------------ *)

PROCEDURE parserDebug () : BOOLEAN;

BEGIN
  RETURN (ParserDebug IN options)
END parserDebug;


(* ---------------------------------------------------------------------------
 * function showSettings()
 * ---------------------------------------------------------------------------
 * Returns TRUE if option --show-settings is turned on, else FALSE.
 * ------------------------------------------------------------------------ *)

PROCEDURE showSettings () : BOOLEAN;

BEGIN
  RETURN (ShowSettings IN options)
END showSettings;


(* ---------------------------------------------------------------------------
 * function errantSemicolons()
 * ---------------------------------------------------------------------------
 * Returns TRUE if option --errant-semicolons is turned on, else FALSE.
 * ------------------------------------------------------------------------ *)

PROCEDURE errantSemicolons () : BOOLEAN;

BEGIN
  RETURN (ErrantSemicolons IN options)
END errantSemicolons;


(* ---------------------------------------------------------------------------
 * function astRequired()
 * ---------------------------------------------------------------------------
 * Returns TRUE if option --ast is turned on, else FALSE.
 * ------------------------------------------------------------------------ *)

PROCEDURE astRequired () : BOOLEAN;

BEGIN
  RETURN (AstRequired IN options)
END astRequired;


(* ---------------------------------------------------------------------------
 * function graphRequired()
 * ---------------------------------------------------------------------------
 * Returns TRUE if option --graph is turned on, else FALSE.
 * ------------------------------------------------------------------------ *)

PROCEDURE graphRequired () : BOOLEAN;

BEGIN
  RETURN (GraphRequired IN options)
END graphRequired;


(* ---------------------------------------------------------------------------
 * function xlatRequired()
 * ---------------------------------------------------------------------------
 * Returns TRUE if option --xlat is turned on, else FALSE.
 * ------------------------------------------------------------------------ *)

PROCEDURE xlatRequired () : BOOLEAN;

BEGIN
  RETURN (XlatRequired IN options)
END xlatRequired;


(* ---------------------------------------------------------------------------
 * function objRequired()
 * ---------------------------------------------------------------------------
 * Returns TRUE if option --obj is turned on, else FALSE.
 * ------------------------------------------------------------------------ *)

PROCEDURE objRequired () : BOOLEAN;

BEGIN
  RETURN (ObjRequired IN options)
END objRequired;


(* ---------------------------------------------------------------------------
 * function preserveComments()
 * ---------------------------------------------------------------------------
 * Returns TRUE if option --preserve-comments is turned on, else FALSE.
 * ------------------------------------------------------------------------ *)

PROCEDURE preserveComments () : BOOLEAN;

BEGIN
  RETURN (PreserveComments IN options)
END preserveComments;


(* ---------------------------------------------------------------------------
 * function lowlineIdentifiers()
 * ---------------------------------------------------------------------------
 * Returns TRUE if option --lowline-identifiers is turned on, else FALSE.
 * ------------------------------------------------------------------------ *)

PROCEDURE lowlineIdentifiers () : BOOLEAN;

BEGIN
  RETURN (LowlineIdentifiers IN options)
END lowlineIdentifiers;


(* ---------------------------------------------------------------------------
 * function dollarIdentifiers()
 * ---------------------------------------------------------------------------
 * Returns TRUE if option --dollar-identifiers is turned on, else FALSE.
 * ------------------------------------------------------------------------ *)

PROCEDURE dollarIdentifiers () : BOOLEAN;

BEGIN
  RETURN (DollarIdentifiers IN options)
END dollarIdentifiers;


(* ---------------------------------------------------------------------------
 * procedure PrintSettings()
 * ---------------------------------------------------------------------------
 * Prints the current settings to the console.
 * ------------------------------------------------------------------------ *)

PROCEDURE PrintSettings;

BEGIN
  FOR option := MIN(Option) TO MAX(Option) DO
    Console.WriteStr(optionStr[option]);
    Console.WriteChars(": ");
    IF option = TRUE THEN
      Console.WriteChars("on")
    ELSE (* option = FALSE *)
      Console.WriteChars("off")
    END; (* IF *)
    Console.WriteLn
  END (* FOR *)
END PrintSettings;


BEGIN (* CompilerOptions *)
  (* init option set *)
  option := { };
  
  (* init option name strings *)
  optionStr[Verbose] := String.NewStr("Verbose");
  optionStr[LexerDebug] := String.NewStr("LexerDebug");
  optionStr[ParserDebug] := String.NewStr("ParserDebug");
  optionStr[ShowSettings] := String.NewStr("ShowSettings");
  optionStr[ErrantSemicolons] := String.NewStr("ErrantSemicolons");
  optionStr[AstRequired] := String.NewStr("AstRequired");
  optionStr[GraphRequired] := String.NewStr("GraphRequired");
  optionStr[XlatRequired] := String.NewStr("XlatRequired");
  optionStr[ObjRequired] := String.NewStr("ObjRequired");
  optionStr[PreserveComments] := String.NewStr("PreserveComments");
  optionStr[DollarIdentifiers] := String.NewStr("DollarIdentifiers");
  optionStr[LowlineIdentifiers] := String.NewStr("LowlineIdentifiers");
END CompilerOptions.