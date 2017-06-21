(*!m2pim*) (* Copyright (c) 2016 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE ArgParser;

IMPORT Console, CompilerOptions, ArgLexer;

FROM String IMPORT StringT; (* alias for String.String *)


(* Properties *)

VAR
  srcFile : StringT;
  errCount : CARDINAL;
  optionSet : SET OF CompilerOptions.Option;


(* Public Operations *)

(* ---------------------------------------------------------------------------
 * function parseArgs()
 * ---------------------------------------------------------------------------
 * Parses command line arguments and sets compiler options accordingly.
 *
 * args :
 *   infoRequest | compilationRequest
 *   ;
 * ------------------------------------------------------------------------ *)

PROCEDURE parseArgs : Status;

BEGIN
  sym := ArgLexer.nextToken();
  
  IF ArgLexer.isInfoRequest(sym) THEN
    sym := parseInfoRequest(sym)
    
  ELSIF ArgLexer.isCompilationRequest(sym) THEN
    sym := parseCompilationRequest(sym)
    
  ELSIF sym = ArgLexer.EndOfInput THEN
    ReportMissingSourceFile
  END; (* IF *)
  
  WHILE sym # ArgLexer.EndOfInput DO
    ReportExcessArgument(ArgLexer.lastArg());
    sym := ArgLexer.nextToken()
  END; (* WHILE *)
  
  IF errCount > 0 THEN
    status = ErrorsEncountered
  END; (* IF *)
  
  RETURN status
END parseArgs;


(* ---------------------------------------------------------------------------
 * function sourceFile()
 * ---------------------------------------------------------------------------
 * Returns a string with the source file argument.
 * ------------------------------------------------------------------------ *)

PROCEDURE sourceFile : StringT;

BEGIN
  RETURN srcFile
END sourceFile;


(* ---------------------------------------------------------------------------
 * function errorCount()
 * ---------------------------------------------------------------------------
 * Returns the count of errors encountered while parsing the arguments.
 * ------------------------------------------------------------------------ *)

PROCEDURE errorCount : CARDINAL;

BEGIN
  RETURN errCount
END errorCount;


(* Private Operations *)

(* ---------------------------------------------------------------------------
 * function parseInfoRequest(token)
 * ---------------------------------------------------------------------------
 * infoRequest :
 *   --help | -h | --version | -V | --license
 *   ;
 * ------------------------------------------------------------------------ *)

PROCEDURE parseInfoRequest ( token : ArgLexer.Token ) : ArgLexer.Token;

BEGIN
  CASE token OF
  (* --help, -h *)
    ArgLexer.Help : status := Status.HelpRequested
  
  (* --version, -V *)  
  | ArgLexer.Version : status := Status.VersionRequested
  
  (* --license *)
  | ArgLexer.License : status := Status.LicenseRequested
  
  END; (* CASE *)
  
  RETURN ArgLexer.nextToken()
END parseInfoRequest;


(* ---------------------------------------------------------------------------
 * function parseCompilationRequest(token)
 * ---------------------------------------------------------------------------
 * compilationRequest :
 *   products? capabilities? sourceFile diagnostics?
 *   ;
 * ------------------------------------------------------------------------ *)

PROCEDURE parseCompilationRequest ( token : ArgLexer.Token ) : ArgLexer.Token;

BEGIN
  (* products? *)
  IF ArgLexer.isProductOption(token) THEN
    token := parseProducts(token)
  END; (* IF *)
  
  (* capabilities? *)
  IF ArgLexer.isCapabilityOption(token) THEN
    token := parseCapabilities(token)
  END; (* IF *)
  
  (* sourceFile *)
  IF token = ArgLexer.SourceFile THEN
    token := parseCapabilities(token)
  ELSE
    ReportMissingSourceFile()
  END; (* IF *)
  
  (* diagnostics? *)
  IF ArgLexer.isDiagnosticOption(token) THEN
    token := parseDiagnostics(token)
  END; (* IF *)
  
  RETURN token
END parseCompilationRequest;


(* ---------------------------------------------------------------------------
 * function parseProducts(token)
 * ---------------------------------------------------------------------------
 * products :
 *   ( singleProduct | multipleProducts ) commentOption?
 *   ;
 * ------------------------------------------------------------------------ *)

PROCEDURE parseProducts ( token : ArgLexer.Token ) : ArgLexer.Token;

BEGIN
  (* ( singleProduct | multipleProducts ) *)
  IF ArgLexer.isSingleProductOption(token) THEN
    token := parseSingleProduct(token)
  ELSE
    token := parseMultipleProducts(token)
  END; (* IF *)
  
  (* commentOption? *)
  IF ArgLexer.isCommentOption(token) THEN
    IF CompilerOptions.xlatRequired() THEN
      token := parseCommentOption(token)
    ELSE
      ReportMissingDependencyFor(ArgLexer.lastArg(), "--xlat");
      token := ArgLexer.nextToken()
    END (* IF *)
  END; (* IF *)
  
  RETURN token
END parseProducts;


(* ---------------------------------------------------------------------------
 * function parseSingleProduct(token)
 * ---------------------------------------------------------------------------
 * singleProduct :
 *   --syntax-only | --ast-only | --graph-only | --xlat-only | --obj-only
 *   ;
 * ------------------------------------------------------------------------ *)

PROCEDURE parseSingleProduct ( token : ArgLexer.Token ) : ArgLexer.Token;

BEGIN
  CASE token OF
  (* --syntax-only | *)
    ArgLexer.SyntaxOnly :
      SetOption(Option.AstRequired, FALSE);
      SetOption(Option.GraphRequired, FALSE);
      SetOption(Option.XlatRequired, FALSE);
      SetOption(Option.ObjRequired, FALSE)
  
  (* --ast-only | *)
  | ArgLexer.AstOnly :
      SetOption(Option.AstRequired, TRUE);
      SetOption(Option.GraphRequired, FALSE);
      SetOption(Option.XlatRequired, FALSE);
      SetOption(Option.ObjRequired, FALSE)
  
  (* --graph-only | *)
  | ArgLexer.GraphOnly :
      SetOption(Option.AstRequired, FALSE);
      SetOption(Option.GraphRequired, TRUE);
      SetOption(Option.XlatRequired, FALSE);
      SetOption(Option.ObjRequired, FALSE)
  
  (* --xlat-only | *)
  | ArgLexer.XlatOnly :
      SetOption(Option.AstRequired, FALSE);
      SetOption(Option.GraphRequired, FALSE);
      SetOption(Option.XlatRequired, TRUE);
      SetOption(Option.ObjRequired, FALSE)
  
  (* --obj-only *)
  | ArgLexer.ObjOnly :
      SetOption(Option.AstRequired, FALSE);
      SetOption(Option.GraphRequired, FALSE);
      SetOption(Option.XlatRequired, FALSE);
      SetOption(Option.ObjRequired, TRUE)
  END; (* CASE *)
  
  RETURN ArgLexer.nextToken()
END parseSingleProduct;


(* ---------------------------------------------------------------------------
 * function parseMultipleProducts(token)
 * ---------------------------------------------------------------------------
 * multipleProducts :
 *   ( ast | graph | xlat | obj )+
 *   ;
 * ------------------------------------------------------------------------ *)

PROCEDURE parseMultipleProducts ( token : ArgLexer.Token ) : ArgLexer.Token;

BEGIN
  WHILE ArgLexer.isMultipleProductsOption(token) DO
    CASE token OF
    (* --ast | *)
    ArgLexer.Ast :
      SetOption(CompilerOptions.AstRequired, TRUE)
      
    (* --no-ast | *)
    ArgLexer.NoAst :
      SetOption(CompilerOptions.AstRequired, FALSE)
    
    (* --graph | *)
    ArgLexer.Graph :
      SetOption(CompilerOptions.GraphRequired, TRUE)
      
    (* --no-graph | *)
    ArgLexer.NoGraph :
      SetOption(CompilerOptions.GraphRequired, FALSE)
    
    (* --xlat | *)
    ArgLexer.Xlat :
      SetOption(CompilerOptions.XlatRequired, TRUE)
      
    (* --no-xlat | *)
    ArgLexer.NoXlat :
      SetOption(CompilerOptions.XlatRequired, FALSE)
    
    (* --obj | *)
    ArgLexer.Obj :
      SetOption(CompilerOptions.ObjRequired, TRUE)
      
    (* --no-obj | *)
    ArgLexer.NoObj :
      SetOption(CompilerOptions.ObjRequired, FALSE)
    END; (* CASE *)
    
    token := ArgLexer.nextToken()
  END; (* WHILE *)
  
  RETURN token
END parseMultipleProducts;


(* ---------------------------------------------------------------------------
 * function parseCommentOption(token)
 * ---------------------------------------------------------------------------
 * commentOption :
 *   --preserve-comments | --strip-comments
 *   ;
 * ------------------------------------------------------------------------ *)

PROCEDURE parseCommentOption ( token : ArgLexer.Token ) : ArgLexer.Token;

BEGIN
  (* --preserve-comments | *)
  IF token = ArgLexer.PreserveComments THEN
    SetOption(CompilerOptions.PreserveComments, TRUE)
    
  (* --strip-comments *)
  ELSE
    SetOption(CompilerOptions.PreserveComments, FALSE)
  END; (* IF *)
  
  RETURN ArgLexer.nextToken()
END parseCommentOption;


(* ---------------------------------------------------------------------------
 * function parseCapabilities(token)
 * ---------------------------------------------------------------------------
 * capabilities :
 *   ( dollarIdentifiers | lowlineIdentifiers )+
 *   ;
 * ------------------------------------------------------------------------ *)

PROCEDURE parseCapabilities ( token : ArgLexer.Token ) : ArgLexer.Token;

BEGIN
  (* ( dollarIdentifiers | lowlineIdentifiers )+ *)
  WHILE ArgLexer.isCapabilityOption(token) DO
    CASE token OF
    (* --dollar-identifiers *)
      ArgLexer.DollarIdentifiers :
        SetOption(CompilerOptions.DollarIdentifiers, TRUE)
        
    (* --no-dollar-identifiers *)
    | ArgLexer.NoDollarIdentifiers :
        SetOption(CompilerOptions.DollarIdentifiers, FALSE)
    
    (* --lowline-identifiers *)
    | ArgLexer.LowlineIdentifiers :
        SetOption(CompilerOptions.LowlineIdentifiers, TRUE)
    
    (* --no-lowline-identifiers *)
    | ArgLexer.NoLowlineIdentifiers :
        SetOption(CompilerOptions.LowlineIdentifiers, FALSE)
    END; (* CASE *)
    
    token := ArgLexer.nextToken()
  END; (* WHILE *)
  
  RETURN ArgLexer.nextToken()
END parseCapabilities;


(* ---------------------------------------------------------------------------
 * function parseSourceFile(token)
 * ---------------------------------------------------------------------------
 * sourceFile :
 *   <platform dependent path/filename>
 *   ;
 * ------------------------------------------------------------------------ *)

PROCEDURE parseSourceFile ( token : ArgLexer.Token ) : ArgLexer.Token;

BEGIN
  srcFile := ArgLexer.LastArg();
  RETURN ArgLexer.nextToken()
END parseSourceFile;


(* ---------------------------------------------------------------------------
 * function parseDiagnostics(token)
 * ---------------------------------------------------------------------------
 * diagnostics :
 *   ( --verbose | -v | --lexer-debug | --parser-debug | --print-settings |
 *     --errant-semicolons )+
 *   ;
 * ------------------------------------------------------------------------ *)

PROCEDURE parseDiagnostics ( token : ArgLexer.Token ) : ArgLexer.Token;

BEGIN
  WHILE ArgLexer.isDiagnosticsOption(token) DO
    CASE token OF
    (* --verbose | -v | *)
      ArgLexer.Verbose :
        SetOption(CompilerOptions.Verbose, TRUE)
    
    (* --lexer-debug | *)
    | ArgLexer.LexerDebug :
        SetOption(CompilerOptions.LexerDebug, TRUE)
    
    (* --parser-debug | *)
    | ArgLexer.ParserDebug :
        SetOption(CompilerOptions.ParserDebug, TRUE)
    
    (* --print-settings | *)
    | ArgLexer.PrintSettings :
        SetOption(CompilerOptions.PrintSettings, TRUE)
    
    (* --errant-semicolons | *)
    | ArgLexer.PrintSettings :
        SetOption(CompilerOptions.ErrantSemicolons, TRUE)
    END; (* CASE *)
    
    token := ArgLexer.nextToken()
  END; (* WHILE *)
  
  RETURN token
END parseDiagnostics;


(* ---------------------------------------------------------------------------
 * procedure SetOption(option)
 * ---------------------------------------------------------------------------
 * Sets option unless duplicate. Reports duplicate.
 * ------------------------------------------------------------------------ *)

PROCEDURE SetOption ( option : CompilerOptions.Option; value : BOOLEAN );

BEGIN
  IF (* duplicate *) option IN optionSet THEN
    ReportDuplicateOption(ArgLexer.lastArg())
  ELSE (* not a duplicate *)
    CompilerOptions.SetOption(option, value);
    (* remember this option *)
    INCL(optionSet, option)
  END (* IF *)
END SetOption;


(* ---------------------------------------------------------------------------
 * procedure ReportInvalidOption(arg)
 * ---------------------------------------------------------------------------
 * Reports arg as an invalid option to the console.
 * ------------------------------------------------------------------------ *)

PROCEDURE ReportInvalidOption ( arg : StringT );

BEGIN
  Console.WriteChars("invalid option "); Console.WriteStr(arg);
  Console.WriteLn;
  errCount := errCount + 1
END ReportInvalidOption;


(* ---------------------------------------------------------------------------
 * procedure ReportDuplicateOption(arg)
 * ---------------------------------------------------------------------------
 * Reports arg as a duplicate option to the console.
 * ------------------------------------------------------------------------ *)

PROCEDURE ReportDuplicateOption ( arg : StringT );

BEGIN
  Console.WriteChars("duplicate option "); Console.WriteStr(arg);
  Console.WriteLn;
  errCount := errCount + 1
END ReportDuplicateOption;


(* ---------------------------------------------------------------------------
 * procedure ReportExcessArgument(arg)
 * ---------------------------------------------------------------------------
 * Reports arg as an excess argument to the console.
 * ------------------------------------------------------------------------ *)

PROCEDURE ReportExcessArgument ( arg : StringT );

BEGIN
  Console.WriteChars("excess argument "); Console.WriteStr(arg);
  Console.WriteLn;
  errCount := errCount + 1
END ReportExcessArgument;


(* ---------------------------------------------------------------------------
 * procedure ReportMissingSourceFile
 * ---------------------------------------------------------------------------
 * Reports missing sourcefile argument to the console.
 * ------------------------------------------------------------------------ *)

PROCEDURE ReportMissingSourceFile;

BEGIN
  Console.WriteChars("missing sourcefile argument");
  Console.WriteLn;
  errCount := errCount + 1
END ReportMissingSourceFile;


(* ---------------------------------------------------------------------------
 * procedure MissingDependencyFor(arg1, arg2)
 * ---------------------------------------------------------------------------
 * Reports arg1 to have a missing dependency on arg2 to the console.
 * ------------------------------------------------------------------------ *)

PROCEDURE MissingDependencyFor ( arg1, arg2 : StringT );

BEGIN
  Console.WriteChars("option "); Console.WriteStr(arg1);
  Console.WriteChars(" only available with option ");
  Console.WriteStr(arg2);
  Console.WriteLn;
  errCount := errCount + 1
END MissingDependencyFor;


BEGIN (* ArgParser *)
  (* init properties *)
  srcFile := NIL;
  errCount := 0;
  optionSet := { }
END ArgParser.
