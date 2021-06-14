(*!m2pim*) (* Copyright (c) 2017 Modula-2 Software Foundation *)

MODULE M2BSK;

(* Compiler Driver *)

IMPORT
  Args, ArgParser, BuildInfo, BuildParams,
  FNStr, Infile, Outfile, Compiler;
 
FROM BasicFileSys IMPORT fileExists, RenameFile;
FROM Infile IMPORT InfileT; (* alias for Infile.Infile *)
FROM Outfile IMPORT OutfileT; (* alias for Outfile.Outfile *)


CONST
  ProgTitle = "M2BSK - Modula-2 Compiler";
  Version   = "Version 0.1.0\n";
  Copyright = "Copyright (c) 2017 Modula-2 Software Foundation\n";
  License   = "Licensed under the LGPL license version 2.1\n";


PROCEDURE PrintBanner;

BEGIN
  Console.WriteChars(ProgTitle); Console.WriteChars(", ");
  Console.WriteChars(Version);
  Console.WriteChars(Copyright)
END PrintBanner;


PROCEDURE PrintUsage; (* needs further adaptation *)

BEGIN
  Console.WriteChars("Usage:\n");
  Console.WriteChars("$ m2bsk infoRequest\n"); Console.WriteChars("or\n");
  Console.WriteChars("$ m2bsk sourceFile option* diagnostic*\n\n");
  
  Console.WriteChars("infoRequest:\n");
  Console.WriteChars(" --help, -h           : print help\n");
  Console.WriteChars(" --version, -V        : print version\n");
  Console.WriteChars(" --license            : print license info\n");
  Console.WriteChars(" --build-info         : print build configuration\n\n");
  
  Console.WriteChars("option:\n");  
  Console.WriteChars(" --outfile targetFile : define outfile\n");
  Console.WriteChars(" --tabwidth number    : set tab width\n");
  Console.WriteChars(" --newline mode       : set newline mode\n\n");
  
  Console.WriteChars("diagnostic:\n");
  Console.WriteChars(" --verbose, -v        : verbose output\n");
  Console.WriteChars(" --show-settings      : print all settings\n\n");
  
  Console.WriteChars
    (" identifier | number | singleQuotedString | doubleQuotedString\n\n");
  
  Console.WriteChars("mode:\n");
  Console.WriteChars(" cr | lf | crlf\n\n")
END PrintUsage;


PROCEDURE PrintBuildInfo;

BEGIN
  Console.WriteChars("Built on    : ");
  Console.WriteChars(BuildInfo.Platform);
  Console.WriteChars("\nDialect     : ");
  Console.WriteChars(BuildInfo.Dialect);
  Console.WriteChars("\nCompiler    : ");
  Console.WriteChars(BuildInfo.Compiler);
  Console.WriteChars("\nI/O library : ");
  Console.WriteChars(BuildInfo.IOLibrary);
  Console.WriteChars("\nMemory Model: ");
  Console.WriteChars(BuildInfo.MemModel);
  Console.WriteLn
END PrintBuildInfo;


PROCEDURE PreflightCheck
  ( VAR infile : InfileT; VAR outfile : OutfileT; VAR passed : BOOLEAN );

VAR
  len : CARDINAL;
  pathStr : StringT;
  status : BasicFileIO.Status;
  path : ARRAY [0..BuildParams.MaxPathLen] OF CHAR;

BEGIN
  pathStr := Settings.infile();
  String.CopyToArray(pathStr, path, len);
  
  IF len = 0 THEN
    Console.WriteChars("source path too long.\n");
    passed := FALSE;
    RETURN
  END; (* IF *)
  
  (* bail out if infile does not exist *)
  IF NOT fileExists(path) THEN
    Console.WriteChars("sourcefile not found.\n");
    passed := FALSE;
    RETURN
  END; (* IF *)
    
  Infile.Open(infile, status);
  
  IF status # Success THEN
    Console.WriteChars("unable to open sourcefile.\n");
    infile := Infile.Nil;
    passed := FALSE;
    RETURN
  END; (* IF *)
  
  IF NOT Settings.alreadySet(Settings.Outfile) THEN
    (* derive target name from source name *)
    pathStr := FNStr.targetName(pathStr)
  ELSE
    pathStr := Settings.outfile()
  END; (* IF *)
  
  String.CopyToArray(pathStr, path, len);
  
  IF len = 0 THEN
    Console.WriteChars("target path too long.\n");
    passed := FALSE;
    RETURN
  END; (* IF *)
  
  IF fileExists(path) THEN
    (* rename existing file *)
  END; (* IF *)
  
  Outfile.Open(outfile, status);
  
  IF status # Success THEN
    Console.WriteChars("unable to create targetfile.\n");
    Infile.Close(infile);
    infile := Infile.Nil;
    outfile := Outfile.Nil;
    passed := FALSE;
    RETURN
  END; (* IF *)
  
  (* all preflight checks passed *)
  passed := TRUE;
END PreflightCheck;


VAR
  passed : BOOLEAN;
  infile : InfileT;
  outfile : OutfileT;
  argStatus : ArgParser.Status;
  fsStatus : BasicFileSys.Status;
  

BEGIN (* M2BSK *)
  (* check if program argument file is present *)
  IF fileExists(Args.Filename) THEN
    Args.Open
  ELSE (* query user and write file *)
    Args.Query
  END; (* IF *)
  
  argStatus := ArgParser.parseArgs();
  Args.Close;
  Args.Delete;
  
  CASE argStatus OF
    Success :
      PrintBanner;
      
      PreflightCheck(infile, outfile, passed);
      
      IF passed THEN
        Compiler.Compile(infile, outfile);
        Infile.Close(infile);
        Outfile.Close(outfile)
      ELSE
        (* unable to proceed *)
      END (* IF *)
            
  | HelpRequested :
      PrintUsage
      
  | VersionRequested :
      Console.WriteChars(Version)
  
  | LicenseRequested :
      Console.WriteChars(Copyright);
      Console.WriteChars(License)
      
  | BuildInfoRequested :
      PrintBuildInfo
      
  | ErrorsEncountered :
      (* TO DO *)
  END (* CASE *)
END M2BSK.
