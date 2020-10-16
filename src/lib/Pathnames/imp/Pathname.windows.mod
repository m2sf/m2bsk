(*!m2pim*) (* Copyright (c) 2020 Modula-2 Software Foundation. *)

IMPLEMENTATION MODULE Pathname; (* Microsoft Windows version *)

(* Microsoft Windows Pathname Parser for Modula-2 R10 Bootstrap Kernel *)

IMPORT ISO646, Char, String, PathnamePolicy;

IMPORT StringT; (* alias for String.String *)

FROM Storage IMPORT DEALLOCATE;
FROM SYSTEM IMPORT TSIZE;

(* Constants *)

CONST
  dirsep = '\';


(* Pathname type *)

TYPE Pathname = POINTER TO Descriptor;

TYPE Descriptor = RECORD
  fullPath,
  dirPath,
  fileName,
  baseName,
  suffix : StringT;
  suffixType : SuffixType
END; (* Descriptor *)


(* Operations *)

(* --------------------------------------------------------------------------
 * procedure NewFromOSPath(path, osPath, status)
 * --------------------------------------------------------------------------
 * Creates a new pathname object, initialised from the path in osPath.
 * ------------------------------------------------------------------------ *)

PROCEDURE NewFromOSPath
  ( VAR path : Pathname; osPath : ARRAY OF CHAR; VAR status : Status );

BEGIN
  IF (HIGH(osPath) = 0) OR osPath[0] = ISO646.NUL THEN
    status := InvalidPath;
    RETURN
  END; (* IF *)

  charsProcessed := parsePathname(osPath, 0, dirpath, filename, suffix);
  
  IF NOT dirpath.found AND NOT filename.found THEN
    status := dirpath.status;
    RETURN
  END; (* IF *)
  
  ALLOCATE(newPath, TSIZE(Descriptor));

  (* obtain dirpath string *)
  IF dirpath.found AND dirpath.status = Success THEN
    status := dirpath.status;
    fullPathStart := dirpath.start;
    newPath^.dirpath :=
      String.forArraySlice(osPath, dirpath.start, dirpath.end)
  ELSE
    status := filename.status;
    fullPathStart := filename.start;
    newPath^.dirpath := NIL;
  END; (* IF *)
  
  (* obtain filename string *)
  IF filename.found AND filename.status = Success THEN
    fullPathEnd := filename.end;
    newPath^.filename :=
      String.forArraySlice(osPath, filename.start, filename.end)
  ELSE
    fullPathEnd := dirpath.end;
    newPath^.filename := NIL
  END; (* IF *)
  
  (* obtain basename and suffix strings *)
  IF suffix.found AND suffix.status = Success THEN
    newPath^.basename :=
      String.forArraySlice(osPath, filename.start, suffix.start - 2)
    newPath^.suffix :=
      String.forArraySlice(osPath, suffix.start, suffix.end);
    newPath^.suffixType := suffixTypeForString(newPath^.suffix)
  ELSE
    newPath^.basename := newPath^.filename;
    newPath^.suffix := NIL;
    newPath^.suffixType := NoSuffix
  END; (* IF *)
  
  (* obtain full path string *)
  newPath^.fullPath :=
    String.forArraySlice(osPath, fullPathStart, fullPathEnd);
    
  path := newPath;
END NewFromOSPath;


(* --------------------------------------------------------------------------
 * procedure newFromComponents(path, dirpath, basename, suffix, status)
 * --------------------------------------------------------------------------
 * Creates a new pathname object from the given component strings.
 * ------------------------------------------------------------------------ *)

PROCEDURE newFromComponents
  ( VAR path : Pathname;
    dirpath, basename, suffix : StringT; VAR status : Status );

VAR
  dirLength : CARDINAL;

BEGIN
  IF (HIGH(dirpath) = 0) OR (HIGH(basename) = 0) OR dirpath[0] = ISO646.NUL THEN
    status := Invalidpath;
    RETURN
  END (* IF *)

  dirLength := HIGH(dirpath);

  ALLOCATE(newPath, TSIZE(Descriptor));

  (* store what we were given *)
  newPath^.dirpath := dirpath;  
  newPath^.basename := basename;
  newPath^.suffix := suffix;

  (* set the suffix type *)
  newPath^.suffixType := suffixTypeForString(newPath^.suffix)

  (* construct the filename *)
  newPath^.filename := String.forConcatenation(basename, suffix);
  
  (* construct the full path *)
  IF dirpath[dirLength - 1] # dirsep THEN
    newPath^.fullpath := String.forConcatenation(newPath^.dirPath, 
      String.forConcatenation(dirsep, newPath^.filename)    
    );
  ELSE
    newPath^.fullpath := String.forConcatenation(newPath^.dirPath, newPath^.filename);
  END (* IF *)     

  (* if we've made it this far without RETURNing, then we have a success)
  newPath^.status := Success;
       
  path := newPath;
END newFromComponents;


(* --------------------------------------------------------------------------
 * function fullPath(path)
 * --------------------------------------------------------------------------
 * Returns a string with the full pathname of path.
 * ------------------------------------------------------------------------ *)
 
PROCEDURE fullPath ( path : Pathname ) : StringT;

BEGIN
  IF path = NIL THEN
    RETURN NIL
  ELSE
    RETURN path^.fullPath
  END (* IF *)
END fullPath;

(* --------------------------------------------------------------------------
 * function dirPath(path)
 * --------------------------------------------------------------------------
 * Returns a string with the dirpath of path.
 * ------------------------------------------------------------------------ *)
 
PROCEDURE dirPath ( path : Pathname ) : StringT;

BEGIN
  IF path = NIL THEN
    RETURN NIL
  ELSE
    RETURN path^.dirPath
  END (* IF *)
END dirPath;

(* --------------------------------------------------------------------------
 * function filename(path)
 * --------------------------------------------------------------------------
 * Returns a string with the filename of path.
 * ------------------------------------------------------------------------ *)
 
PROCEDURE filename ( path : Pathname ) : StringT;

BEGIN
  IF path = NIL THEN
    RETURN NIL
  ELSE
    RETURN path^.filename
  END (* IF *)
END filename;

(* --------------------------------------------------------------------------
 * function basename(path)
 * --------------------------------------------------------------------------
 * Returns a string with the basename of path.
 * ------------------------------------------------------------------------ *)
 
PROCEDURE basename ( path : Pathname ) : StringT;

BEGIN
  IF path = NIL THEN
    RETURN NIL
  ELSE
    RETURN path^.basename
  END (* IF *)
END basename;

(* --------------------------------------------------------------------------
 * function suffix(path)
 * --------------------------------------------------------------------------
 * Returns a string with the suffix of path.
 * ------------------------------------------------------------------------ *)
 
PROCEDURE suffix ( path : Pathname ) : StringT;

BEGIN
  IF path = NIL THEN
    RETURN NIL
  ELSE
    RETURN path^.suffix
  END (* IF *)
END suffix;

(* --------------------------------------------------------------------------
 * function suffixType(path)
 * --------------------------------------------------------------------------
 * Returns the suffix type of path.
 * ------------------------------------------------------------------------ *)
 
PROCEDURE suffixType ( path : Pathname ) : SuffixType;

BEGIN
  IF path = NIL THEN
    RETURN NoSuffix
  ELSE
    RETURN path^.suffixType
  END (* IF *)
END suffixType;

(* --------------------------------------------------------------------------
 * procedure Release(path)
 * --------------------------------------------------------------------------
 * Releases the path object and sets path to nil.
 * ------------------------------------------------------------------------ *)
 
PROCEDURE Release ( VAR path : Pathname );

BEGIN
  (* bail out if path invalid *)
  IF path = NIL THEN
    RETURN
  END; (* IF *)
  
  DEALLOCATE(path);
  path := NIL
END Release;

(* Operations on character arrays *)

(* --------------------------------------------------------------------------
 * function isValidOSPath(osPath)
 * --------------------------------------------------------------------------
 * Returns TRUE if osPath is a valid pathname, otherwise FALSE.
 * ------------------------------------------------------------------------ *)

PROCEDURE isValidOSPath ( osPath : ARRAY OF CHAR ) : BOOLEAN;

VAR
  valid : BOOLEAN;
  index : CARDINAL;
  
BEGIN
  IF (HIGH(osPath) = 0) OR (osPath[0] = ISO646.NUL) THEN
    RETURN FALSE
  END; (* IF *)
  
  index := parsePathname(osPath, 0, valid, NIL);
  
  RETURN valid

END isValidOSPath;

(* --------------------------------------------------------------------------
 * function isValidFilename(filename)
 * --------------------------------------------------------------------------
 * Returns TRUE if filename is a valid filename, otherwise FALSE.
 * ------------------------------------------------------------------------ *)

PROCEDURE isValidFilename ( filename : ARRAY OF CHAR ) : BOOLEAN;

VAR
  valid : BOOLEAN;
  index : CARDINAL;
  
BEGIN
  IF (HIGH(filename) = 0) OR (filename[0] = ISO646.NUL) THEN
    RETURN FALSE
  END; (* IF *)
  
  index := parsePathComponent(filename, 0, valid, NIL);
  
  RETURN valid
END isValidFilename;


(* ************************************************************************ *
 * Private Operations                                                       *
 * ************************************************************************ *)

 (* --------------------------------------------------------------------------
 * function parsePathname(path, startIndex, dirpath, basename, suffix)
 * --------------------------------------------------------------------------
 * pathname :=
 *   TO DO
 *   ;
 * ----------------------------------------------------------------------- *)

PROCEDURE parsePathname
  ( VAR (* CONST *) path : ARRAY OF CHAR;
    startIndex           : CARDINAL;
    VAR dirpath,
    basename,
    suffix               : Result ) : CARDINAL;

BEGIN
  (* TO DO *)
END parsePathname;

(* --------------------------------------------------------------------------
 * function parsePathComponent(path, index, invalid, suffixIndex)
 * --------------------------------------------------------------------------
 * pathComponent :=
 *   '.'? pathSubComponent ( '.' pathSubComponent )*
 *   ;
 * ----------------------------------------------------------------------- *)

PROCEDURE parsePathComponent
  ( VAR path        : ARRAY OF CHAR;
    index           : CARDINAL;
    VAR valid       : BOOLEAN;
    VAR suffixIndex : CARDINAL ) : CARDINAL;

BEGIN
  (* TO DO *)
END parsePathComponent;

 (* --------------------------------------------------------------------------
 * function suffixTypeForString(suffix)
 * --------------------------------------------------------------------------
 *   TO DO
 * ----------------------------------------------------------------------- *)

PROCEDURE suffixTypeForString
  ( suffix          : StringT ) : SuffixType;

BEGIN
  (* TO DO *)
END suffixTypeForString;

END Pathname.