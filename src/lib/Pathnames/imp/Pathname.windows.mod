(*!m2pim*) (* Copyright (c) 2020 Modula-2 Software Foundation. *)

IMPLEMENTATION MODULE Pathname; (* Microsoft Windows version *)

(* Microsoft Windows Pathname Parser for Modula-2 R10 Bootstrap Kernel *)

IMPORT Char, String, PathnamePolicy;

IMPORT StringT; (* alias for String.String *)

FROM Storage IMPORT DEALLOCATE;


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
  (* TO DO *)
END NewFromOSPath;


(* --------------------------------------------------------------------------
 * procedure newFromComponents(path, dirpath, basename, suffix, status)
 * --------------------------------------------------------------------------
 * Creates a new pathname object from the given component strings.
 * ------------------------------------------------------------------------ *)

PROCEDURE newFromComponents
  ( VAR path : Pathname;
    dirpath, basename, suffix : StringT; VAR status : Status );

BEGIN
  (* TO DO *)
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

BEGIN
  (* TO DO *)
END isValidOSPath;

(* --------------------------------------------------------------------------
 * function isValidFilename(filename)
 * --------------------------------------------------------------------------
 * Returns TRUE if filename is a valid filename, otherwise FALSE.
 * ------------------------------------------------------------------------ *)

PROCEDURE isValidFilename ( filename : ARRAY OF CHAR ) : BOOLEAN;

BEGIN
  (* TO DO *)
END isValidFilename;

END Pathname.