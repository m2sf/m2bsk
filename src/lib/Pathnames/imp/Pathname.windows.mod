(*!m2pim*) (* Copyright (c) 2020 Modula-2 Software Foundation. *)

IMPLEMENTATION MODULE Pathname; (* Microsoft Windows version *)

(* Microsoft Windows Pathname Parser for Modula-2 R10 Bootstrap Kernel *)

IMPORT Char, String, PathnamePolicy;

IMPORT StringT; (* alias for String.String *)


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
  (* TO DO *)
END fullPath;

(* --------------------------------------------------------------------------
 * function dirPath(path)
 * --------------------------------------------------------------------------
 * Returns a string with the dirpath of path.
 * ------------------------------------------------------------------------ *)
 
PROCEDURE dirPath ( path : Pathname ) : StringT;

BEGIN
  (* TO DO *)
END dirPath;

(* --------------------------------------------------------------------------
 * function filename(path)
 * --------------------------------------------------------------------------
 * Returns a string with the filename of path.
 * ------------------------------------------------------------------------ *)
 
PROCEDURE filename ( path : Pathname ) : StringT;

BEGIN
  (* TO DO *)
END filename;

(* --------------------------------------------------------------------------
 * function basename(path)
 * --------------------------------------------------------------------------
 * Returns a string with the basename of path.
 * ------------------------------------------------------------------------ *)
 
PROCEDURE basename ( path : Pathname ) : StringT;

BEGIN
  (* TO DO *)
END basename;

(* --------------------------------------------------------------------------
 * function suffix(path)
 * --------------------------------------------------------------------------
 * Returns a string with the suffix of path.
 * ------------------------------------------------------------------------ *)
 
PROCEDURE suffix ( path : Pathname ) : StringT;

BEGIN
  (* TO DO *)
END suffix;

(* --------------------------------------------------------------------------
 * function suffixType(path)
 * --------------------------------------------------------------------------
 * Returns the suffix type of path.
 * ------------------------------------------------------------------------ *)
 
PROCEDURE suffixType ( path : Pathname ) : SuffixType;

BEGIN
  (* TO DO *)
END suffixType;

(* --------------------------------------------------------------------------
 * procedure Release(path)
 * --------------------------------------------------------------------------
 * Releases the path object and sets path to nil.
 * ------------------------------------------------------------------------ *)
 
PROCEDURE Release ( VAR path : Pathname );

BEGIN
  (* TO DO *)
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