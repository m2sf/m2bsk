(*!m2pim*) (* Copyright (c) 2020 Modula-2 Software Foundation. *)

IMPLEMENTATION MODULE Pathname; (* Microsoft Windows version *)

(* Microsoft Windows Pathname Parser for Modula-2 R10 Bootstrap Kernel *)

IMPORT ISO646, Char, String, PathnamePolicy;

IMPORT StringT; (* alias for String.String *)

FROM Storage IMPORT DEALLOCATE;
FROM SYSTEM IMPORT TSIZE;

(* Constants *)

CONST
  DirSep = CHR(92); (* ASCII backslash *)
  NoFileNameFound = -1;
  NoSuffixFound = -1;

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

  (* if we've made it this far without RETURNing, then we have a success *)
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
 *  ( server | device )? rootPath | ( '.' | parentPath ) rootPath? |
 *  filenameOnly
 *  ;
 *
 * server :=
 *   '\\' ComponentLeadChar+
 *   ;
 *
 * device :=
 *   ( 'a' .. 'z' | 'A' .. 'Z' ) ':'
 *   ;
 * ----------------------------------------------------------------------- *)

PROCEDURE parsePathname
  ( VAR (* CONST *) path : ARRAY OF CHAR;
    VAR startIndex       : CARDINAL;
    VAR dirpath,
    basename,
    suffix               : Result ) : CARDINAL;

VAR
  filenamePos   : Integer;
  invalid       : BOOLEAN;

BEGIN

  (* intermediate filename position *)
  filenamePos := NoFileNameFound;

  (* server? rootPath *)
  IF (dirpath[startIndex] = DirSep) THEN
    
    (* server? *)
    IF (dirpath[startIndex+1] = DirSep) THEN
      (* '\\' *)
      startIndex := startIndex + 2;

      (* ComponentLeadChar+ *)
      IF (isPathComponenteLeadChar(dirpath[startIndex])) THEN
        startIndex := startIndex + 1;
        WHILE (isPathComponenteLeadChar(dirpath[startIndex])) DO
          startIndex := startIndex + 1;
        END; (* WHILE *)
      ELSE (* invalid path *)
        invalid := TRUE;
        return startIndex;
      END; (* IF *)
    END; (* IF *)
    (* rootPath *)
    startIndex := parseRootPath(dirpath, startIndex, filenamePos);
  
  (* device rootPath *)
  ELSIF ((dirpath[startIndex+1] = ':') AND (isLetter(dirpath[startIndex]))) THEN
      (* ( 'a' .. 'z' | 'A' .. 'Z' ) ':' *)
      startIndex := startIndex + 2;  

      (* rootPath*)
      IF (dirpath[startIndex] = dirsep) THEN
        startIndex := parseRootPath(dirpath, startIndex, filenamePos);
      ELSE (* invalid path *)
        invalid := TRUE;
        return startIndex;
      END; (* IF *)
    END; (* IF *)

  (* leading period *)
  ELSIF (dirpath[startIndex] = '.') THEN
    (* '.' *)
    IF (dirpath[startIndex+1] = ISO646.NUL) THEN
      startIndex := startIndex+1;    
    (* '.' rootPath *)
    ELSIF (dirpath[startIndex+1] = dirsep) THEN
      startIndex := parseRootPath(dirpath, startIndex, filenamePos);
    (* '..' ( '\' '..' )* rootPath? *)
    ELSIF (dirpath[startIndex+1] = '.') THEN
      (* '..' ( '\' '..' )* *)
      startIndex = parseParentPath(dirpath, startIndex);
      
      (* rootPath? *)
      IF (dirpath[startIndex] = dirsep) THEN
        startIndex := parseRootPath(dirpath, startIndex, filenamePos);
      END; (* IF *)      
    END; (* ELSIF *)
    (* '.' filename *)
    ELSIF (IS_PATH_COMPONENT_LEAD_CHAR(path[index+1])) {
      filenamePos := startIndex;
      startIndex := parsePathComponent(dirpath, startIndex, invalid, NIL);
    END; (* ELSIF *)
    ELSE (* invalid pathname *) 
      invalid := TRUE;
      return startIndex;
    END; (* IF *)
  END; (* IF *)

  (* filenameOnly *)
  ELSIF (isPathComponentLeadChar(dirpath[startIndex])) THEN
    filenamePos := startIndex;
    startIndex := parsePathComponent(dirpath, startIndex, invalid, NIL);

  (* invalid pathname *)
  ELSE
    invalid := TRUE;
    return startIndex;
  END; (* IF *)
  
  (* pathname should end here, otherwise it is invalid *)
  IF (dirpath[startIndex] # ISO646.NUL) THEN
    invalid := TRUE;
  END; (* IF *)
  
  (* if successful, pass back filename index *)
  IF NOT invalid THEN
    startIndex := filenamePOS;
  END; (* IF *)

  RETURN startIndex
END parsePathname;

(* --------------------------------------------------------------------------
 * function parseRootPath()
 * --------------------------------------------------------------------------
 * Verifies a substring of path starting at the given index against the EBNF
 * rule for rootPath (see below) and returns the index of the character that
 * follows the last matched character.  If the substring does not match,
 * processing stops at the first mismatched character and true is passed in
 * out-parameter invalid, otherwise false.  Upon success, the index of the
 * last found directory separator within the matched substring is passed in
 * out-parameter last_dirsep, unless it is NULL.  Value NO_DIRSEP_FOUND
 * indicates that no such separator was found within the matched substring.
 * --------------------------------------------------------------------------
 * rootPath :=
 *   '\' ( pathComponent '\' )* pathComponent?
 *   ;
 * ----------------------------------------------------------------------- *)

PROCEDURE parseRootPath
  ( VAR path        : ARRAY OF CHAR;
    VAR index,      : CARDINAL 
    VAR filenamePos : INTEGER) : CARDINAL;

VAR
  invalid           : BOOLEAN;

BEGIN
  (* intermediate filename index *)
  filenamePos := NoFileNameFound;
    
  (* '\' *)
  index := index + 1;

  (* ( pathComponent '\' )* pathComponent? *)
  WHILE (((PathCompMayContainPeriod = TRUE) AND (path[index] = '.')) OR
         (isPathComponentLeadChar(path[index]))) DO
    
    (* possibly a filename, remember position *)
    filenamePos := index;
    
    (* pathComponent *)
    index := parsePathComponent(path, index, invalid, NIL);
    
    (* bail if error occurred *)
    IF invalid = TRUE THEN
      return index;
    END; (* IF *)
    
    (* '\' *)
    IF (path[index] = DirSep) THEN
      (* last path component was not a filename *)
      filenamePos := nofilenamefound;
      index := index + 1;
    (* pathComponent? *)
    ELSIF (path[index] = ISO646.NUL) THEN
      EXIT;
    END; (* IF *)
  END; (* WHILE *)
  
  (* pass back index of last path component *)
  index := filenamePos;
      
  (* pass back validity *)
  invalid := FALSE;

  RETURN index
END ParseRootPath;

(* --------------------------------------------------------------------------
 * function isReservedPathComponent(path, startIndex, endIndex)
 * --------------------------------------------------------------------------
 * reservedPathComponent :=
 *   'AUX' | 'CON' | 'NUL' | 'PRN' | ( 'COM' | 'LPT' ) ( '0' .. '9' )
 *   ;
 * ----------------------------------------------------------------------- *)
PROCEDURE isReservedPathComponent
  ( path            : ARRAY OF CHAR; (* in *)
    startIndex      : CARDINAL;      (* in *)                                              
    endIndex        : CARDINAL;      (* in *)
    ) : BOOLEAN;

VAR
  len               : CARDINAL;
  ch1, ch2, ch3     : CHAR;
     
BEGIN
  len := endIndex - startIndex + 1;
  IF (len = 3 OR len = 4) THEN
    (* TO DO *)
  ELSE
    RETURN FALSE;
  END; (* IF *)

  (* test for AUX, CON, NUL and PRN *)
  IF (len = 3) THEN
    (* TO DO *)
  END; (* IF *)
       
  RETURN FALSE;
END isReservedPathComponent;

(* --------------------------------------------------------------------------
 * function isOptionalComponentChar(ch)
 * --------------------------------------------------------------------------
 *   '-' | '~'
 * ------------------------------------------------------------------------ *)
PROCEDURE isOptionalComponentChar(ch)
  ( ch              : CHAR; (* in *) ) : BOOLEAN;

BEGIN
  (* TO DO *)
END isOptionalComponentChar;
     
(* --------------------------------------------------------------------------
 * function isPathComponentChar(ch)
 * --------------------------------------------------------------------------
 * PathComponentChar :=
 *   PathComponentLeadChar | '-' | '~'
 *   ;
 * ----------------------------------------------------------------------- *)
PROCEDURE isPathComponentChar
  ( ch              : CHAR; (* in *) ) : BOOLEAN;

BEGIN
  RETURN (isPathComponentLeadChar(ch) OR isOptionalComponentChar(ch));
END isPathComponentChar;
     
(* --------------------------------------------------------------------------
 * function parsePathSubcomponent(path, index)
 * --------------------------------------------------------------------------
 * pathSubComponent :=
 *   ComponentLeadChar ComponentChar* ( ' ' ComponentChar+ )*
 *   ;
 * ----------------------------------------------------------------------- *)
PROCEDURE parsePathSubcomponent
  ( path            : ARRAY OF CHAR; (* in *)
    index           : CARDINAL;      (* in *)
    VAR invalid     : BOOLEAN;       (* out, may not be NIL *)
    ) : CARDINAL;

VAR
  startIndex        : CARDINAL;

BEGIN
  (* remember start position *)
  startIndex := index;

  (* ComponentLeadChar *)
  index := index + 1;

  (* ComponentChar *)     
  WHILE isPathComponentChar(path[index]) DO
    index := index + 1;
  END; (* WHILE *)     

  (* TO DO *)

  (* check for AUX, CON, NUL, PRN, COMx and LPTx *)
  IF NOT isReservedPathComponent(path, startIndex, index - 1) THEN
    (* not reserved -- all clear *)
    index := index - 1;
    invalid := FALSE;
  ELSE (* invalid path : reserved name *)
    (* reset index to last character of offending sub-component *)
    index := index - 1;
    invalid := TRUE;   
  END; (* IF *)

  RETURN index;          
END parsePathSubcomponent; 
      
      
(* --------------------------------------------------------------------------
 * function parsePathComponent(path, index, invalid, suffixIndex)
 * --------------------------------------------------------------------------
 * Verifies a substring of path starting at the given index against the EBNF
 * rule for pathComponent (see below) and returns the index of the character
 * that follows the last matched character.  If the substring does not match,
 * processing stops at the first mismatched character and true is passed in
 * out-parameter invalid, otherwise false.  Upon success, the index of the
 * last found period within the matched substring is passed in out-parameter
 * last_period, unless it is NULL.  Value NO_PERIOD_FOUND indicates that no
 * period was found within the matched substring.
 * --------------------------------------------------------------------------
 * pathComponent :=
 *   [# '.'? #] pathSubComponent
 *   ( '.' pathSubComponent [# ( '.' pathSubComponent )* #] )?
 *   ;
 * ----------------------------------------------------------------------- *)

PROCEDURE parsePathComponent
  ( VAR path        : ARRAY OF CHAR;
    index           : CARDINAL;
    VAR invalid     : BOOLEAN;
    VAR suffixIndex : CARDINAL ) : CARDINAL;

VAR
  suffixPos         : CARDINAL;          

BEGIN
  (* intermediate suffix index *)
  suffixPos := NoSuffixFound;

  (* pathSubComponent *)
  IF isPathComponentLeadChar(path[index]) THEN
    index := parsePathSubcomponent(path, index, invalid);

    (* bail if error occurred *)
    IF invalid THEN
      RETURN index;
    END; (* IF *)
  ELSE (* invalid path *)
    invalid := TRUE;
    RETURN index;   
  END (* IF *)
       
  (* TO DO *)
       
  (* pass back validity *)
  invalid := FALSE;     

  RETURN index;
END parsePathComponent;

(* --------------------------------------------------------------------------
 * function parseParentPath(path, index)
 * --------------------------------------------------------------------------
 * parentPath :=
 *   '..' ( '\' '..' )*
 *   ;
 * ----------------------------------------------------------------------- *)

PROCEDURE parseParentPath
  ( path            : ARRAY OF CHAR;
    index           : CARDINAL) : CARDINAL;

  BEGIN
  
  (* '..' *)
  index := index + 2;
  
  (* ( '\' '..' )* *)
  WHILE ((path[index] = DirSep) AND
         (path[index+1] = '.') AND (path[index+2] = '.')) DO
    index := index + 3;
  END; (* WHILE *)
  
  RETURN index
END parseParentPath;

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

(* --------------------------------------------------------------------------
 * function isPathComponentLeadChar(ch)
 * --------------------------------------------------------------------------
 * PathComponentFirstChar :=
 *   'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_'
 *   ;
 * ----------------------------------------------------------------------- *)

  PROCEDURE isPathComponentLeadChar
    ( ch            : CHAR ) : BOOLEAN;
  
  BEGIN
    RETURN (isAlphaNum(ch) OR ch = '_')    
  END isPathComponentLeadChar;

END Pathname.
