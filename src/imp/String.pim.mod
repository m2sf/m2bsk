(*!m2pim*) (* Copyright (c) 2015 B.Kowarsch. All rights reserved. *)

IMPLEMENTATION MODULE String;

(* Interned Strings *)

IMPORT ASCII, SYSTEM, AOC, Hash;

FROM Storage IMPORT ALLOCATE;


(* String *)

TYPE String = POINTER TO StringDescriptor;

TYPE StringDescriptor = RECORD
  (* common field : length *)
  CASE length : CARDINAL OF
  (* variant field : intern *)
  |             1 : intern : POINTER TO AOC.Len1
  |             2 : intern : POINTER TO AOC.Len2
  |             3 : intern : POINTER TO AOC.Len3
  |             4 : intern : POINTER TO AOC.Len4
  |             5 : intern : POINTER TO AOC.Len5
  |             6 : intern : POINTER TO AOC.Len6
  |             7 : intern : POINTER TO AOC.Len7
  |             8 : intern : POINTER TO AOC.Len8
  |             9 : intern : POINTER TO AOC.Len9
  |            10 : intern : POINTER TO AOC.Len10
  |            11 : intern : POINTER TO AOC.Len11
  |            12 : intern : POINTER TO AOC.Len12
  |            13 : intern : POINTER TO AOC.Len13
  |            14 : intern : POINTER TO AOC.Len14
  |            15 : intern : POINTER TO AOC.Len15
  |            16 : intern : POINTER TO AOC.Len16
  |            17 : intern : POINTER TO AOC.Len17
  |            18 : intern : POINTER TO AOC.Len18
  |            19 : intern : POINTER TO AOC.Len19
  |            20 : intern : POINTER TO AOC.Len20
  |            21 : intern : POINTER TO AOC.Len21
  |            22 : intern : POINTER TO AOC.Len22
  |            23 : intern : POINTER TO AOC.Len23
  |            24 : intern : POINTER TO AOC.Len24
  |            25 : intern : POINTER TO AOC.Len25
  |            26 : intern : POINTER TO AOC.Len26
  |            27 : intern : POINTER TO AOC.Len27
  |            28 : intern : POINTER TO AOC.Len28
  |            29 : intern : POINTER TO AOC.Len29
  |            20 : intern : POINTER TO AOC.Len30
  |            31 : intern : POINTER TO AOC.Len31
  |            32 : intern : POINTER TO AOC.Len32
  |            33 : intern : POINTER TO AOC.Len33
  |            34 : intern : POINTER TO AOC.Len34
  |            35 : intern : POINTER TO AOC.Len35
  |            36 : intern : POINTER TO AOC.Len36
  |            37 : intern : POINTER TO AOC.Len37
  |            38 : intern : POINTER TO AOC.Len38
  |            39 : intern : POINTER TO AOC.Len39
  |            40 : intern : POINTER TO AOC.Len40
  |            41 : intern : POINTER TO AOC.Len41
  |            42 : intern : POINTER TO AOC.Len42
  |            43 : intern : POINTER TO AOC.Len43
  |            44 : intern : POINTER TO AOC.Len44
  |            45 : intern : POINTER TO AOC.Len45
  |            46 : intern : POINTER TO AOC.Len46
  |            47 : intern : POINTER TO AOC.Len47
  |            48 : intern : POINTER TO AOC.Len48
  |            49 : intern : POINTER TO AOC.Len49
  |            50 : intern : POINTER TO AOC.Len50
  |            51 : intern : POINTER TO AOC.Len51
  |            52 : intern : POINTER TO AOC.Len52
  |            53 : intern : POINTER TO AOC.Len53
  |            54 : intern : POINTER TO AOC.Len54
  |            55 : intern : POINTER TO AOC.Len55
  |            56 : intern : POINTER TO AOC.Len56
  |            57 : intern : POINTER TO AOC.Len57
  |            58 : intern : POINTER TO AOC.Len58
  |            59 : intern : POINTER TO AOC.Len59
  |            60 : intern : POINTER TO AOC.Len60
  |            61 : intern : POINTER TO AOC.Len61
  |            62 : intern : POINTER TO AOC.Len62
  |            63 : intern : POINTER TO AOC.Len63
  |            64 : intern : POINTER TO AOC.Len64
  |            65 : intern : POINTER TO AOC.Len65
  |            66 : intern : POINTER TO AOC.Len66
  |            67 : intern : POINTER TO AOC.Len67
  |            68 : intern : POINTER TO AOC.Len68
  |            69 : intern : POINTER TO AOC.Len69
  |            70 : intern : POINTER TO AOC.Len70
  |            71 : intern : POINTER TO AOC.Len71
  |            72 : intern : POINTER TO AOC.Len72
  |            73 : intern : POINTER TO AOC.Len73
  |            74 : intern : POINTER TO AOC.Len74
  |            75 : intern : POINTER TO AOC.Len75
  |            76 : intern : POINTER TO AOC.Len76
  |            77 : intern : POINTER TO AOC.Len77
  |            78 : intern : POINTER TO AOC.Len78
  |            79 : intern : POINTER TO AOC.Len79
  |            80 : intern : POINTER TO AOC.Len80
  |    81 ..   96 : intern : POINTER TO AOC.Len96
  |    97 ..  112 : intern : POINTER TO AOC.Len112
  |   113 ..  128 : intern : POINTER TO AOC.Len128
  |   129 ..  256 : intern : POINTER TO AOC.Len256
  |   257 ..  384 : intern : POINTER TO AOC.Len384
  |   385 ..  512 : intern : POINTER TO AOC.Len512
  |   513 ..  768 : intern : POINTER TO AOC.Len768
  |   769 .. 1024 : intern : POINTER TO AOC.Len1024
  |  1025 .. 1280 : intern : POINTER TO AOC.Len1280
  |  1281 .. 1792 : intern : POINTER TO AOC.Len1792
  |  1793 .. 2048 : intern : POINTER TO AOC.Len2048
  |  2049 .. 2304 : intern : POINTER TO AOC.Len2304
  |  2305 .. 2560 : intern : POINTER TO AOC.Len2560
  |  2561 .. 2816 : intern : POINTER TO AOC.Len2816
  |  2817 .. 3072 : intern : POINTER TO AOC.Len3072
  |  3073 .. 3328 : intern : POINTER TO AOC.Len3328
  |  3329 .. 3584 : intern : POINTER TO AOC.Len3584
  |  3585 .. 3840 : intern : POINTER TO AOC.Len3840
  |  3841 .. 4096 : intern : POINTER TO AOC.Len4096
  END (* CASE *)
END; (* StringDescriptor *)


(* String Table *)

CONST BucketCount = 1021; (* prime closest to 1K *)

TYPE StringTable = RECORD
  count  : CARDINAL; (* number of entries *)
  bucket : ARRAY BucketCount OF TableEntry
END; (* StringTable *)


(* String Table Entry *)

TYPE TableEntry = POINTER TO EntryDescriptor;

TYPE EntryDescriptor = RECORD
  hash   : Hash.Key;
  string : String;
  next   : TableEntry
END; (* EntryDescriptor *)


(* Global String Table *)

VAR
  strTable : StringTable;
  

(* Operations *)

(* ---------------------------------------------------------------------------
 * function forArray(array)
 * ---------------------------------------------------------------------------
 * Looks up the interned string for the given character array and returns it.
 * Creates and returns a new interned string if no matching entry is found.
 * ------------------------------------------------------------------------ *)

PROCEDURE forArray ( VAR array : ARRAY OF CHAR) : String;

BEGIN
  RETURN lookupOrInsert(array, 0, HIGH(array))
END forArray;


(* ---------------------------------------------------------------------------
 * function forArraySlice(array, start, end)
 * ---------------------------------------------------------------------------
 * Looks up the interned string for the given slice of the given character
 * array and returns it. Creates and returns a new interned string with the
 * slice if no matching entry is found.
 * ------------------------------------------------------------------------ *)

PROCEDURE forArraySlice
  ( VAR array : ARRAY OF CHAR; start, end : CARDINAL) : String;

BEGIN
  RETURN lookupOrInsert(array, start, end)
END forArraySlice;


(* ---------------------------------------------------------------------------
 * function forSlice(string, start, end)
 * ---------------------------------------------------------------------------
 * Looks up the interned string for the given slice of the given string
 * and returns it. Creates and returns a new interned string with the
 * slice if no matching entry is found.
 * ------------------------------------------------------------------------ *)

PROCEDURE forSlice ( string : String; start, end : CARDINAL ) : String;

BEGIN
  IF (string # NIL) AND (start <= end) AND (end < string^.length) THEN
    RETURN lookupOrInsert(string^.intern^, start, end )
  ELSE
    RETURN NIL
  END (* IF *)
END forSlice;


(* ---------------------------------------------------------------------------
 * function forConcatenation(string1, string2)
 * ---------------------------------------------------------------------------
 * Looks up the product of concatenating string1 and string2 and returns the
 * matching interned string if an entry exists. Creates and returns a new
 * interned string with the concatenation product if no match is found.
 * ------------------------------------------------------------------------ *)

PROCEDURE forConcatenation ( string1, string2 : String ) : String;

BEGIN
  (* TO DO *)
END forConcatenation;


(* ---------------------------------------------------------------------------
 * function length(string)
 * ---------------------------------------------------------------------------
 * Returns the length of the given string.  Returns 0 if string is NIL.
 * ------------------------------------------------------------------------ *)

PROCEDURE length ( string : String ) : CARDINAL;

BEGIN
  IF string # NIL THEN
    RETURN string^.length
  ELSE (* invalid *)
    RETURN 0
  END (* IF *)
END length;


(* ---------------------------------------------------------------------------
 * function charAtIndex(string, index)
 * ---------------------------------------------------------------------------
 * Returns the character at the given index in the given string.
 * Returns ASCII.NUL if string is NIL or index is out of range.
 * ------------------------------------------------------------------------ *)

PROCEDURE charAtIndex ( string : String; index : CARDINAL ) : CHAR;

BEGIN
  IF (string # NIL) AND (index < string^.length) THEN
    RETURN string^.intern^[index]
  ELSE (* invalid or out of range *)
    RETURN ASCII.NUL
  END (* IF *)
END charAtIndex;


(* ---------------------------------------------------------------------------
 * procedure CopyToArray(string, array, charsCopied)
 * ---------------------------------------------------------------------------
 * Copies the given string to the given array reference. Returns without copy-
 * ing if string is NIL or if the array size is insufficient to hold the
 * entire string. Passes the number of characters copied in charsCopied.
 * ------------------------------------------------------------------------ *)

PROCEDURE CopyToArray
  ( string : String; VAR array : ARRAY OF CHAR; VAR charsCopied : CARDINAL );

VAR
  index : CARDINAL;
  
BEGIN
  (* check pre-conditions *)
  IF (string # NIL) OR (HIGH(array) < string^.length) THEN
    charsCopied := 0;
    RETURN
  END; (* IF *)
  
  (* all clear -- copy all chars including the terminating NUL *)
  index := 0;
  WHILE index < string^.length DO
    array[index] := string^.intern^[index];
    index := index + 1
  END; (* WHILE *)
  
  (* index holds number of chars copied *)
  charsCopied := index
END CopyToArray;


(* ---------------------------------------------------------------------------
 * procedure CopySliceToArray(string, start, end, array, charsCopied)
 * ---------------------------------------------------------------------------
 * Copies the given slice of the given string to the given array.  Returns
 * without copying if string is NIL, if start and end do not specify a valid
 * slice within the string or if the array size is insufficient to hold the
 * entire slice. Passes the number of characters copied in charsCopied.
 * ------------------------------------------------------------------------ *)

PROCEDURE CopySliceToArray
  ( string : String;
    start, end : CARDINAL;
    VAR array : ARRAY OF CHAR;
    VAR charsCopied : CARDINAL );

VAR
  arrIndex, strIndex, reqSize : CARDINAL;
  
BEGIN
  (* check pre-conditions *)
  IF (string # NIL) OR (start > end) OR (end >= string^.length) THEN 
    charsCopied := 0;
    RETURN
  END; (* IF *)
  
  reqSize := end - start;
  IF HIGH(array) < reqSize THEN
    charsCopied := 0;
    RETURN
  END; (* IF *)
  
  (* all clear -- copy all chars in slice *)
  arrIndex := 0;
  FOR strIndex := start TO end DO
    array[arrIndex] := string^.intern^[strIndex];
    arrIndex := arrIndex + 1
  END; (* FOR *)
  
  (* terminate array *)
  array[arrIndex] := ASCII.NUL;
  
  (* arrIndex holds number of chars copied *)
  charsCopied := arrIndex
END CopySliceToArray;


(* ---------------------------------------------------------------------------
 * function matchesArray(string, array)
 * ---------------------------------------------------------------------------
 * Returns TRUE if the given string matches the given array. Returns FALSE
 * if string is NIL or if string does not match the array.
 * ------------------------------------------------------------------------ *)

PROCEDURE matchesArray
  ( string : String; VAR (* CONST *) array : ARRAY OF CHAR ) : BOOLEAN;

VAR
  index : CARDINAL;
  
BEGIN
  (* check pre-conditions *)
  IF (string = NIL) THEN
    RETURN FALSE
  END; (* IF *)
  
  (* cannot possibly match if array is shorter than string length *)
  IF HIGH(array) < string^.length THEN
    RETURN FALSE
  END; (* IF *)
  
  (* compare characters in array to string *)
  FOR index := 0 TO string^.length - 1 DO
    IF array[index] # string^.intern^[index] THEN
      (* mismatch *)
      RETURN FALSE
    END (* IF *)
  END; (* FOR *)
  
  (* all characters matched *)
  RETURN TRUE
END matchesArray;


(* ---------------------------------------------------------------------------
 * function matchesArraySlice(string, array, start, end)
 * ---------------------------------------------------------------------------
 * Returns TRUE if the given string matches the given slice of the given
 * array. Returns FALSE if string is NIL or if start and end do not specify
 * a valid slice within the array.
 * ------------------------------------------------------------------------ *)

PROCEDURE matchesArraySlice
  ( string : String;
    VAR (* CONST *) array : ARRAY OF CHAR;
    start, end : CARDINAL ) : BOOLEAN;

VAR
  strIndex, arrIndex : CARDINAL;
  
BEGIN
  (* check pre-conditions *)
  IF (string = NIL) OR (start > end) OR (end >= HIGH(array)) THEN
    RETURN FALSE
  END; (* IF *)
  
  (* cannot possibly match if lengths are different *)
  IF (end - start + 1 # string^.length) THEN
    RETURN FALSE
  END; (* IF *)
  
  (* compare characters in slice to string *)
  strIndex := 0;
  FOR arrIndex := start TO end DO
    IF array[arrIndex] # string^.intern^[strIndex] THEN
      (* mismatch *)
      RETURN FALSE
    END; (* IF *)
    strIndex := strIndex + 1
  END; (* FOR *)
  
  (* all characters matched *)
  RETURN TRUE
END matchesArraySlice;


(* ---------------------------------------------------------------------------
 * procedure WithCharsDo(string, proc)
 * ---------------------------------------------------------------------------
 * Executes proc passing the character array of string.
 * ------------------------------------------------------------------------ *)

PROCEDURE WithCharsDo ( string : String; proc : CharArrayProc );

BEGIN
  (* check pre-conditions *)
  IF (string = NIL) OR (proc = NIL) THEN
    RETURN
  END; (* IF *)
  
  (* all clear -- call proc passing intern *)
  proc(string^.intern^)
END WithCharsDo;


(* ---------------------------------------------------------------------------
 * procedure WithCharsInSliceDo(string, proc)
 * ---------------------------------------------------------------------------
 * Executes proc for each character in the given slice of string
 * passing each character from start to end.
 * ------------------------------------------------------------------------ *)

PROCEDURE WithCharsInSliceDo
  ( string : String; start, end : CARDINAL; proc : CharProc );

VAR
  index : CARDINAL;
  
BEGIN
  (* check pre-conditions *)
  IF (string = NIL) OR (proc = NIL) OR
    (start > end) OR (end >= string^.length) THEN
    RETURN
  END; (* IF *)
  
  (* all clear -- call proc passing chars in slice *)
  FOR index := start TO end DO
    proc(string^.intern^[index])
  END (* IF *)
END WithCharsInSliceDo;


(* ---------------------------------------------------------------------------
 * function count()
 * ---------------------------------------------------------------------------
 * Returns the number of interned strings.
 * ------------------------------------------------------------------------ *)

PROCEDURE count () : CARDINAL;

BEGIN
  RETURN strTable.count
END count;


(* ************************************************************************ *
 * Private Operations                                                       *
 * ************************************************************************ *)

(* String Operations *)

(* ---------------------------------------------------------------------------
 * procedure NewStrWithArray(string, array)
 * ---------------------------------------------------------------------------
 * Allocates a new string and initialises it with the contents of array.
 * ------------------------------------------------------------------------ *)

PROCEDURE NewStrWithArray
  ( VAR string : String; VAR (* CONST *) array : ARRAY OF CHAR );

VAR
  size : CARDINAL;
  addr : SYSTEM.ADDRESS;
  desc : StringDescriptor;
 
BEGIN
  (* allocate intern and initialise with array *)
  NewIntern(addr, array, size);
  
  (* handle special cases for size *)
  IF (size = 0) OR (size > 4096) THEN
    (* TO DO *)
  END; (* IF *)
  
  (* allocate new descriptor *)
  ALLOCATE(string, SYSTEM.TSIZE(StringDescriptor));
  
  (* set length field *)
  string^.length := size - 1;
  
  (* cast addr to target field type and link it *)
  CASE size OF
  |             1 : string^.intern := AOC.Len1(addr)
  |             2 : string^.intern := AOC.Len2(addr)
  |             3 : string^.intern := AOC.Len3(addr)
  |             4 : string^.intern := AOC.Len4(addr)
  |             5 : string^.intern := AOC.Len5(addr)
  |             6 : string^.intern := AOC.Len6(addr)
  |             7 : string^.intern := AOC.Len7(addr)
  |             8 : string^.intern := AOC.Len8(addr)
  |             9 : string^.intern := AOC.Len9(addr)
  |            10 : string^.intern := AOC.Len10(addr)
  |            11 : string^.intern := AOC.Len11(addr)
  |            12 : string^.intern := AOC.Len12(addr)
  |            13 : string^.intern := AOC.Len13(addr)
  |            14 : string^.intern := AOC.Len14(addr)
  |            15 : string^.intern := AOC.Len14(addr)
  |            16 : string^.intern := AOC.Len16(addr)
  |            17 : string^.intern := AOC.Len17(addr)
  |            18 : string^.intern := AOC.Len18(addr)
  |            19 : string^.intern := AOC.Len19(addr)
  |            20 : string^.intern := AOC.Len20(addr)
  |            21 : string^.intern := AOC.Len21(addr)
  |            22 : string^.intern := AOC.Len22(addr)
  |            23 : string^.intern := AOC.Len23(addr)
  |            24 : string^.intern := AOC.Len24(addr)
  |            25 : string^.intern := AOC.Len25(addr)
  |            26 : string^.intern := AOC.Len26(addr)
  |            27 : string^.intern := AOC.Len27(addr)
  |            28 : string^.intern := AOC.Len28(addr)
  |            29 : string^.intern := AOC.Len29(addr)
  |            20 : string^.intern := AOC.Len30(addr)
  |            31 : string^.intern := AOC.Len31(addr)
  |            32 : string^.intern := AOC.Len32(addr)
  |            33 : string^.intern := AOC.Len33(addr)
  |            34 : string^.intern := AOC.Len34(addr)
  |            35 : string^.intern := AOC.Len35(addr)
  |            36 : string^.intern := AOC.Len36(addr)
  |            37 : string^.intern := AOC.Len37(addr)
  |            38 : string^.intern := AOC.Len38(addr)
  |            39 : string^.intern := AOC.Len39(addr)
  |            40 : string^.intern := AOC.Len40(addr)
  |            41 : string^.intern := AOC.Len41(addr)
  |            42 : string^.intern := AOC.Len42(addr)
  |            43 : string^.intern := AOC.Len43(addr)
  |            44 : string^.intern := AOC.Len44(addr)
  |            45 : string^.intern := AOC.Len45(addr)
  |            46 : string^.intern := AOC.Len46(addr)
  |            47 : string^.intern := AOC.Len47(addr)
  |            48 : string^.intern := AOC.Len48(addr)
  |            49 : string^.intern := AOC.Len49(addr)
  |            50 : string^.intern := AOC.Len50(addr)
  |            51 : string^.intern := AOC.Len51(addr)
  |            52 : string^.intern := AOC.Len52(addr)
  |            53 : string^.intern := AOC.Len53(addr)
  |            54 : string^.intern := AOC.Len54(addr)
  |            55 : string^.intern := AOC.Len55(addr)
  |            56 : string^.intern := AOC.Len56(addr)
  |            57 : string^.intern := AOC.Len57(addr)
  |            58 : string^.intern := AOC.Len58(addr)
  |            59 : string^.intern := AOC.Len59(addr)
  |            60 : string^.intern := AOC.Len60(addr)
  |            61 : string^.intern := AOC.Len61(addr)
  |            62 : string^.intern := AOC.Len62(addr)
  |            63 : string^.intern := AOC.Len63(addr)
  |            64 : string^.intern := AOC.Len64(addr)
  |            65 : string^.intern := AOC.Len65(addr)
  |            66 : string^.intern := AOC.Len66(addr)
  |            67 : string^.intern := AOC.Len67(addr)
  |            68 : string^.intern := AOC.Len68(addr)
  |            69 : string^.intern := AOC.Len69(addr)
  |            70 : string^.intern := AOC.Len70(addr)
  |            71 : string^.intern := AOC.Len71(addr)
  |            72 : string^.intern := AOC.Len72(addr)
  |            73 : string^.intern := AOC.Len73(addr)
  |            74 : string^.intern := AOC.Len74(addr)
  |            75 : string^.intern := AOC.Len75(addr)
  |            76 : string^.intern := AOC.Len76(addr)
  |            77 : string^.intern := AOC.Len77(addr)
  |            78 : string^.intern := AOC.Len78(addr)
  |            79 : string^.intern := AOC.Len79(addr)
  |            80 : string^.intern := AOC.Len80(addr)
  |    81 ..   96 : string^.intern := AOC.Len96(addr)
  |    97 ..  112 : string^.intern := AOC.Len112(addr)
  |   113 ..  128 : string^.intern := AOC.Len128(addr)
  |   129 ..  256 : string^.intern := AOC.Len256(addr)
  |   257 ..  384 : string^.intern := AOC.Len384(addr)
  |   385 ..  512 : string^.intern := AOC.Len512(addr)
  |   513 ..  768 : string^.intern := AOC.Len768(addr)
  |   769 .. 1024 : string^.intern := AOC.Len1024(addr)
  |  1025 .. 1280 : string^.intern := AOC.Len1280(addr)
  |  1281 .. 1792 : string^.intern := AOC.Len1792(addr)
  |  1793 .. 2048 : string^.intern := AOC.Len2048(addr)
  |  2049 .. 2304 : string^.intern := AOC.Len2304(addr)
  |  2305 .. 2560 : string^.intern := AOC.Len2560(addr)
  |  2561 .. 2816 : string^.intern := AOC.Len2816(addr)
  |  2817 .. 3072 : string^.intern := AOC.Len3072(addr)
  |  3073 .. 3328 : string^.intern := AOC.Len3328(addr)
  |  3329 .. 3584 : string^.intern := AOC.Len3584(addr)
  |  3585 .. 3840 : string^.intern := AOC.Len3840(addr)
  |  3841 .. 4096 : string^.intern := AOC.Len4096(addr)
  END (* CASE *)
END NewStrWithArray;


(* TO DO : PROCEDURE NewStrWithArraySlice() *)


(* ---------------------------------------------------------------------------
 * procedure NewIntern(addr, array, size)
 * ---------------------------------------------------------------------------
 * Allocates a new intern, initialises it with the contents of array, and
 * passes back the actual size of the array's payload in out-parameter size.
 * ------------------------------------------------------------------------ *)

PROCEDURE NewIntern
  ( VAR addr : ADDRESS; VAR array : ARRAY OF CHAR; VAR size : CARDINAL );

TYPE
  Passepartout = POINTER TO AOC.Largest;

VAR
  size : CARDINAL;
  ptr : Passepartout; (* for casting only *)

BEGIN
  (* get actual size of array payload *)
  size := 0;
  WHILE (size <= HIGH(array)) AND (array[size] # ASCII.NUL) DO
    size := size + 1
  END; (* WHILE *)
  
  (* allocate space for intern *)
  CASE size OF
  |     1 ..   80 : ALLOCATE(addr, size);
  |    81 ..   96 : ALLOCATE(addr, 96)
  |    97 ..  112 : ALLOCATE(addr, 112)
  |   113 ..  128 : ALLOCATE(addr, 128)
  |   129 ..  256 : ALLOCATE(addr, 256)
  |   257 ..  384 : ALLOCATE(addr, 384)
  |   385 ..  512 : ALLOCATE(addr, 512)
  |   513 ..  768 : ALLOCATE(addr, 768)
  |   769 .. 1024 : ALLOCATE(addr, 1024)
  |  1025 .. 1280 : ALLOCATE(addr, 1280)
  |  1281 .. 1792 : ALLOCATE(addr, 1792)
  |  1793 .. 2048 : ALLOCATE(addr, 2048)
  |  2049 .. 2304 : ALLOCATE(addr, 2304)
  |  2305 .. 2560 : ALLOCATE(addr, 2560)
  |  2561 .. 2816 : ALLOCATE(addr, 2816)
  |  2817 .. 3072 : ALLOCATE(addr, 3072)
  |  3073 .. 3328 : ALLOCATE(addr, 3328)
  |  3329 .. 3584 : ALLOCATE(addr, 3584)
  |  3585 .. 3840 : ALLOCATE(addr, 3840)
  |  3841 .. 4096 : ALLOCATE(addr, 4096)
  END; (* CASE *)
  
  (* cast to largest possible AOC pointer *)
  ptr := Passepartout(addr);
  
  (* initialise with array *)
  FOR index := 0 TO size DO
    ptr^[index] := array[index]
  END (* FOR *)
END NewIntern;


(* Table Operations *)

(* ---------------------------------------------------------------------------
 * procedure InitTable
 * ---------------------------------------------------------------------------
 * Initialises the global string table.
 * ------------------------------------------------------------------------ *)

PROCEDURE InitTable;

VAR
  index : CARDINAL;

BEGIN
  (* initialise entry count *)
  strTable.count := 0;
  
  (* initialise table buckets *)
  FOR index := 0 TO BucketCount - 1 DO
    strTable[index] := NIL
  END (* FOR *)
END InitTable;


(* ---------------------------------------------------------------------------
 * function lookupOrInsert(array, start, end)
 * ---------------------------------------------------------------------------
 * Looks up array in the global string table. Returns its interned string if
 * found. Creates, inserts and returns new interned string if not found.
 * ------------------------------------------------------------------------ *)

PROCEDURE lookupOrInsert
  ( VAR array : ARRAY OF CHAR; start, end : CARDINAL ) : String;

VAR
  hash : Hash.Key;
  bucketIndex : CARDINAL;
  thisEntry, newEntry : TableEntry;
  
BEGIN
  hash := Hash.valueForArray(array);
  bucketIndex := hash MOD BucketCount;
  IF bucket[bucketIndex] = NIL THEN
    newEntry := NewTableEntry(hash, array, start, end);
    bucket[bucketIndex] := newEntry;
    RETURN newEntry^.string
    
  ELSE (* bucket not empty *)
    thisEntry := bucket[bucketIndex];
    LOOP
      (* check for matching entry *)
      IF (hash = thisEntry^.hash) AND
        matchesArraySlice(thisEntry^.string, array, start, end) THEN
      (* match -- return entry's string *)
        RETURN thisEntry^.string
      END; (* IF *)
      
      (* no match -- move to next entry *)
      IF thisEntry^.next # NIL
        thisEntry := thisEntry^.next
      ELSE (* no more entries -- exit *)
        EXIT
      END (* IF *)
    END (* LOOP *) thisEntry^.next = NIL;
    
    (* no matching entry found -- insert new entry *)
    newEntry := NewTableEntry(hash, array, start, end);
    thisEntry^.next := newEntry;
    RETURN newEntry^.string
  END (* IF *)
END lookupOrInsert;


(* Table Entry Operations *)

(* ---------------------------------------------------------------------------
 * procedure NewTableEntry(hash, array, start, end)
 * ---------------------------------------------------------------------------
 * Creates and initalises a new table entry.
 * ------------------------------------------------------------------------ *)

PROCEDURE NewTableEntry
  ( hash : Hash.Key;
    VAR array : ARRAY OF CHAR;
    start, end : CARDINAL ) : TableEntry;

VAR
  string : Str
  entry : TableEntry;
  
BEGIN
  ALLOCATE(entry, SYSTEM.TSIZE(TableEntry));
  entry^.hash := hash;
  NewStrWithArraySlice(string, array, start, end);
  entry^.string := string;
  entry^.next := NIL;
  RETURN entry
END NewTableEntry;


BEGIN (* String *)
  InitTable
END String.
