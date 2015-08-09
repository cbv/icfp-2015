structure JSONUtils =
struct
datatype json = datatype JSONDatatypeCallbacks.json

exception JSONUtils of string

fun UnInt (JInt i) = i
  | UnInt _ = raise JSONUtils "Not an int"

fun UnString (JStr s) = s
  | UnString _ = raise JSONUtils "Not a string"

fun UnList (JArray a) = a
  | UnList _ = raise JSONUtils "Not a list/array"

fun Key (JMap m, k) =
    (case ListUtil.Alist.find op= m k of
         SOME (obj) => obj
       | NONE => raise JSONUtils ("Didn't find " ^ k))
  | Key _ = raise JSONUtils "Not JMap"

fun Int (j, key) = UnInt (Key (j, key))

fun String (j, key) = UnString (Key (j, key))

fun List (j, key) = UnList (Key (j, key))

fun Coord j = (Int (j, "x"), Int (j, "y"))

end
