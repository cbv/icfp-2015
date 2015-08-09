structure JSONUtils =
struct
datatype json = datatype JSON.value

exception JSONUtils of string

fun UnInt (INT i) = IntInf.toInt i
  | UnInt _ = raise JSONUtils "Not an int"

fun UnString (STRING s) = s
  | UnString _ = raise JSONUtils "Not a string"

fun UnList (ARRAY a) = a
  | UnList _ = raise JSONUtils "Not a list/array"

fun Key (OBJECT m, k) =
    (case ListUtil.Alist.find op= m k of
         SOME (obj) => obj
       | NONE => raise JSONUtils ("Didn't find " ^ k))
  | Key _ = raise JSONUtils "Not JMap"

fun Int (j, key) = UnInt (Key (j, key))

fun String (j, key) = UnString (Key (j, key))

fun List (j, key) = UnList (Key (j, key))

fun Coord j = (Int (j, "x"), Int (j, "y"))

end
