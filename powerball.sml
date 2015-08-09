
structure DB =
struct

  exception DB of string
  structure PU = PowerUtil

  val chars = Vector.map Board.forgetlegal Board.legalchars
  val radix = Vector.length chars

  val l = DeBruijnSequence.debruijn (3, radix)
  val s : string = CharVector.fromList (map (fn idx => Vector.sub(chars, idx)) l)

  val problems = PowerUtil.problems ()

  val padding = "hog"
  fun longest_prefix_with_padding solution =
    let
      val (n, i, s) = PU.longest_prefix problems solution
      val (nn, ii, ss) = PU.longest_prefix problems (padding ^ solution)
    in
      if nn - size padding > n
      then (nn - size padding, ii, ss, true)
      else (n, i, s, false)
    end


  val big = ref nil
  val covered = ref 0
  val wasted = ref 0
  fun consume "" = print "Done.\n"
    | consume (s : string) =
    let
      val (n, idx, seed, padded) = longest_prefix_with_padding s
    in
      if n > 0
      then
        let in
          print ("[" ^ Int.toString (String.size s) ^ " left] " ^
                 "first " ^ Int.toString n ^ " chars on " ^
                 "problem " ^ Int.toString idx ^ " seed " ^
                 Int.toString (Word32.toInt seed) ^
                 (if padded then " (padded)" else "") ^
                     "\n");
          (if (n > 100)
           then
             let in
               big := (String.substring (s, 0, n), idx, seed, padded) :: !big;
               covered := !covered + n
             end
           else wasted := !wasted + n);
          consume (String.substring (s, n, String.size s - n))
        end
      else
        let in
          print ("Had to skip one char [" ^ String.substring(s, 0, 1) ^
                 "] :(\n");
          wasted := !wasted + 1;
          consume (String.substring (s, 1, String.size s - 1))
        end
    end

  val () = consume s
  val () = print ("Covered " ^ Int.toString (!covered) ^ " chars and wasted " ^
                  Int.toString (!wasted) ^ ".\n" ^
                  "There are " ^ Int.toString (length (!big)) ^ " big strings.\n")

  val big = ListUtil.sort (fn ((s, _, _, _), (ss, _, _, _)) =>
                           Int.compare (size ss, size s)) (!big)

  val f = TextIO.openOut "db3.txt"
  (* Ridiculous! Since we are targeting 'quoted' output, turn a single
     quote into ' (ending the quote) "'" (quoted quote) ' (restart quotes).
     Of course, quote all that for SML string literals. *)
  fun escape s = StringUtil.replace "'" "'\"'\"'" s
  fun onebig ((s, problem, seed, padded), i) =
    let val actual = if padded then padding ^ s
                     else s
    in
      TextIO.output (f, "./submitty.py --prob " ^ Int.toString problem ^
                     " --seed " ^ Int.toString (Word32.toInt seed) ^
                     " --tag db3_" ^ Int.toString i ^
                     " --sol '" ^ escape actual ^ "'\n")
    end
  val () = ListUtil.appi onebig big
  val () = TextIO.closeOut f
end
