
structure DB =
struct

  exception DB of string

  val chars = Vector.map Board.forgetlegal Board.legalchars
  val radix = Vector.length chars

  val l = DeBruijnSequence.debruijn (3, radix)
  val s : string = CharVector.fromList (map (fn idx => Vector.sub(chars, idx)) l)

  fun loadproblem p = Board.fromjson
    (StringUtil.readfile ("qualifiers/problem_" ^ Int.toString p ^ ".json"))

  val () = print "loading problems...\n"
  val problems = Vector.tabulate(25, loadproblem)
  val () = print "ok.\n"

  (* Return the longest prefix that does not result in ERROR; can be 0 *)
  fun get_valid_prefix problem seed solution =
    let
      val state = Board.resetwithseed (problem, seed)
      val solution_size = String.size solution
      fun score_from i =
        if i = solution_size
        then i
        else
          let
            val c = Board.legalize (String.sub (solution, i))
            val Board.M { status, ... } = Board.move (state, c)
          in
            case status of
              Board.CONTINUE => score_from (i + 1)
            | Board.GAMEOVER _ => i
            | Board.ERROR => i
          end
  in
    score_from 0
  end

  fun longest_prefix solution =
    let
      val best = ref NONE
      fun oneproblem (idx, p) =
        let
          val seeds = Board.seeds p
          fun oneseed s =
            let
              val n = get_valid_prefix p s solution
            in
              if (case !best of
                    NONE => true
                  | SOME (x, _, _) => n > x)
              then best := SOME (n, idx, s)
              else ()
            end
        in
          Vector.app oneseed seeds
        end
    in
      Vector.appi oneproblem problems;
      case !best of
        NONE => raise DB "There is at least one problem, right?"
      | SOME (n, idx, s) => (n, idx, s)
    end

  val padding = "hog"
  fun longest_prefix_with_padding solution =
    let
      val (n, i, s) = longest_prefix solution
      val (nn, ii, ss) = longest_prefix (padding ^ solution)
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

  val f = TextIO.openOut "db3.txt"
  (* Ridiculous! Since we are targeting 'quoted' output, turn a single
     quote into ' (ending the quote) "'" (quoted quote) ' (restart quotes).
     Of course, quote all that for SML string literals. *)
  fun escape s = StringUtil.replace "'" "'\"'\"'" s
  fun onebig ((s, problem, seed, padded), i) =
    let val actual = if padded then padding ^ s
                     else s
    in
      TextIO.output (f, "./submitty.py -prob " ^ Int.toString problem ^
                     " -seed " ^ Int.toString (Word32.toInt seed) ^
                     " -db3_" ^ Int.toString i ^
                     " -sol '" ^ escape actual ^ "'\n")
    end
  val () = ListUtil.appi onebig (!big)
  val () = TextIO.closeOut f
end
