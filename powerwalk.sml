structure PowerWalk =
struct
  (* compute the score for the given solution on the given problem/seed
     assuming the given phrases of power *)
  fun getScore problem seed solution phrases =
    let val problem = Board.setPower (problem, Vector.fromList phrases)
        val state = Board.resetwithseed (problem, seed)
        val solution_size = String.size solution
        fun scoreFrom i score_so_far =
          if i >= solution_size
          then score_so_far
          else
            let val c = Board.legalize (String.sub (solution, i))
                (* XXX track total phrases of power encountered, for sanity check *)
                val Board.M { scored, status, ... } = Board.move (state, c)
                val score = score_so_far + scored
            in
              case status of
                Board.CONTINUE => scoreFrom (i + 1) score
              | Board.GAMEOVER why => score
              | Board.ERROR => 0
            end
    in
      scoreFrom 0 0
    end

  structure StringSet = SplaySetFn (struct type ord_key = string val compare = String.compare end)

  (* computes a list of substrings of given length of the given solution would,
     if considered as phrases of power, would yield the given target score. *)
  fun candidates length problem seed solution score =
    let val solution_size = String.size solution
        fun candidatesFrom i seen =
          if solution_size - i < length
          then []
          else
            let val c = String.substring (solution, i, length)
                val cs = candidatesFrom (i + 1) (StringSet.add (seen, c))
            in
              if not (StringSet.member (seen, c)) andalso
                 (print ("trying " ^ c ^ "...\n");
                  getScore problem seed solution (c :: Phrases.power) = score)
              then c :: cs
              else cs
            end
    in
      ListUtil.sort_unique (String.compare) (candidatesFrom 0 StringSet.empty)
    end


  val seedp = Params.param "0" (SOME ("-seed", "Seed value to use")) "seed"
  val problemp = Params.param "11" (SOME ("-problem", "Problem number to load")) "problem"
  val scriptp = Params.param "" (SOME ("-script", "Command sequence to run")) "script"
  val scorep = Params.param "0" (SOME ("-score", "Expected score from leaderboard")) "score"

  (* or: *)
  (* XXX doesn't work yet *)
  val filep = Params.param "" (SOME ("-file", "A file containing a JSON solution")) "file"

  fun main args =
    let val problem = Board.fromjson (StringUtil.readfile ("qualifiers/problem_" ^ !problemp ^ ".json"))
        val seed = Word32.fromInt (Params.asint 0 seedp)
        val solution = !scriptp
        val score = Params.asint 0 scorep
        (* XXX 3?  for testing, for now.. or make it iterate on 3..51 -wjl *)
        val candidates = candidates 39 problem seed solution score
    in
      print "candidate words:\n";
      List.app (fn s => print ("  " ^ s ^ "\n")) candidates
    end
end

val () = Params.main0 "This program takes flags.  See source." PowerWalk.main
