(*

PowerWalk attempts to find possible phrases of power in solutions that contain
exactly one unknown mystery phrase.  Given a solution to a problem/seed and a
purported score that we expect to get but aren't for that solution (from the
Leaderboard, e.g.), it attempts to find candidate power phrases as substrings
of the given solution that would allow us to achieve the expected score if we
knew they were power phrases.

It considers every possible substring of the solution between size 3 and 51
(known possible lengths of power phrases) that could potentially yield the
expected score.  As an optimization, it can ignore many lengths: subtracting
out our known portion of the score and the 300 bonus points for a new phrase of
power, the resulting delta must be divisible by the length of the power phrase.

   [ From the spec:

    power_score_p = 2 * len_p * reps_p + power_bonusp
    where
    power_bonusp = if reps_p > 0
                   then 300
                   else 0


   Since the delta must be entirely attributable to the new power word, we have:

    expected_score - our_score = 2 * len_p * reps_p + 300

   And thus we may discard any lengths which do not evenly divide

    (expected_score - our_score - 300) / 2 ]
*)

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
    let val () = print ("finding candidates of length " ^ Int.toString length ^ "...\n")
        fun candidatesFrom i seen candidates_so_far =
          if String.size solution - i < length
          then candidates_so_far
          else
            let val c = String.substring (solution, i, length)
                val candidates =
                  if not (StringSet.member (seen, c)) andalso
                     getScore problem seed solution (c :: Phrases.power) = score
                  then c :: candidates_so_far
                  else candidates_so_far
            in
              candidatesFrom (i + 1) (StringSet.add (seen, c)) candidates
            end
    in
      ListUtil.sort_unique String.compare (candidatesFrom 0 StringSet.empty [])
    end


  val seedp = Params.param "0" (SOME ("-seed", "Seed value to use")) "seed"
  val problemp = Params.param "11" (SOME ("-problem", "Problem number to load")) "problem"
  val scriptp = Params.param "" (SOME ("-script", "Command sequence to run")) "script"
  val scorep = Params.param "0" (SOME ("-score", "Expected score from leaderboard")) "score"

  (* or: *)
  (* XXX doesn't work yet, but should be "easy" to implement -- getscore.sml has some
     code for reading in a json solution and doing something sensible with it. *)
  val filep = Params.param "" (SOME ("-file", "A file containing a JSON solution")) "file"

  fun main args =
    let val problem = Board.fromjson (StringUtil.readfile ("qualifiers/problem_" ^ !problemp ^ ".json"))
        val seed = Word32.fromInt (Params.asint 0 seedp)
        val solution = !scriptp
        val score = Params.asint 0 scorep
        (* can a new power phrase of the given length possibly help?
           only if it evenly divides the delta power_score, discounting power_bonus. *)
        fun possibleLength len =
          let val our_score  = getScore problem seed solution Phrases.power
          in
            ((score - our_score - 300) div 2) mod len = 0
          end
        val candidates = List.concat (List.map (fn len => 
                                                  if possibleLength len
                                                  then candidates len problem seed solution score
                                                  else [])
                                               (List.tabulate (49, fn x => x + 3)))
    in
      print "candidate words:\n";
      List.app (fn s => print ("  " ^ s ^ "\n")) candidates
    end
end

val () = Params.main0 "This program takes flags.  See source." PowerWalk.main
