
structure GetScore =
struct

  val scriptp = Params.param ""
    (SOME("-script", "The command sequence (as characters) to run."))
    "script"

  val problemp = Params.param "11"
    (SOME("-problem", "Problem number to load."))
    "problem"

  val seedp = Params.param "0"
    (SOME("-seed", "Seed *value* to use. Not an index."))
    "seed"

  fun main () =
    let
      val seed = Word32.fromInt (Params.asint 0 seedp)
      val problem = Board.fromjson
        (StringUtil.readfile ("qualifiers/problem_" ^ !problemp ^ ".json"))

      val state = Board.resetwithseed (problem, seed)

      val full_script = explode (!scriptp)
      fun replay { score, lines, phrases, script : char list } =
        case script of
          nil => { score = score, lines = lines, phrases = phrases,
                   leftover = 0,
                   fate = "SCRIPT_EXHAUSTED" }
        | c :: rest =>
            let
              val legalchar : Board.legalchar = Board.legalize c

              val Board.M { scored, lines = newlines, locked = _, status } =
                Board.move (state, legalchar)

              val score = score + scored
              val lines = lines + newlines
              val phrases = phrases (* XXX *)
              datatype status = datatype Board.status
            in
              case status of
                CONTINUE => replay { score = score, lines = lines,
                                     phrases = phrases, script = rest }
              | GAMEOVER why => { score = score,
                                  lines = lines,
                                  phrases = phrases,
                                  leftover = length rest,
                                  fate = (case why of
                                            Board.COMPLETE => "COMPLETE"
                                          | Board.NO_SPACE => "NO_SPACE") }
              | ERROR => { score = 0,
                           lines = lines,
                           phrases = phrases,
                           leftover = length rest,
                           fate = "ERROR_STUTTER" }
            end

      val { score, lines, phrases, leftover, fate } =
        replay { score = 0, lines = 0, phrases = 0, script = full_script }
        handle Board.Board s =>
          let in
            TextIO.output(TextIO.stdErr, "Board exn: " ^ s ^ "\n");
            { score = 0, lines = 0, phrases = 0, leftover = 0,
              fate = "EXCEPTION" }
          end

      fun jsonline (k, v) = "\"" ^ k ^ "\": " ^ v
    in
      print
      ("{\n  " ^
       StringUtil.delimit ",\n  "
       (map jsonline
        [("score", Int.toString score),
         ("distinct_phrases", Int.toString phrases),
         ("fate", "\"" ^ fate ^ "\""),
         ("commands_left", Int.toString leftover)]) ^
       "\n}\n")
    end

end

val () = Params.main0
  ("This program takes no arguments (but uses -flags). It prints out a simple " ^
   "JSON object with statistics about the -script executed on the -problem " ^
   "number with the given -seed value. Information about phrases of power, " ^
   "including their impact on the score, is only relative to the phrases " ^
   "known in phrases.sml.")
  GetScore.main
