
structure GetScore =
struct

  val file = Params.param ""
    (SOME("-file", "A file containing JSON input as sent to the server."))
    "file"

  val scriptp = Params.param ""
    (SOME("-script", "The command sequence (as characters) to run."))
    "script"

  val scriptfilep = Params.param ""
    (SOME("-scriptfile", "A file containing (only) the command sequence to run."))
    "scriptfile"

  val problemp = Params.param "11"
    (SOME("-problem", "Problem number to load."))
    "problem"

  val seedp = Params.param "0"
    (SOME("-seed", "Seed *value* to use. Not an index."))
    "seed"

  fun get_score problema scripta seeda =
    let
      val seed = Word32.fromInt seeda
      val problem = Board.fromjson
        (StringUtil.readfile ("qualifiers/problem_" ^ problema ^ ".json"))

      val state = Board.resetwithseed (problem, seed)

      val full_script = explode (scripta)
      fun replay { score, lines, phrases, script : char list } =
        case script of
          nil => { score = score, lines = lines, phrases = phrases,
                   leftover = 0, fate = "SCRIPT_EXHAUSTED" }
        | #"\n" :: rest =>
          replay { score = score, lines = lines, phrases = phrases,
                   script = rest }
        | #"\r" :: rest =>
          replay { score = score, lines = lines, phrases = phrases,
                   script = rest }
        | #"\t" :: rest =>
          replay { score = score, lines = lines, phrases = phrases,
                   script = rest }
        | c :: rest =>
            let
              val legalchar : Board.legalchar = Board.legalize c

              val Board.M { scored, lines = new_lines, locked = _,
                            new_phrases, status } =
                Board.move (state, legalchar)

              val score = score + scored
              val lines = lines + new_lines
              val phrases = phrases + new_phrases
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

    in
        replay { score = 0, lines = 0, phrases = 0, script = full_script }
        handle Board.Board s =>
          let in
            TextIO.output(TextIO.stdErr, "Board exn: " ^ s ^ "\n");
            { score = 0, lines = 0, phrases = 0, leftover = 0,
              fate = "EXCEPTION" }
          end
    end

  fun fromjson s =
    let
      datatype json = datatype JSONDatatypeCallbacks.json

      fun error_handle (msg,pos,data) =
          raise Fail ("Error: " ^ msg ^ " near " ^ Int.toString pos)

      val j =
          (JSON.inputData := s ;
           JSON.inputPosition := 0 ;
           JSON.parseArray ()) handle JSON.JSONParseError (m,p) =>
                                      error_handle (m,p,!JSON.inputData)

      fun entry e =
          (Int.toString (JSONUtils.Int (e, "problemId")),
           JSONUtils.String (e, "solution"),
           JSONUtils.Int (e, "seed"))
    in
        List.map entry (JSONUtils.UnList j)
        handle JSONUtils.JSONUtils s =>
               (TextIO.output (TextIO.stdErr,
                              "Uncaught JSON parse error: " ^ s ^ "\n");
                [])
    end

  fun main () =
    let
      fun jsonline (k, v) = "\"" ^ k ^ "\": " ^ v
      fun print_score { score, lines, phrases, leftover, fate } =
        ("{\n  " ^
         StringUtil.delimit ",\n  "
         (map jsonline
          [("score", Int.toString score),
           ("distinct_phrases", Int.toString phrases),
           ("fate", "\"" ^ fate ^ "\""),
           ("commands_left", Int.toString leftover)]) ^
         "\n}\n")
    in
        (if !file = "" then
             (if !scriptfilep = "" then
                  print (print_score (get_score (!problemp) (!scriptp)
                                                (Params.asint 0 seedp)))
              else
                  print (print_score (get_score
                                          (!problemp)
                                          (StringUtil.readfile (!scriptfilep))
                                          (Params.asint 0 seedp)))
             )
         else
             (print "[";
              print
                  (String.concatWith ",\n"
                  (List.map
                      (fn (p, sc, sd) => print_score (get_score p sc sd))
                      (fromjson (StringUtil.readfile (!file)))));
             print "]\n")
        ) (*; (* This next line can't be right. -rjs *)
        print "]" *)
    end
end

val () = Params.main0
  ("This program takes no arguments (but uses -flags). It prints out a simple " ^
   "JSON object with statistics about the -script executed on the -problem " ^
   "number with the given -seed value. Information about phrases of power, " ^
   "including their impact on the score, is only relative to the phrases " ^
   "known in phrases.sml.")
  GetScore.main
