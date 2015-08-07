structure Board :> BOARD =
struct

  exception Board of string

  datatype dir = E | W | SE | SW
  datatype turn = CW | CCW
  datatype command = D of dir | T of turn
  datatype reason =
      COMPLETE
    | NO_SPACE
    | ERROR
  datatype moveresult =
      Continue of { scored: int, lines: int, locked: bool }
    | Done of { reason: reason }

  type legalchar = char

  (* Vector of members (filled spaces), assuming the pivot is at 0,0 *)
  type piece =
    (int * int) Vector.vector

  datatype problem =
    P of { start: bool vector,
           width: int,
           height : int,
           sourcelength : int,
           seeds : Word32.word vector,
           (* aka units, an ml keyword *)
           pieces : piece vector
          }

  datatype state =
    S of { board: bool array,
           problem: problem,
           score: int ref,
           (* position of pivot *)
           x: int ref,
           y: int ref,
           rng: RNG.rng ref }

  fun clone_array a =
    (* PERF There must be a faster way to do this?? *)
    Array.tabulate (Array.length a, fn i => Array.sub(a, i))

  fun clone (S { board, problem, score, x, y, rng }) : state =
           S { board = clone_array board,
               problem = problem,
               score = ref (!score),
               x = ref (!x),
               y = ref (!y),
               rng = ref (!rng) }

  fun isfull (S { board, problem = P{ width, height, ... }, ... },
              x, y) =
    x < 0 orelse y < 0 orelse x >= width orelse y >= height orelse
    Array.sub (board, y * width + x)
  fun isempty (state, x, y) = not (isfull (state, x, y))

  fun move _ = raise Board "unimplemented"
  fun move_undo _ = raise Board "unimplemented"
  fun move_unwind _ = raise Board "unimplemented"
  fun reset _ = raise Board "unimplemented"

  fun dirstring E = "E"
    | dirstring W = "W"
    | dirstring SE = "SE"
    | dirstring SW = "SW"
  fun dirorder (E, E) = EQUAL
    | dirorder (E, _) = LESS
    | dirorder (_, E) = GREATER
    | dirorder (W, W) = EQUAL
    | dirorder (W, _) = LESS
    | dirorder (_, W) = GREATER
    | dirorder (SE, SE) = EQUAL
    | dirorder (SE, _) = LESS
    | dirorder (_, SE) = GREATER
    | dirorder (SW, SW) = EQUAL

  fun turnstring CW = "CW"
    | turnstring CCW = "CCW"

  fun turnorder (CW, CW) = EQUAL
    | turnorder (CW, _) = LESS
    | turnorder (_, CW) = GREATER
    | turnorder (CCW, CCW) = EQUAL

  fun commandstring (D d) = dirstring d
    | commandstring (T t) = turnstring t

  fun commandorder (D d, D dd) = dirorder (d, dd)
    | commandorder (D _, _) = LESS
    | commandorder (_, D _) = GREATER
    | commandorder (T t, T tt) = turnorder (t, tt)

  fun reasonstring _ = "sorry, unimplemented"

  (* XXX show everything *)
  fun moveresultstring (Continue {scored, lines, locked}) = "Continue..."
    | moveresultstring (Done {reason}) = "Done..."

  (* TODO: Use this beauty, but might need to use ansi backgrounds...
  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/
  /  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__
  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/
  /  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__
  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/
*)

  fun toascii (state as
               S { problem = P {width, height, ... },
                   board, x = px, y = py, ... }) =
    let
      (*
         . . . . .
          . . O O .
         . # . O .
          . # . . .

          *)

      fun oneline y =
        let val r = ref
          (if y mod 2 = 0
           then "" (* empty half *)
           else " ")
        in
          (* XXX also draw current piece. *)
          Util.for 0 (width - 1)
          (fn x =>
           r := !r ^
           (if isfull (state, x, y)
            then "# "
            else ". "));
          !r
        end

    in
      StringUtil.delimit "\n" (List.tabulate (height, oneline))
    end


  val dw   : legalchar vector = Vector.fromList (explode "p'!.03")
  val de   : legalchar vector = Vector.fromList (explode "bcefy2")
  val dsw  : legalchar vector = Vector.fromList (explode "aghij4")
  val dse  : legalchar vector = Vector.fromList (explode "lmno 5") (* ell *)
  val tcw  : legalchar vector = Vector.fromList (explode "dqrvz1") (* one *)
  val tccw : legalchar vector = Vector.fromList (explode "kstuwx")
  fun getchars (D W) = dw
    | getchars (D E) = de
    | getchars (D SW) = dsw
    | getchars (D SE) = dse
    | getchars (T CW) = tcw
    | getchars (T TCCW) = tccw

  fun anychar d = Vector.sub(getchars d, 0)

  fun charcommand #"p" = D W
    | charcommand #"'" = D W
    | charcommand #"!" = D W
    | charcommand #"." = D W
    | charcommand #"0" = D W
    | charcommand #"3" = D W

    | charcommand #"b" = D E
    | charcommand #"c" = D E
    | charcommand #"e" = D E
    | charcommand #"f" = D E
    | charcommand #"y" = D E
    | charcommand #"2" = D E

    | charcommand #"a" = D SW
    | charcommand #"g" = D SW
    | charcommand #"h" = D SW
    | charcommand #"i" = D SW
    | charcommand #"j" = D SW
    | charcommand #"4" = D SW

    | charcommand #"l" = D SE (* ell *)
    | charcommand #"m" = D SE
    | charcommand #"n" = D SE
    | charcommand #"o" = D SE
    | charcommand #" " = D SE
    | charcommand #"5" = D SE

    | charcommand #"d" = T CW
    | charcommand #"q" = T CW
    | charcommand #"r" = T CW
    | charcommand #"v" = T CW
    | charcommand #"z" = T CW
    | charcommand #"1" = T CW (* one *)

    | charcommand #"k" = T CCW
    | charcommand #"s" = T CCW
    | charcommand #"t" = T CCW
    | charcommand #"u" = T CCW
    | charcommand #"w" = T CCW
    | charcommand #"x" = T CCW

    | charcommand _ = raise Board "impossible (bad legalchar)"

  fun legalize (c : char) : legalchar =
    let in
      (* For side effect of raising exception *)
      ignore (charcommand c);
      c
    end

  fun size (P { width, height, ... }) = (width, height)
  fun width (P { width, ... }) = width
  fun height (P { height, ... }) = height
  fun pieces (P { pieces, ... }) = pieces

end
