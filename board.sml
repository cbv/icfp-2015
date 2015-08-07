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

  datatype problem =
    P of { start: bool Vector.vector,
           width: int,
           height : int
           (* ... *)
          }

  (* Assuming the origin is at 0,0 *)
  type member =
    (int * int) Vector.vector
  (* XXX *)
  type state = unit

  fun isempty _ = raise Board "unimplemented"
  fun isfull _ = raise Board "unimplemented"
  fun members _ = raise Board "unimplemented"
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

  fun toascii _ = "sorry, unimplemented"

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

  fun size (P {width, height, ...}) = (width, height)
  fun width (P {width, ...}) = width
  fun height (P {height, ...}) = height

end
