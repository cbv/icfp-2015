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
  (* XXX this should have all the rotations cached. *)
  type piece =
    (* Set of (spec) coordinate offsets from pivot *)
    (int * int) vector
    (* 6 of them, one for each rotation. 0 is the natural rotation,
       1 is 1 hexgree clockwise, etc. *)
    vector

  (* PERF: Should sort vector and do binary search, etc. *)
  fun oriented_piece_has (v : (int * int) vector) (x, y) =
    Vector.exists (fn (xx, yy) => x = xx andalso y = yy) v

  datatype problem =
    P of { start: bool vector,
           width: int,
           height : int,
           sourcelength : int,
           seeds : Word32.word vector,
           (* aka units, an ml keyword *)
           pieces : piece vector }

  datatype state =
    S of { board: bool array,
           problem: problem,
           score: int ref,

           piece: piece,

           (* Position of pivot (spec coords) *)
           x: int ref,
           y: int ref,

           (* Angle in hexgrees [0, 5] *)
           a: int ref,

           (* XXX history of where we've been *)
           rng: RNG.rng ref }

  fun uniformize_coord (x, y) =
    (x - y div 2, y)

  fun deuniformize_coord (x, y) =
    (x + y div 2, y)

  (* Correctly translate a point (x, y) by (dx, dy) where (dx, dy) are
     "east" and "south-east". This is not just pointwise addition
     because the grid is ragged; "down" moves alternatingly SE and
     SW in "spec" coordinates. *)
  fun translate (dx, dy) (x, y) =
    let
      val (ux, uy) = uniformize_coord (x, y)
      val ux = ux + dx
      val uy = uy + dy
    in
      deuniformize_coord (ux, uy)
    end

  (* Rotate about the origin. We won't rotate any other way, because
     what we actually do is compute the 6 rotations of the piece
     at the beginning, and then only translate.

     (x, y) are in uniform coordinates.
     n is the clockwise angle in hexgrees. *)
  fun rotate_uniform n (ux, uy) =
    let
      val uxx = ref ux
      val uyy = ref uy
    in
      Util.for 0 (n - 1)
      (fn _ =>
       let in
         uxx := ~ (!uyy);
         uyy := (!uxx + !uyy)
       end);
      (!uxx, !uyy)
    end

  fun rotate n (x, y) =
    let
      val (ux, uy) = uniformize_coord (x, y)
      val (ux, uy) = rotate_uniform n (ux, uy)
    in
      deuniformize_coord (ux, uy)
    end

  fun fromjson s =
    let
      datatype json = datatype JSONDatatypeCallbacks.json

      val j = JSON.parse s

      fun UnInt (JInt i) = i
        | UnInt _ = raise Board "Not an int"

      fun UnList (JArray a) = a
        | UnList _ = raise Board "Not a list/array"

      fun Key (JMap m, k) =
        (case ListUtil.Alist.find op= m k of
           SOME (obj) => obj
         | NONE => raise Board ("Didn't find " ^ k))
        | Key _ = raise Board "Not JMap"

      fun Int (j, key) = UnInt (Key (j, key))

      fun List (j, key) = UnList (Key (j, key))

      fun Coord j = (Int (j, "x"), Int (j, "y"))

      val width = Int (j, "width")
      val height = Int (j, "height")
      val id = Int (j, "id")
      val filled = map Coord (List (j, "filled"))
      val sourcelength = Int (j, "sourceLength")
      (* TODO: Harden against these being bigger than 2^31, negative, etc. *)
      val seeds = map UnInt (List (j, "sourceSeeds"))

      fun Piece j =
        let
          val members = map Coord (List (j, "members"))
          (* We represent all pieces as being centered on their origin.
             So we start by translating all members so that the pivot ends
             up on the origin. *)
          val (px, py) = uniformize_coord (Coord (Key (j, "pivot")))
        in
          Vector.fromList (map (translate (~px, ~py)) members)
        end

      (* Take a single piece (relative to origin) in its natural rotation.
         Return a 6-place vector of all its rotations *)
      fun makerotations p =
        (* PERF; can iteratively rotate *)
        Vector.tabulate (6, fn a => Vector.map (rotate a) p)

      val a = Array.array (width * height, false)
    in
      app (fn (x, y) =>
           Array.update (a, y * width + x, true)) filled;
      P { width = width, height = height,
          seeds = Vector.fromList (map Word32.fromInt seeds),
          sourcelength = sourcelength,
          start = Array.vector a,
          pieces = Vector.fromList
          (map makerotations
           (map Piece (List (j, "units")))) }
    end

  fun clone_array a =
    (* PERF There must be a faster way to do this?? *)
    Array.tabulate (Array.length a, fn i => Array.sub(a, i))

  fun clone (S { board, problem, score, rng, piece, a, x, y }) : state =
           S { board = clone_array board,
               problem = problem,
               score = ref (!score),
               a = ref (!a),
               x = ref (!x),
               y = ref (!y),
               piece = piece,
               (* piece = clone_array piece, *)
               rng = ref (!rng) }

  fun isfull (S { board, problem = P{ width, height, ... }, ... },
              x, y) =
    x < 0 orelse y < 0 orelse x >= width orelse y >= height orelse
    Array.sub (board, y * width + x)
  fun isempty (state, x, y) = not (isfull (state, x, y))

  fun reset (problem as P { seeds, start, pieces, ... },
             seed_idx) : state =
    let
      val rng =
        if seed_idx < 0 orelse
           seed_idx >= Vector.length seeds
        then raise Board "Bad seed idx in reset"
        else RNG.fromseed (Vector.sub (seeds, seed_idx))

      val (piece_idx, rng) = RNG.next rng

      val piece = Vector.sub (pieces, piece_idx)

      (* FIXME *)
      val startx = 0
      val starty = 0
      (* should start in this orientation. *)
      val starta = 0
    in
      S { rng = ref rng,
          score = ref 0,
          piece = piece,
          problem = problem,
          x = ref startx,
          y = ref starty,
          a = ref starta,
          board = Array.tabulate
          (Vector.length start,
           (fn x => Vector.sub(start, x))) }
    end

  fun move_undo (S { ... }, ch) =
    let in
      { result = raise Board "unimplemented",
        undo = raise Board "unimplemented" }
    end

  fun move (s, c) = #result (move_undo (s, c))

  fun move_unwind (s, c, k) =
    let
      val {result, undo} = move_undo (s, c)
      val r = k result
    in
      undo ();
      r
    end


  fun ispiece (S {x, y, piece, a, ...}, xx, yy) =
    let
      (* translate (xx, yy) to piece space, and look it up
         within the current rotation. *)
      val (px, py) = translate (~ (!x), ~ (!y)) (xx, yy)

      val oriented_piece = Vector.sub (piece, !a)
    in
      oriented_piece_has oriented_piece (px, py)
    end

  fun ispivot (S {x, y, ...}, xx, yy) = xx = !x andalso yy = !y
  fun pivot (S { x, y, ... }) = (!x, !y)


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

     WRONG NOT BEAUTIFUL!!!
      \__/  \__/  \__/  \__/  \__/  \__/
      /  \__/  \__/  \__/  \__/  \__/  \__
      \__/  \__/  \__/  \__/  \__/  \__/
      /  \__/  \__/  \__/  \__/  \__/  \__
      \__/  \__/  \__/  \__/  \__/  \__/      *)

  fun toascii (state as
               S { problem = P {width, height, ... },
                   board, (* x = px, y = py, *) ... }) =
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
            else (case (ispiece (state, x, y), ispivot (state, x, y)) of
                    (false, false) => ". "
                  | (false, true) => "a "
                  | (true, false) => "O "
                  | (true, true) => "@ ")));
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
