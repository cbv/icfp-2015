structure Board :> BOARD =
struct

  exception Board of string

  datatype dir = E | W | SE | SW
  datatype turn = CW | CCW
  datatype command = D of dir | T of turn
  datatype why =
  (* Gracefully used all pieces *)
    COMPLETE
  (* Last move locked a piece, but there's no space
     to place the next one. *)
  | NO_SPACE
  datatype status =
    (* Game keeps going *)
    CONTINUE
  | GAMEOVER of why
  (* Bad command? *)
  | ERROR

  datatype moveresult =
    M of { scored: int, lines: int, locked: (int * int * int) option,
           status: status }

  type legalchar = char

  (* Vector of members (filled spaces), assuming the pivot is at 0,0 *)
  datatype piece =
    Piece of { rotations:
               (* Set of (spec) coordinate offsets from pivot *)
               (int * int) vector
               (* 6 of them, one for each rotation. 0 is the natural rotation,
                  1 is 1 hexgree clockwise, etc. *)
               vector,

               (* see get_symmetry *)
               symmetry : int,

               (* Start position for pivot in spec coords. *)
               start: int * int }

  (* PERF: Should sort vector and do binary search, etc.
     Currently only used in printing the board, though. *)
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


  (* This is a place that we've been in the past; we need to keep a set
     of these so that we don't ever repeat a configuration. Contest
     people have confirmed that the identity of the pivot matters, and
     the identity of the members does not. Since we have already
     pre-computed the symmetry group, it suffices to store here just the
     position of the pivot and the angle modulo the symmetry size. *)
  (* x, y, angle mod symmetry *)
  type stutter = int * int * int

  fun stutterorder ((x, y, a), (xx, yy, aa)) =
    case Int.compare (x, xx) of
      EQUAL => (case Int.compare (y, yy) of
                  EQUAL => Int.compare (a, aa)
                | other => other)
    | other => other

  structure StutterSet = SplaySetFn(type ord_key = stutter
                                     val compare = stutterorder)

  datatype state =
    S of { board: bool array,
           problem: problem,
           score: int ref,

           piece: piece ref,
           next_sourceidx: int ref,

           (* Position of pivot (spec coords) *)
           x: int ref,
           y: int ref,

           (* Angle in hexgrees [0, 5] *)
           a: int ref,

           (* Number of lines we made on the last step *)
           last_lines : int ref,

           (* XXX Need history of commands, for word scoring *)

           (* Places we have already been. *)
           stutters: StutterSet.set ref,

           valid: bool ref,

           rng: RNG.rng ref }

  fun piece_position (S {x, y, ...}) = (!x, !y)
  fun piece_angle (S {a, ...}) = !a
  fun piece_symmetry (S {piece = ref (Piece { symmetry, ... }), ...}) =
    symmetry

  fun delta E = (1, 0)
    | delta W = (~1, 0)
    | delta SE = (0, 1)
    | delta SW = (~1, 1)

  fun uniformize_coord (x, y) =
    (x - y div 2, y)

  fun deuniformize_coord (x, y) =
    (x + y div 2, y)

  (* Correctly translate a point (x, y) by (dx, dy) where (dx, dy) are
     "east" and "south-east". This is not just pointwise addition
     because the grid is ragged; "down" moves alternatingly SE and
     SW in "spec" coordinates. *)
  fun translate (udx, udy) (x, y) =
    let
      val (ux, uy) = uniformize_coord (x, y)
      val ux = ux + udx
      val uy = uy + udy
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
       let
         val prev_x = !uxx
       in
         uxx := ~ (!uyy);
         uyy := (prev_x + !uyy)
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

  fun coordorder ((x, y), (xx, yy)) =
    case Int.compare (x, xx) of
      EQUAL => Int.compare (y, yy)
    | other => other

  structure CoordSet = SplaySetFn(type ord_key = int * int
                                  val compare = coordorder)
  structure CoordMap = SplayMapFn(type ord_key = int * int
                                  val compare = coordorder)

  (* symmetry group; always 1, 2, 3, or 6:

     group 1: symmetric for any angle
      . . .
     . @ .
      . . .

     group 2:
      . O .     O . .
     O @ .     . @ O
      . O .     O . .

     group 3:
      . . .     O . .     . O .
     O @ O     . @ .     . @ .
      . . .     . O .     O . .

     group 6:
      . . .     . . .     . . .     . . .     O . .     . O .
     . @ O     . @ .     . @ .     O @ .     . @ .     . @ .
      . . .     . O .     O . .     . . .     . . .     . . .
     *)
  fun get_symmetry (v : (int * int) vector) =
    let
      val vs0 = Vector.foldr CoordSet.add' CoordSet.empty v

      val vs1 = CoordSet.map (rotate 1) vs0
    in
      if CoordSet.equal (vs0, vs1)
      then 1
      else
        let val vs2 = CoordSet.map (rotate 1) vs1
        in
          if CoordSet.equal (vs0, vs2)
          then 2
          else
            let val vs3 = CoordSet.map (rotate 1) vs2
            in
              if CoordSet.equal (vs0, vs3)
              then 3
              else 6
            end
        end
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

      fun ExpectPiece j =
        let
          val members = map Coord (List (j, "members"))
          (* We represent all pieces as being centered on their origin.
             So we start by translating all members so that the pivot ends
             up on the origin. *)
          val (px, py) = uniformize_coord (Coord (Key (j, "pivot")))
        in
          Vector.fromList (map (translate (~px, ~py)) members)
        end

      (* Get the position (in spec coords) of the pivot for where we
         should start this piece. (We pass in the piece in its natural
         orientation.) *)
      fun get_start_coord piece =
        let
          (* PERF can just act natively on vectors, though this
             code is only run at parse time *)
          local
            val piecel = Vector.foldr op:: nil piece
          in
            val (_, min_y) = ListUtil.min (ListUtil.bysecond Int.compare) piecel
          end

          (* Put its head on the 0th row. (Note we could just operate on y
             directly, but being hygeinic by explicitly transforming to
             uniform coordinate space...) *)
          val (head_udx, head_udy) = uniformize_coord (0, min_y)
          val piece = Vector.map (translate (head_udx, 0 - head_udy)) piece

          (* val (y0_pivotx, y0_pivoty) = translate (udx, ~udy) (0, 0) *)

          (* Now get spec coordinates for minimum and maximum x values when
             in this location. *)
          local
            val piecel = Vector.foldr op:: nil piece
            val (min_x, _) = ListUtil.min (ListUtil.byfirst Int.compare) piecel
            val (max_x, _) = ListUtil.max (ListUtil.byfirst Int.compare) piecel

            (* distance from left edge
               this was tom's way. jason put the kibosh
               *)
            (* val left_offset = min_x
               val right_offset = (width - 1) - max_x *)
          in
            (* jason's way. he is SURE about this.... buuuut nervous *)
            val borders = (width - max_x - min_x)
            val center_udx =
              if borders mod 2 = 0
              then borders div 2 - 1
              else (borders - 1) div 2
          end
        in
          (* Now we are translating the pivot by the two motions
             we computed above (move the head to the 0th row and then
             center it), which we can just compose with addition.
             We actually apply this to (0, 0) since what we want here
             are the spec coordinates of the pivot, even though it
             will actually just be the same as the displacement. *)
          translate (head_udx + center_udx, 0 - head_udy) (0, 0)
        end

      (* Take a single piece (relative to origin) in its natural rotation.
         Return a 6-place vector of all its rotations, plus the start
         position. *)
      fun makepiece p =
        (* PERF; can iteratively rotate *)
        let
          val rotations = Vector.tabulate (6, fn a => Vector.map (rotate a) p)
        in
          Piece { rotations = rotations,
                  symmetry = get_symmetry p,
                  start = get_start_coord p }
        end

      val a = Array.array (width * height, false)
    in
      app (fn (x, y) =>
           Array.update (a, y * width + x, true)) filled;
      P { width = width, height = height,
          seeds = Vector.fromList (map Word32.fromInt seeds),
          sourcelength = sourcelength,
          start = Array.vector a,
          pieces = Vector.fromList
          (map makepiece
           (map ExpectPiece (List (j, "units")))) }
    end

  fun clone_array a =
    (* PERF There must be a faster way to do this?? *)
    Array.tabulate (Array.length a, fn i => Array.sub(a, i))

  fun clone (S { board, next_sourceidx, last_lines, stutters,
                 valid, problem, score, rng, piece, a, x, y }) : state =
           S { board = clone_array board,
               problem = problem,
               score = ref (!score),
               next_sourceidx = ref (!next_sourceidx),
               stutters = ref (!stutters),
               a = ref (!a),
               x = ref (!x),
               y = ref (!y),
               valid = ref (!valid),
               last_lines = ref (!last_lines),
               piece = ref (!piece),
               rng = ref (!rng) }

  fun isfull (S { board, problem = P{ width, height, ... }, ... },
              x, y) =
    x < 0 orelse y < 0 orelse x >= width orelse y >= height orelse
    Array.sub (board, y * width + x)
  fun isempty (state, x, y) = not (isfull (state, x, y))

  datatype getpieceresult =
      GPGameOver of why
    | GP of { next_sourceidx: int,
              rng: RNG.rng,
              piece: piece }

  (* Hypothetically, if we moved the pivot to (nx, ny) with angle a,
     would this be a collision? Then we lock at the OLD location. *)
  fun is_locked_at (P { width, height, ... },
                    board, Piece { rotations, ... }, nx, ny, na) =
    let
      val oriented_piece = Vector.sub(rotations, na)

      (* The displacement applied to every member of the piece. *)
      val (upx, upy) = uniformize_coord (nx, ny)
      fun is_collision (px, py) =
        let val (px, py) = translate (upx, upy) (px, py)
        in
          px < 0 orelse py < 0 orelse
          px >= width orelse py >= height orelse
          Array.sub (board, py * width + px)
        end
    in
      Vector.exists is_collision oriented_piece
    end

  fun getpiece (problem as P { sourcelength, pieces, ... } : problem,
                board,
                sourceidx : int,
                rng : RNG.rng) =
    if sourceidx >= sourcelength
    then GPGameOver COMPLETE
    else
      let
        val (piece_idx, rng) = RNG.next rng
        val piece_idx = piece_idx mod Vector.length pieces

        val piece as Piece { rotations,
                             symmetry = _,
                             start = (startx, starty) } =
          Vector.sub (pieces, piece_idx)
      in
        if is_locked_at (problem, board, piece, startx, starty, 0)
        then GPGameOver NO_SPACE
        else GP { next_sourceidx = sourceidx + 1,
                  piece = piece,
                  rng = rng }
      end

  fun resetwithseed (problem as P { seeds, start, pieces, ... },
                     seed : Word32.word) : state =
    let
      val initial_rng = RNG.fromseed seed

      (* PERF Faster way? *)
      val board = Array.tabulate
        (Vector.length start,
         (fn x => Vector.sub(start, x)))
    in
      case getpiece (problem, board, 0, initial_rng) of
        (* TODO: Handle these gracefully if technically legal? *)
        GPGameOver NO_SPACE => raise Board "no space in INITIAL CONFIGURATION??"
      | GPGameOver COMPLETE => raise Board "source length is 0??"
      | GP { next_sourceidx,
             piece as Piece { start = (startx, starty), ... },
             rng } =>
          let
            val initial_stutters =
              (* angle is 0 mod symmetry, which is always 0 *)
              StutterSet.add (StutterSet.empty, (startx, starty, 0))
          in
              S { rng = ref rng,
                  score = ref 0,
                  piece = ref piece,
                  problem = problem,
                  next_sourceidx = ref next_sourceidx,
                  last_lines = ref 0,
                  x = ref startx,
                  y = ref starty,
                  (* start valid; we checked the invalid conditions
                     above and raised an exception *)
                  valid = ref true,
                  (* always start in natural orientation *)
                  a = ref 0,
                  stutters = ref initial_stutters,
                  board = board }
          end
    end

  fun reset (problem as P { seeds, ... }, seed_idx : int) : state =
    let
      val seed =
        if seed_idx < 0 orelse
           seed_idx >= Vector.length seeds
        then raise Board "Bad seed idx in reset"
        else Vector.sub (seeds, seed_idx)
    in
      resetwithseed (problem, seed)
    end

  fun ispiece (S {x, y, piece = ref (Piece { rotations, ... }), a, ...},
               xx, yy) =
    let
      (* translate (xx, yy) to piece space, and look it up
         within the current rotation. *)

      (* To translate, coordinates need to be uniform *)
      val (udx, udy) = uniformize_coord (!x, !y)
      val (px, py) = translate (~udx, ~udy) (xx, yy)

      val oriented_piece = Vector.sub (rotations, !a)
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

  fun statusstring CONTINUE = "CONTINUE"
    | statusstring (GAMEOVER COMPLETE) = "COMPLETE"
    | statusstring (GAMEOVER NO_SPACE) = "NO_SPACE"
    | statusstring ERROR = "ERROR"

  fun moveresultstring (M {scored, lines, locked, status }) =
    "{ scored: " ^ Int.toString scored ^ ", status: " ^ statusstring status ^" }"

  (* TODO: Use this beauty, but might need to use ansi backgrounds...

     WRONG NOT BEAUTIFUL!!!
      \__/  \__/  \__/  \__/  \__/  \__/
      /  \__/  \__/  \__/  \__/  \__/  \__
      \__/  \__/  \__/  \__/  \__/  \__/
      /  \__/  \__/  \__/  \__/  \__/  \__
      \__/  \__/  \__/  \__/  \__/  \__/      *)

  fun toascii (state as
               S { problem = P {width, height, ... },
                   valid,
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
      (if false = !valid then "INVALID!\n" else "") ^
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
    let
      val c = Char.toLower c
    in
      (* For side effect of raising exception *)
      ignore (charcommand c) handle Board _ => raise Board ("bad legalchar [" ^
                                                            implode [c] ^ "]");
      c
    end

  fun forgetlegal c = c

  (* Imperatively update board to reflect deleted lines, and return
     the number of lines so deleted. *)
  fun check_lines (P { width, height, ... }) board =
    let
      val newy = ref (height - 1)
      val lines = ref 0
    in
      Util.for 0 (height - 1)
      (fn negy =>
       let
         val y = height - negy - 1
         val should_delete =
             ArraySlice.all (fn x => x)
                            (ArraySlice.slice(board, y * width, SOME width))
       in
         if should_delete
         then lines := !lines + 1
         else (if !newy <> y
                then Util.for 0 (width - 1)
                  (fn x => Array.update(board, !newy * width + x,
                                        Array.sub(board, y * width + x)))
               else ();
               newy := !newy - 1)
       end);
      Util.for 0 (!newy)
      (fn y =>
       Util.for 0 (width - 1)
       (fn x => Array.update(board, y * width + x, false)));
      !lines
    end

  fun move_undo (state as
                 S { rng, score, piece as ref (Piece { symmetry, ... }),
                     next_sourceidx, last_lines,
                     stutters, valid = valid as ref true,
                     problem = problem as P { width, height, ... },
                     x, y, a, board },
                 ch : legalchar) =
    let
      (* Don't modify anything until we know what branch we're in!
         We want to create an undo function that does a minimal
         amount of work. *)

      fun freeze () =
        let
          val Piece { rotations, ... } = !piece
          val oriented_piece = Vector.sub(rotations, !a)

          (* The displacement applied to every member of the piece. *)
          val (upx, upy) = uniformize_coord (!x, !y)
          fun write_to_board (px, py) =
            let val (px, py) = translate (upx, upy) (px, py)
            in
              (* Could be assert. (none of these conditions should
                 be true)
              px < 0 orelse py < 0 orelse
              px >= width orelse py >= height orelse
              Array.sub (board, py * width + px) *)
              Array.update (board, py * width + px, true)
            end
        in
          Vector.app write_to_board oriented_piece
        end

      val (nx, ny, na) =
        case charcommand ch of
          D dir =>
            let
              val (dx, dy) = delta dir
              val (nx, ny) = translate (dx, dy) (!x, !y)
            in
              (nx, ny, !a)
            end
        | T turn =>
            let
              val angle = case turn of CW => 1 | CCW => ~1
              val new_a = (!a + angle) mod 6
            in
              (!x, !y, new_a)
            end

      (* Check if we have a stutter. If not, add to the stutter
         set. *)
      fun check_and_add_repeat_at (nx, ny, na) : bool =
        let
          val na = na mod symmetry
        in
          if StutterSet.member (!stutters, (nx, ny, na))
          then true
          else
            let in
              stutters := StutterSet.add (!stutters, (nx, ny, na));
              false
            end
        end

      (* check_and_add_repeat_at may modify this, so save the old set *)
      val old_stutters = !stutters

    in
      if check_and_add_repeat_at (nx, ny, na)
      then
        (* Here, we haven't even modified the stutter set, so
           there is nothing to undo. *)
        { result = M { scored = 0, lines = 0, locked = NONE, status = ERROR },
          undo = fn () => () }
      else
      if is_locked_at (problem, board, !piece, nx, ny, na)
      then
        let
          (* Slow path. Just copy everything. Nothing should have been
             modified yet except for the stutter set. *)
          (* PERF! Avoid copying the board if for example we don't have
             any lines. The idea is that undo can just un-fill the cells
             that are locked.

             It's a little tricky because we'd like to avoid copying
             the board at all unless we make a line, but we need to
             modify the board to add frozen cells before looking
             whether we created lines. (or, make the line code be
             capable of reinserting a line; it's actually pretty easy
             since we know what the contents of the line was (all
             full). *)
          val old_board = clone_array board
          val old_rng = !rng
          val old_score = !score
          val old_x = !x
          val old_y = !y
          val old_a = !a
          (* old_stutters above *)
          val old_piece = !piece
          val old_last_lines = !last_lines
          val old_next_sourceidx = !next_sourceidx

          fun full_undo () =
            let in
              rng := old_rng;
              score := old_score;
              x := old_x;
              y := old_y;
              a := old_a;
              stutters := old_stutters;
              piece := old_piece;
              last_lines := old_last_lines;
              next_sourceidx := old_next_sourceidx;
              valid := true;
              Array.copy {di = 0, dst = board, src = old_board}
            end

          (* OK, now safe to modify anything we want... *)

          val () = freeze()
          val lines = check_lines problem board

          val Piece { rotations, ... } = !piece
          val oriented_piece = Vector.sub(rotations, !a)
          val points = Vector.length oriented_piece + 100 *
            ((1 + lines) * lines) div 2
          val line_bonus =
            if !last_lines > 1
            then ((!last_lines - 1) * points) div 10
            else 0
          val move_score = points + line_bonus
          val locked = SOME (!x, !y, !a)
        in
          (* Now, try to place the next piece (if any) in the updated board. *)
          case getpiece (problem, board, !next_sourceidx, !rng) of
            GP { next_sourceidx = new_ns,
                 piece = new_piece as Piece { start = (startx, starty), ... },
                 rng = new_rng } =>
              let
              in
                (* print "New piece!\n"; *)
                next_sourceidx := new_ns;
                piece := new_piece;
                rng := new_rng;
                last_lines := lines;
                x := startx;
                y := starty;
                a := 0;
                stutters := StutterSet.add (StutterSet.empty,
                                            (startx, starty, 0));

                (* lines should affect score. *)
                { result = M { lines = lines, scored = move_score,
                               locked = locked, status = CONTINUE },
                  undo = full_undo }
              end
          | GPGameOver why =>
              let in
                valid := false;
                { result = M { lines = lines, scored = move_score,
                               locked = locked, status = GAMEOVER why },
                  undo = full_undo }
              end
        end
      else
        let
          (* Here we are just moving. We don't need to copy the board
             (it can't change), nor check for lines, etc. *)
          val old_score = !score
          val old_x = !x
          val old_y = !y
          val old_a = !a
          val old_last_lines = !last_lines
          (* old_stutters above *)

          (* These can't change. *)
          (* val old_piece = !piece
             val old_next_sourceidx = !old_next_sourceidx
             val old_rng = !rng *)

          fun positional_undo () =
            let in
              score := old_score;
              x := old_x;
              y := old_y;
              a := old_a;
              stutters := old_stutters;
              last_lines := old_last_lines
            end
        in
          (* No locking. Don't need to check lines, score, etc. *)
          x := nx;
          y := ny;
          a := na;
          last_lines := 0;
          (* stays valid... *)

          (* PERF board hasn't changed -- don't need backup of it *)
          { result = M { scored = 0, lines = 0, locked = NONE,
                         status = CONTINUE },
            undo = positional_undo }
        end
    end
    | move_undo _ = raise Board "invalid board in move(_undo,_unwind)"

  fun move (s, c) = #result (move_undo (s, c))

  fun move_unwind (s, c, k) =
    let
      val {result, undo} = move_undo (s, c)
      val r = k result
    in
      undo ();
      r
    end

  fun piecesleft (S { problem = P { sourcelength, ... },
                      valid = ref true,
                      next_sourceidx, ... }) =
    sourcelength - !next_sourceidx
    | piecesleft _ = raise Board "invalid board in piecesleft"

  fun size (P { width, height, ... }) = (width, height)
  fun width (P { width, ... }) = width
  fun height (P { height, ... }) = height
  fun pieces (P { pieces, ... }) = pieces
  fun seeds (P { seeds, ... }) = seeds

end
