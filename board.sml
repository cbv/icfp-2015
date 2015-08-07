structure Board :> BOARD =
struct

  exception Board of string

  datatype dir = E | W | SE | SW
  datatype turn = CW | CCW
  datatype command = D of dir | T of turn
  datatype status =
    (* Game keeps going *)
    CONTINUE
  (* Gracefully used all pieces *)
  | COMPLETE
  (* Last move locked a piece, but there's no space
     to place the next one. *)
  | NO_SPACE
  (* Bad command? *)
  | ERROR

  datatype moveresult =
    M of { scored: int, lines: int, locked: bool, status: status }

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

           piece: piece ref,
           next_sourceidx: int ref,

           (* Position of pivot (spec coords) *)
           x: int ref,
           y: int ref,

           (* Angle in hexgrees [0, 5] *)
           a: int ref,

           (* XXX Need history of commands, for word scoring *)

           (* XXX history of where we've been *)

           rng: RNG.rng ref }

    (*

       XXX todo: just keep pivot x,y and angle MOD SYMMETRY GROUP PRECOMPUTED

  (* This is a place that we've been in the past; we need to keep a set
     of these so that we don't ever repeat a configuration. Contest
     people have confirmed that the identity of the pivot matters, and
     the identity of the
*)
 type
     presence
     *)

  fun piece_position (S {x, y, ...}) = (!x, !y)
  fun piece_angle (S {a, ...}) = !a

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
    (* I think ArraySlice *)
    Array.tabulate (Array.length a, fn i => Array.sub(a, i))

  fun clone (S { board, next_sourceidx,
                 problem, score, rng, piece, a, x, y }) : state =
           S { board = clone_array board,
               problem = problem,
               score = ref (!score),
               next_sourceidx = ref (!next_sourceidx),
               a = ref (!a),
               x = ref (!x),
               y = ref (!y),
               piece = ref (!piece),
               (* piece = clone_array piece, *)
               rng = ref (!rng) }

  fun isfull (S { board, problem = P{ width, height, ... }, ... },
              x, y) =
    x < 0 orelse y < 0 orelse x >= width orelse y >= height orelse
    Array.sub (board, y * width + x)
  fun isempty (state, x, y) = not (isfull (state, x, y))

  datatype getpieceresult =
      GPNoSpace
    | GPGameOver
    | GP of { next_sourceidx: int,
              rng: RNG.rng,
              piece: piece }

  fun getpiece (problem as P { sourcelength, pieces, ... } : problem,
                sourceidx : int,
                rng : RNG.rng) =
    if sourceidx >= sourcelength
    then GPNoSpace
    else
      let
        val (piece_idx, rng) = RNG.next rng

        val piece as Piece { rotations,
                             symmetry = _,
                             start = (startx, starty) } =
          Vector.sub (pieces, piece_idx)

      in
        (* XXX see if it fits *)
        GP { next_sourceidx = sourceidx + 1,
             piece = piece,
             rng = rng }
      end


  fun reset (problem as P { seeds, start, pieces, ... },
             seed_idx) : state =
    let
      val initial_rng =
        if seed_idx < 0 orelse
           seed_idx >= Vector.length seeds
        then raise Board "Bad seed idx in reset"
        else RNG.fromseed (Vector.sub (seeds, seed_idx))
    in
      case getpiece (problem, 0, initial_rng) of
        (* TODO: Handle these gracefully if legal? *)
        GPNoSpace => raise Board "no space in INITIAL CONFIGURATION??"
      | GPGameOver => raise Board "source length is 0??"
      | GP { next_sourceidx,
             piece as Piece { start = (startx, starty), ... },
             rng } =>
          S { rng = ref rng,
              score = ref 0,
              piece = ref piece,
              problem = problem,
              next_sourceidx = ref next_sourceidx,
              x = ref startx,
              y = ref starty,
              (* always start in natural orientation *)
              a = ref 0,
              (* PERF ArraySlice. *)
              board = Array.tabulate
              (Vector.length start,
               (fn x => Vector.sub(start, x))) }

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

  fun statusstring _ = "sorry, unimplemented"

  (* XXX show everything *)
  fun moveresultstring _ = "sorry, unimplemented..."
(*
  fun moveresultstring (Continue {scored, lines, locked}) = "Continue..."
    | moveresultstring (Done {reason}) = "Done..."
*)

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

  (* Imperatively update board to reflect deleted lines, and return
     the number of lines so deleted. *)
  fun check_lines (P { width, height, ... }) board =
    let
      val newy = ref (height-1)
      val lines = ref 0
    in
      Util.for 0 (height-1)
      (fn negy =>
       let
         val y = height - negy - 1
       in
         if ArraySlice.all (fn x => x)
                           (ArraySlice.slice(board, y * width, SOME width))
         then lines := !lines + 1
         else ((if !newy <> y
                then Util.for 0 (width-1)
                  (fn x => Array.update(board, !newy * width + x,
                                        Array.sub(board, y * width + x)))
                else ());
               newy := !newy - 1)
       end);
      Util.for 0 (!newy)
      (fn negy =>
       let
         val y = height - negy - 1
       in
         Util.for 0 (width-1)
         (fn x => Array.update(board, y * width + x, false))
       end);
      !lines
    end

  fun move_undo (state as
                 S { rng, score, piece, next_sourceidx,
                     problem = problem as P { width, height, ... },
                     x, y, a, board },
                 ch : legalchar) =
    let
      (* PERF! Avoid copying the board; the idea here is that we
         can just un-fill any cells that are locked, if that happens. *)
      val old_state = clone state
      fun undo () =
        let
          val S { rng = old_rng, score = old_score, x = old_x, y = old_y,
                  a = old_a, board = old_board, piece = old_piece,
                  next_sourceidx = old_next_sourceidx, ... } = old_state
        in
          rng := !old_rng;
          score := !old_score;
          x := !old_x;
          y := !old_y;
          a := !old_a;
          piece := !old_piece;
          next_sourceidx := !old_next_sourceidx;
          Array.copy {di = 0, dst = board, src = old_board}
        end

      (* Hypothetically, if we moved the pivot to (nx, ny) with angle a,
         would this be a collision? Then we lock at the OLD location. *)
      fun is_locked_at (nx, ny, na) =
        let
          val Piece { rotations, ... } = !piece
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
    in
      if is_locked_at (nx, ny, na)
      then
        let
          (* TODO: get score from freezing *)
          val () = freeze()
          val lines = check_lines problem board
        in
          case getpiece (problem, !next_sourceidx, !rng) of
            GP { next_sourceidx = new_ns, piece = new_piece, rng = new_rng } =>
              let in
                next_sourceidx := new_ns;
                piece := new_piece;
                rng := new_rng;

                (* lines should affect score. *)
                { result = M { lines = lines, scored = 0, locked = true,
                               status = CONTINUE },
                  undo = undo }
              end
          | GPNoSpace =>
              { result = M { lines = lines, scored = 0, locked = true,
                             status = NO_SPACE },
                undo = undo }
          | GPGameOver =>
              { result = M { lines = lines, scored = 0, locked = true,
                             status = COMPLETE },
                undo = undo }
        end
      else
        let in
          (* Don't need to check lines, score, etc. *)
          x := nx;
          y := ny;
          a := na;
          (* PERF board hasn't changed -- don't need backup of it *)
          { result = M { scored = 0, lines = 0, locked = false,
                         status = CONTINUE },
            undo = undo }
        end
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


  fun size (P { width, height, ... }) = (width, height)
  fun width (P { width, ... }) = width
  fun height (P { height, ... }) = height
  fun pieces (P { pieces, ... }) = pieces

end
