structure Board :> BOARD =
struct

  exception Board of string

  datatype dir = E | W | SE | SW
  datatype turn = CW | CCW
  datatype command = D of dir | T of turn
  datatype why =
    COMPLETE
  | NO_SPACE
  datatype status =
    CONTINUE
  | GAMEOVER of why
  | ERROR

  datatype moveresult =
    M of { scored: int, lines: int, new_phrases: int,
           locked: (int * int * int) option,
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
    P of { id : int,
           start : bool vector,
           width : int,
           height : int,
           sourcelength : int,
           (* power words; each word is REVERSED and LOWERCASE. *)
           power : string vector,
           seeds : Word32.word vector,
           (* aka units, an ml keyword *)
           pieces : piece vector }

  fun setPower (P {id, start, width, height, sourcelength, seeds, pieces, ...}, newPower) =
    P { id = id, start = start, width = width, height = height,
        sourcelength = sourcelength,
        seeds = seeds, pieces = pieces,
        power = Vector.map (StringUtil.reverse o StringUtil.lcase) newPower }

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

           (* Please don't expose these; I want to
              make this representation more efficient, which
              will involve not actually storing the command string *)
           (* commands executed to this point; head is the most recent *)
           chars: legalchar list ref,
           (* how many times each power word has been used so far. *)
           power_count: int array,

           (* Places we have already been. *)
           stutters: StutterSet.set ref,

           valid: bool ref,

           rng: RNG.rng ref }

  fun piece_position (S {x, y, valid = ref true, ...}) = (!x, !y)
    | piece_position _ = raise Board "invalid board in piece_position"
  fun piece_angle (S {a, valid = ref true, ...}) = !a
    | piece_angle _ = raise Board "invalid board in piece_angle"

  fun piece_symmetry (S {piece = ref (Piece { symmetry, ... }),
                         valid = ref true, ...}) =
    symmetry
    | piece_symmetry _ = raise Board "invalid board in piece_symmetry"

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

  fun fromjsonwithpower (s, power) =
    let
      datatype json = datatype JSON.value

      val j = JSONParser.parseFile s

      val width = JSONUtils.Int (j, "width")
      val height = JSONUtils.Int (j, "height")
      val id = JSONUtils.Int (j, "id")
      val filled = map JSONUtils.Coord (JSONUtils.List (j, "filled"))
      val sourcelength = JSONUtils.Int (j, "sourceLength")
      (* TODO: Harden against these being bigger than 2^31, negative, etc. *)
      val seeds = map JSONUtils.UnInt (JSONUtils.List (j, "sourceSeeds"))

      fun ExpectPiece j =
        let
          val members = map JSONUtils.Coord (JSONUtils.List (j, "members"))
          (* We represent all pieces as being centered on their origin.
             So we start by translating all members so that the pivot ends
             up on the origin. *)
          val (px, py) = uniformize_coord (JSONUtils.Coord
                                               (JSONUtils.Key (j, "pivot")))
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

      val power = Vector.map (StringUtil.reverse o
                              StringUtil.lcase) power
    in
      app (fn (x, y) =>
           Array.update (a, y * width + x, true)) filled;
      P { id = id, width = width, height = height,
          seeds = Vector.fromList (map Word32.fromInt seeds),
          sourcelength = sourcelength,
          power = power,
          start = Array.vector a,
          pieces = Vector.fromList
          (map makepiece
           (map ExpectPiece (JSONUtils.List (j, "units")))) }
    end

  fun fromjson s = fromjsonwithpower (s, Vector.fromList Phrases.power)

  fun clone_array a =
    (* PERF There must be a faster way to do this?? *)
    Array.tabulate (Array.length a, fn i => Array.sub(a, i))

  (* OK to clone invalid boards, I guess. *)
  fun clone (S { board, next_sourceidx, last_lines, stutters, chars,
                 power_count, valid, problem, score, rng, piece,
                 a, x, y }) : state =
           S { board = clone_array board,
               problem = problem,
               score = ref (!score),
               next_sourceidx = ref (!next_sourceidx),
               stutters = ref (!stutters),
               a = ref (!a),
               x = ref (!x),
               y = ref (!y),
               valid = ref (!valid),
               chars = ref (!chars),
               power_count = clone_array power_count,
               last_lines = ref (!last_lines),
               piece = ref (!piece),
               rng = ref (!rng) }

  fun isfull (S { board, problem = P{ width, height, ... },
                  valid = ref true, ... },
              x, y) =
    x < 0 orelse y < 0 orelse x >= width orelse y >= height orelse
    Array.sub (board, y * width + x)
    | isfull _ = raise Board "invalid board in isfull/isempty"
  fun isempty (state, x, y) = not (isfull (state, x, y))

  datatype getpieceresult =
      GPGameOver of why
    | GP of { next_sourceidx: int,
              rng: RNG.rng,
              piece: piece }

  (* Hypothetically, if we moved the pivot to (nx, ny) with angle a,
     would this be a collision? Then we lock at the OLD location. *)
  fun is_locked_at (P { id, width, height, ... },
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

  fun resetwithseed (problem as P { seeds, start, pieces, power, ... },
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
                  chars = ref nil,
                  power_count = Array.array (Vector.length power, 0),
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

  fun ispiece (S {x, y, piece = ref (Piece { rotations, ... }), a,
                  valid = ref true, ...},
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
    | ispiece _ = raise Board "invalid board in ispiece"

  fun ispivot (S {x, y, valid = ref true, ...}, xx, yy) = xx = !x andalso yy = !y
    | ispivot _ = raise Board "invalid board in ispivot"
  fun pivot (S { x, y, valid = ref true, ... }) = (!x, !y)
    | pivot _ = raise Board "invalid board in pivot"

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

  fun moveresultstring (M {scored, lines, new_phrases, locked, status }) =
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

  fun powerinfostring (state as
                       S { problem = P { power, ... },
                           chars, power_count, valid, ... }) =
    let
      val s = ref ""
    in
      Util.for 0 (Vector.length power - 1)
      (fn i =>
       s := !s ^
       Int.toString i ^ ". x" ^ Int.toString (Array.sub(power_count, i)) ^
       " = " ^ StringUtil.reverse (Vector.sub (power, i)) ^ "\n");
      (if false = !valid then "INVALID!\n" else "") ^
      !s
    end


  val dw   : legalchar vector = Vector.fromList (explode "p'!.03")
  val de   : legalchar vector = Vector.fromList (explode "bcefy2")
  val dsw  : legalchar vector = Vector.fromList (explode "aghij4")
  val dse  : legalchar vector = Vector.fromList (explode "lmno 5") (* ell *)
  val tcw  : legalchar vector = Vector.fromList (explode "dqrvz1") (* one *)
  val tccw : legalchar vector = Vector.fromList (explode "kstuwx")
  val legalchars = Vector.concat [dw, de, dsw, dse, tcw, tccw]
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

  fun islegal c =
    (* PERF -- are exns fast? *)
    (ignore (charcommand c); true) handle Board _ => false

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
                     next_sourceidx, last_lines, chars, power_count,
                     stutters, valid = valid as ref true,
                     problem = problem as P { width, height, power, ... },
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

      (* PERF! *)
      fun get_powerlist revchars =
        let
          (* does the word w (which is reversed) match the
             end of the revchars list (which is reversed?) *)
          fun oneword w =
            let
              fun loop (idx, l) =
                if idx = size w
                then true
                else
                  case l of
                    nil => false
                  | h :: t =>
                      if h = String.sub(w, idx)
                      then loop (idx + 1, t)
                      else false
            in
              loop (0, revchars)
            end

          val res = ref nil
          fun loop n =
            if n = Vector.length power
            then nil
            else
              let in
                (if oneword (Vector.sub (power, n))
                 then res := n :: !res
                 else ());
                loop (n + 1)
              end
        in
          loop 0;
          !res
        end

      (* check_and_add_repeat_at may modify this, so save the old set *)
      val old_stutters = !stutters

      (* need this no matter what. *)
      val old_chars = !chars
      val newchars = ch :: old_chars
      val powerlist = get_powerlist newchars

      fun add_power r =
        List.app (fn word_idx =>
                  Array.update(r, word_idx, Array.sub(r, word_idx) + 1)) powerlist
      fun subtract_power r =
        List.app (fn word_idx =>
                  Array.update(r, word_idx, Array.sub(r, word_idx) - 1)) powerlist

      (* Compute the power score for this char. Doesn't modify the
         power counts yet. *)
      val power_score = ref 0
      val new_phrases = ref 0
      val () =
        (* All the power words (indices) we just made. *)
        app (fn i =>
             let
               val revw = Vector.sub(power, i)
               val power_base = 2 * size revw
               (* Bonus only the first time *)
               val is_new = Array.sub(power_count, i) = 0
               val power_bonus = if is_new
                                 then 300
                                 else 0
             in
               (if is_new then new_phrases := !new_phrases + 1 else ());
               power_score := !power_score + power_base + power_bonus
             end) powerlist
      val power_score = !power_score
      val new_phrases = !new_phrases

    in
      if check_and_add_repeat_at (nx, ny, na)
      then
        (* Here, we haven't even modified the stutter set, so
           there is nothing to undo. *)
        { result = M { scored = 0, lines = 0, new_phrases = new_phrases,
                       locked = NONE, status = ERROR },
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
          (* old_chars above *)
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
              (* or just pop? *)
              chars := old_chars;
              subtract_power power_count;

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
                chars := newchars;
                add_power power_count;

                stutters := StutterSet.add (StutterSet.empty,
                                            (startx, starty, 0));

                (* lines should affect score. *)
                { result = M { lines = lines, scored = move_score + power_score,
                               new_phrases = new_phrases,
                               locked = locked, status = CONTINUE },
                  undo = full_undo }
              end
          | GPGameOver why =>
              let in
                (* Additionally mark invalid. *)
                valid := false;
                { result = M { lines = lines, scored = move_score + power_score,
                               new_phrases = new_phrases,
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
          (* old_chars above *)

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
              last_lines := old_last_lines;
              chars := old_chars;
              subtract_power power_count
            end
        in
          (* No locking. Don't need to check lines, score, etc. *)
          x := nx;
          y := ny;
          a := na;
          last_lines := 0;
          (* stays valid... *)
          chars := newchars;
          add_power power_count;

          (* PERF board hasn't changed -- don't need backup of it *)
          { result = M { scored = power_score, lines = 0, locked = NONE,
                         new_phrases = new_phrases,
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

  fun move_unwind_many (s, [], k) = k true
    | move_unwind_many (s, c::cs, k) =
      move_unwind (s, c, (fn (M {locked, status, ...}) =>
                          (case (locked, status) of
                             (NONE, CONTINUE) => move_unwind_many (s, cs, k)
                           | _ => k false)))

  fun piecesleft (S { problem = P { sourcelength, ... },
                      valid = ref true,
                      next_sourceidx, ... }) =
    sourcelength - !next_sourceidx
    | piecesleft _ = raise Board "invalid board in piecesleft"

  fun id (P { id, ... }) = id
  fun size (P { width, height, ... }) = (width, height)
  fun width (P { width, ... }) = width
  fun height (P { height, ... }) = height
  fun pieces (P { pieces, ... }) = pieces
  fun seeds (P { seeds, ... }) = seeds


  fun raggedness_heuristic (S { problem = P { width, height, ... },
                               board, ... }) =
    let
      val count = ref 0
    in
      Util.for 0 (height - 1)
      (fn y =>
       let
         val off = y * width
         val parity =
           ref (Array.sub (board, off + 0))
       in
         Util.for 1 (width - 1)
         (fn x =>
          let val value = Array.sub (board, off + x)
          in
            if value <> !parity
            then (count := !count + 1;
                  parity := value)
            else ()
          end)
       end);
      !count
    end

  fun simple_heuristic (S { problem = P { width, height, sourcelength, ... },
                            board,
                            next_sourceidx, ... }) =
      let
        val piecesleft = sourcelength - !next_sourceidx
        val future_pieces = piecesleft * 50
        val score = ref future_pieces
      in
        Util.for 0 (height - 1)
        (fn yy =>
         Util.for 0 (width - 1)
         (fn xx =>
          if false = Array.sub (board, yy * width + xx)
          then
            let in
              (* more points, proportional to distance from botton *)
              score := (!score + (height - yy))
            end
          else ()));
        !score
    end

  (*
  type stateset = Word32.word array

  type bitarray = Word32Array.array
  fun ba_sub (a, i) =
    Word32Array.sub (a, i div
    *)
    type stateset = BoolArray.array

  local
    (* from jenkins *)
    fun mix3 (a, b, c) =
      let
        open Word32
        infix >> <<
      in
        a := !a - !b; a := !a - !c; a := Word32.xorb(!a, !c >> 0w13);
        b := !b - !c; b := !b - !a; b := Word32.xorb(!b, !a << 0w8);
        c := !c - !a; c := !c - !b; c := Word32.xorb(!c, !b >> 0w13);
        a := !a - !b; a := !a - !c; a := Word32.xorb(!a, !c >> 0w12);
        b := !b - !c; b := !b - !a; b := Word32.xorb(!b, !a << 0w16);
        c := !c - !a; c := !c - !b; c := Word32.xorb(!c, !b >> 0w5);
        a := !a - !b; a := !a - !c; a := Word32.xorb(!a, !c >> 0w3);
        b := !b - !c; b := !b - !a; b := Word32.xorb(!b, !a << 0w10);
        c := !c - !a; c := !c - !b; c := Word32.xorb(!c, !b >> 0w15)
      end

  in
    fun hashboard board =
      let
        val a = ref (0wx9e3779b9 : Word32.word)
        val b = ref (0wx9e3779b9 : Word32.word)
        val c = ref (0wxbeef : Word32.word)
      in
        Util.for 0 (Array.length board - 1)
        (fn i =>
         let val bit = if (Array.sub (board, i)) then 0w1 else 0w0
         in
           b := !b + bit;
           mix3 (a, b, c)
         end);
        mix3 (a, b, c);
        !c
      end
  end

  val BITS = 15485863
  val BITSW = Word32.fromInt BITS
  (*   val WORDS = (BITS div 8) + 1 *)
  fun empty_stateset () = (* Array.array (WORDS, 0w0 : Word32.word) *)
    BoolArray.array (BITS, false)

  fun hashkey (S { board, ... }) =
    Word32.toInt (Word32.mod (hashboard board, BITSW))
  fun insert stateset state =
    BoolArray.update (stateset, hashkey state, true)
  fun contains stateset state =
    BoolArray.sub (stateset, hashkey state)

end
