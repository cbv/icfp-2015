structure Perturbation (* :> PERTURBATION *) =
struct

  datatype turn = datatype Board.turn
  datatype dir = datatype Board.dir
  datatype command = datatype Board.command
  datatype ptpos = PTP of {ux:int, uy:int, a:int}

  datatype pt_soln = S of {
             phop_length : int,
             cmds : command Vector.vector,
             choices : int Vector.vector,
             start_time : int,
             time : int,
             (* The additional offset we need to *achieve* to reach
             parity with the original solution. *)
             offset : ptpos }

  val ptz = PTP {ux=0, uy=0, a=0}
  fun ptadd (PTP p, PTP q) =
    PTP { ux = #ux p + #ux q,
          uy = #uy p + #uy q,
          a = #a p + #a q }

  fun ptsub (PTP p, PTP q) =
    PTP { ux = #ux p - #ux q,
          uy = #uy p - #uy q,
          a = #a p - #a q }

  fun ptsum pts = foldr ptadd ptz pts

  fun execute (D E) = PTP {ux=1, uy=0, a=0}
    | execute (D W) = PTP {ux= ~1, uy=0, a=0}
    | execute (D SE) = PTP {ux=0, uy=1, a=0}
    | execute (D SW) = PTP {ux= ~1, uy=1, a=0}
    | execute (T CW) = PTP {ux=0, uy=0, a=1}
    | execute (T CCW) = PTP {ux=0, uy=0, a= ~1}

  fun choice_order (PTP {ux,uy,a}) =
    let
      val turns = if a = 1 orelse a = 2 orelse a = 3
                  then [T CW, T CCW]
                  else [T CCW, T CW]
      val horiz = if ux > 0
                  then [D E, D W]
                  else [D W, D E]
      val diag = if ux > 0
                 then [D SE, D SW]
                 else [D SW, D SE]
      val moves = if uy > 0
                  then diag @ horiz
                  else horiz @ diag
    in
      if (a <> 0)
      then turns @ moves
      else moves @ turns
    end

  (* fun strict_next_soln cmds (time, choices, residual) = *)
  (*   let *)
  (*     fun next_soln time  choices 0 = choices *)
  (*       | next_soln time  choices residual = increment (time choices residual) *)
  (*     fun increment (time, choices, residual) *)
  (*   in *)
  (*     next *)
  (*   end *)

  (* fun next_soln (soln as S { phop_length, cmds, choices, start_time, time, offset }) *)
  (*   = { phop_length = phop_length, *)
  (*       cmds = cmds, *)
  (*       choices = next_choice choices, *)
  (*       start_time = start_time, *)
  (*       time =  *)
  fun next_soln soln = soln
  fun soln_cmds (S { cmds, ...}) = cmds

  fun insert_power phop cmds n =
    let
      val phop_length = size phop
      (* The offset caused by the phrase of power *)
      val phop_offset = ptsum (map (execute o Board.charcommand o Board.legalize)
                                     (explode phop))
      (* The offset caused by the commands the phop occludes *)
      val orig_offset = Vector.foldr
                            ptadd ptz
                            (VectorSlice.map
                                 execute
                                 (VectorSlice.slice(cmds, n, SOME phop_length)))
    in
      next_soln (
        S { cmds = cmds,
            phop_length = phop_length,
            choices = Vector.tabulate(0, fn _ => raise Match),
            start_time = n,
            time = n + phop_length,
            offset = ptsub (orig_offset, phop_offset)
      })
    end

end

structure PP = Perturbation
structure PP :> PERTURBATION = Perturbation
