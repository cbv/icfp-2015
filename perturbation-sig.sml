signature PERTURBATION =
sig
  type pt_soln

  datatype ptpos = PTP of {ux:int, uy:int, a:int}
  val ptadd : ptpos * ptpos -> ptpos
  val ptsum : ptpos list -> ptpos
  val execute : Board.command -> ptpos

  val choice_order : ptpos -> Board.command list

  (* insert_power phop cmds n

     Set up a perturbation problem. Its goal is to use the
     phrase-of-power `phop` at index n of commands `cmds`, and somehow
     recover back to a piece state that is similar to what had
     obtained before the perturbation.

     It finds a candidate solution to the problem. We don't guarantee here
     that it doesn't, e.g. collide with walls or anything. We're just
     aiming for an identical (x, y, angle) displacement. The caller
     must verify that the solution is as desired. *)

  val insert_power : string -> Board.command Vector.vector -> int -> pt_soln

  (* Given a candidate solution, give another one *)
  val next_soln : pt_soln -> pt_soln

  (* Yield the actual commands a solution prescribes *)
  val soln_cmds : pt_soln -> Board.command Vector.vector
end
