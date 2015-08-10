signature SOLUTIONS =
sig

  type solution = { seconds : int,
                    problem : Board.problem,
                    seed_idx : int } -> string

  val david : solution
  val ragged : solution
  val both : solution
  val highfive : solution

  (* All solutions with their short ascii names *)
  val all_solutions : (string * solution) list

  (* Consensus current best from tests *)
  val best_solution : solution

end