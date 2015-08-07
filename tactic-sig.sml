
signature TACTIC = sig
  exception Tactic of string

  (* facing encodes the rotation from initial of the piece *)
  datatype facing = R0 | R1 | R2 | R3 | R4 | R5

  (* a goal is a target location for the pivot, along with a net rotation of
   * the piece *)
  type goal = int * int * facing

  datatype solution =
      (* I can't find a solution *)
      Unsolved
      (* There is no solution *)
    | Unsolvable
      (* Solution, with power-word counts/prefixes *)
    | Solved of Board.command list (* commands to place piece at targe t *)
              * int list           (* number of times each power word chanted,
                                    * index is power word index, value is
                                    * count *)
              * int list list      (* power word prefixes - first index is
                                    * power word index, second list is is an
                                    * unordered bag, values in second list are
                                    * how many characters into the word we
                                    * are *)
  val solve : Board.state -> goal -> solution
end
