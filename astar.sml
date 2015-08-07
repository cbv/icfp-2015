signature PROBLEM = sig
  type node
  val compare_node : node * node -> order

  type move

  type goal

  val moves     : node -> move list
  val do_move   : node -> move -> node
  val move_cost : move -> real
  val heuristic : node -> goal -> real
  val is_goal   : goal -> node -> bool
end

functor AStar (P : PROBLEM) = struct
  structure Q = HeapFn(struct
    type priority = real
    val compare = Real.compare
  end)
  structure NS = SplaySetFn(struct
    type ord_key = P.node
    val compare  = P.compare_node
  end)
  structure M = SplayMapFn(struct
    type ord_key = P.node
    val compare  = P.compare_node
  end)
  fun move_history parent node = []
  type solve_state = {
    goal    : P.goal,
    queue   : P.node Q.heap,
    f       : real M.map,
    g       : real M.map,
    hit     : NS.set,
    parent  : (P.node * P.move) M.map,
    heapdex : Q.hand M.map
  }

  fun update_hit (s : solve_state) (hit : NS.set) = {
    goal    = #goal s,
    queue   = #queue s,
    f       = #f s,
    g       = #g s,
    hit     = hit,
    parent  = #parent s,
    heapdex = #heapdex s
  }

  fun update_heapdex s heapdex = {
    goal    = #goal s,
    queue   = #queue s,
    f       = #f s,
    g       = #g s,
    hit     = #hit s,
    parent  = #parent s,
    heapdex = heapdex
  }

  fun better_path target gt (s : solve_state) =
    case M.find (#heapdex s, target) of
        SOME(_) => M.lookup (#g s, target) > gt
      | NONE => true

  fun update_pos target (s : solve_state) =
    let val f = M.lookup (#f s, target) in
    case M.find (#heapdex s, target) of
        SOME (h) => (Q.adjust (#queue s) h f; s)
      | NONE => let val h = Q.insert (#queue s) f target in
                update_heapdex s (M.insert (#heapdex s, target, h))
                end
    end

  fun proc_move node (move, s) : solve_state =
    let val target = P.do_move node move in
    if NS.member (#hit s, target)
    then s
    else let val gt = (M.lookup (#g s, node)) + (P.move_cost move) in
         if better_path target gt s
         then update_pos target {
           goal    = #goal s,
           queue   = #queue s,
           g       = M.insert (#g s, target, gt),
           f       = M.insert (#f s, target, (gt + P.heuristic target
                                                               (#goal s))),
           hit     = #hit s,
           parent  = M.insert (#parent s, target, (node, move)),
           heapdex = #heapdex s
         }
         else s
         end
    end

  fun fst (x,_) = x

  fun pop s : (P.node * solve_state) option =
    case Q.min (#queue s) of
        NONE => NONE
      | SOME(_,n) => SOME(n, {
          goal    = #goal s,
          queue   = #queue s,
          g       = fst (M.remove (#g s, n)),
          f       = fst (M.remove (#f s, n)),
          hit     = #hit s,
          parent  = #parent s,
          heapdex = fst (M.remove (#heapdex s, n))
        })

  fun move_history parents node =
    case M.find (parents, node) of
        NONE => []
      | SOME (parent, move) => move :: (move_history parents parent)

  fun subsolve (s : solve_state) =
    case pop s of
        NONE => NONE (* No path exists *)
      | SOME(node, s') =>
        if P.is_goal (#goal s') node
          then SOME (List.rev (move_history (#parent s') node))
          else subsolve (List.foldr (proc_move node)
                                    (update_hit s (NS.add (#hit s', node)))

                                    (P.moves node))

  (* val solve : P.node -> P.goal -> P.move list option *)
  fun solve node goal = NONE

end
