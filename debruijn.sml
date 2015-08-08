
structure DeBruijnSequence =
struct

  fun debruijn (len, radix) =
    let
      val a = Array.array (len * radix + 1, 0)
      fun update (arr, x, u) =
        if x < 0
        then print ("x < 0: " ^ Int.toString x ^ "\n")
        else if x >= Array.length arr
             then print ("x > " ^ Int.toString (Array.length arr) ^ ": " ^
                         Int.toString x ^ "\n")
             else Array.update (arr, x, u)
      fun sub (arr, x) =
        if x < 0
        then (print ("sub x < 0: " ^ Int.toString x ^ "\n"); raise Match)
        else if x >= Array.length arr
             then (print ("sub x > " ^ Int.toString (Array.length arr) ^ ": " ^
                          Int.toString x ^ "\n");
                   raise Match)

             else Array.sub (arr, x)

      val revout = ref nil
      fun db (t, p) =
        if t > len
        then
           if len mod p = 0
           then
             Util.for 1 p (* (p + 1) *)
             (fn j =>
              revout := sub(a, j) :: !revout)
           else ()
        else
          let in
            update(a, t, sub(a, t - p));
            db (t + 1, p);
            Util.for (sub(a, t - p) + 1) (radix - 1)
            (fn j =>
             let in
               update(a, t, j);
               db (t + 1, t)
             end)
          end

      val () = db (1, 1)
      val r = rev (!revout)
    in

      (* The above generates a sequence that, if circular,
         contains all substrings exactly once. But we want a single
         sequence. Append enough beginning chars to achieve that. *)
      r @ List.take (r, len - 1)
    end

end
