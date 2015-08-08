(* Flying Spaghetti Monsters *)
structure FSM =
struct

  (*

     datatype syntax
       = Group of syntax
       | Alt of syntax list
       | Concat of syntax list
       | Interval of (syntax * int * int option)
       | Option of syntax       (* == Interval(re, 0, SOME 1) *)
       | Star of syntax (* == Interval(re, 0, NONE) *)
       | Plus of syntax (* == Interval(re, 1, NONE) *)
       | MatchSet of CharSet.set
       | NonmatchSet of CharSet.set
       | Char of char
       | Begin
       | End
       *)

  structure RS = RegExpSyntax
  fun stringtosyntax s =
    RS.Concat (map RS.Char (explode s))


  val syntax =
    RS.Concat[RS.Star (RS.MatchSet (RS.allChars)),
              (RS.Alt [stringtosyntax "monster",
                       stringtosyntax "spaghetti"])]

  val dfa = Dfa.build syntax

end