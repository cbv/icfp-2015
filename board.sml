structure Board :> BOARD =
struct

  exception Board of string

  datatype move = E | W | SE | SW
  datatype turn = CW | CCW

  fun movestring E = "E"
    | movestring W = "W"
    | movestring SE = "SE"
    | movestring SW = "SW"
  fun moveorder (E, E) = EQUAL
    | moveorder (E, _) = LESS
    | moveorder (_, E) = GREATER
    | moveorder (W, W) = EQUAL
    | moveorder (W, _) = LESS
    | moveorder (_, W) = GREATER
    | moveorder (SE, SE) = EQUAL
    | moveorder (SE, _) = LESS
    | moveorder (_, SE) = GREATER
    | moveorder (SW, SW) = EQUAL

  fun turnstring CW = "CW"
    | turnstring CCW = "CCW"

  fun turnorder (CW, CW) = EQUAL
    | turnorder (CW, _) = LESS
    | turnorder (_, CW) = GREATER
    | turnorder (CCW, CCW) = EQUAL

end
