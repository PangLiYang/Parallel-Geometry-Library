fun vertexToString (x, y, z) =
    "(" ^ Real.toString x ^ ", " ^ Real.toString y ^ ", " ^ Real.toString z ^ ")"

fun faceToString (i, j, k) =
    "(" ^ Int.toString i ^ ", " ^ Int.toString j ^ ", " ^ Int.toString k ^ ")"

fun listToString f lst =
    "[" ^ String.concatWith ", " (List.map f lst) ^ "]"

fun meshToString {vertices, faces} =
    "Vertices:\n" ^ listToString vertexToString vertices ^ "\n" ^
    "Faces:\n" ^ listToString faceToString faces

fun isSpace c = c = #" " orelse c = #"\t" orelse c = #"\n" orelse c = #"\r"

fun dropWhile _ [] = []
  | dropWhile pred (x::xs) =
      if pred x then dropWhile pred xs else x::xs

fun takeWhile _ [] = []
  | takeWhile pred (x::xs) =
      if pred x then x :: takeWhile pred xs else []

fun trimLeft s =
    case String.explode s of
        [] => ""
      | cs => String.implode (dropWhile isSpace cs)

fun trimRight s =
    case String.explode s of
        [] => ""
      | cs => String.implode (List.rev (dropWhile isSpace (List.rev cs)))

fun trim s = trimRight (trimLeft s)

fun parse_vertex line =
    case String.tokens (fn c => c = #" ") line of
        ("v" :: x :: y :: z :: _) =>
            (case (Real.fromString x, Real.fromString y, Real.fromString z) of
                (SOME x', SOME y', SOME z') => SOME (x', y', z')
              | _ => NONE)
      | _ => NONE

fun parse_face line =
    case String.tokens (fn c => c = #" ") line of
        ("f" :: v1 :: v2 :: v3 :: _) =>
            (case (Int.fromString v1, Int.fromString v2, Int.fromString v3) of
                (SOME v1', SOME v2', SOME v3') => SOME (v1' - 1, v2' - 1, v3' - 1)
              | _ => NONE)
      | _ => NONE

fun read_triangle_mesh filename =
    let
        val file = TextIO.openIn filename
        val content = TextIO.inputAll file
        val _ = TextIO.closeIn file
        val lines = List.map trim (String.fields (fn c => c = #"\n") content)

        fun loop [] (vs, fs) = (rev vs, rev fs)
          | loop (line::rest) (vs, fs) =
                (case parse_vertex line of
                    SOME v => loop rest (v::vs, fs)
                  | NONE =>
                    (case parse_face line of
                        SOME f => loop rest (vs, f::fs)
                      | NONE => loop rest (vs, fs)))
        
        val (v, f) = loop lines ([], [])
        val vertices_seq = Seq.fromList v
        val faces_seq = Seq.fromList f
    in
        (vertices_seq, faces_seq)
    end
