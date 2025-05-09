structure Geometry3D =
struct

    type Vertex = real * real * real
    type Face = int * int * int
    type Mesh = Vertex Seq.t * Face Seq.t

    structure Vector =
    struct
      type t = real * real * real
      val zero = (0.0, 0.0, 0.0)

      fun toString (x, y, z) =
        "(" ^ Real.toString x ^ ", " ^ Real.toString y ^ ", " ^ Real.toString z ^ ")"

      fun eq_tuple_input ((x1, y1, z1), (x2, y2, z2)) : bool =
        (Real.== (x1, x2)) andalso (Real.== (y1, y2)) andalso (Real.== (z1, z2))

      fun add_tuple_input ((x1, y1, z1), (x2, y2, z2)) : t = (x1 + x2, y1 + y2, z1 + z2)
      
      fun add (x1, y1, z1) (x2, y2, z2) : t = (x1 + x2, y1 + y2, z1 + z2)
      fun sub (x1, y1, z1) (x2, y2, z2) : t = (x1 - x2, y1 - y2, z1 - z2)

      fun dot (x1, y1, z1) (x2, y2, z2) : real = x1 * x2 + y1 * y2 + z1 * z2
      fun cross (x1, y1, z1) (x2, y2, z2) : t = (y1 * z2 - z1 * y2,
                                                    z1 * x2 - x1 * z2,
                                                    x1 * y2 - y1 * x2)

      fun length (x, y, z) : real = Math.sqrt (x * x + y * y + z * z)

      fun scale (x, y, z) c : t = (c * x, c * y, c * z)

      fun normalize (x, y, z) : t =
        let
            val len = length (x, y, z)
        in
            if (Real.== (len, 0.0)) then (0.0, 0.0, 0.0)
            else (x / len, y / len, z/ len)
        end

      fun triangleArea (v1 : t) (v2 : t) (v3 : t) : real = 
        (length (cross (sub v2 v1) (sub v3 v1))) / 2.0

      fun angle_v1 (v1 : t) (v2 : t) (v3 : t) : real = 
        let
          val u = sub v2 v1
          val v = sub v3 v1
          val cos_theta = (dot u v) / ((length u) * (length v))
          val modifier = Real.min(1.0, Real.max(~1.0, cos_theta))
        in
          Math.acos(modifier)
        end
      
      fun voronoi_areas_v1 (v1 : t) (v2 : t) (v3 : t) : real = 
        let
          val a = angle_v1 v1 v2 v3
          val b = angle_v1 v2 v3 v1
          val c = angle_v1 v3 v1 v2
        in
          if a > Math.pi / 2.0 then
            (triangleArea v1 v2 v3) / 2.0
          else if b > Math.pi / 2.0 orelse c > Math.pi / 2.0 then
            (triangleArea v1 v2 v3) / 4.0
          else
            let
              val len_ba = length (sub v2 v1)
              val len_ca = length (sub v3 v1)
            in
              (len_ba * len_ba * (1.0 / Math.tan(c)) + len_ca * len_ca * (1.0 / Math.tan(b))) / 8.0
            end
        end
      
      fun barycentric_areas_v1 (v1 : t) (v2 : t) (v3 : t) : real = 
        (triangleArea v1 v2 v3) / 3.0

      fun cotangent (v1 : t) (v2 : t) : real = 
        (dot v1 v2) / (2.0 * (length (cross v1 v2) ) )

    end

end
