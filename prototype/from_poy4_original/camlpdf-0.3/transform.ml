(* \chaptertitle{Transform}{Affine transforms in 2D} *)

(* This module provides affine transformation on cartesian coordinates, using
the standard methods given in \cite{Foley96}. Two patterns of use are supported:
building a single matrix from the composition of the desired transformation
operations and then using it repeatedly (preferable when one wishes to
transform many points); and transforming a point directly from the
transformation operations (requires no state at the caller, so simpler). *)
open Utility

(* \section{Types} *)

(* \intf Individual transformation operations. *)
type transform_op =
  | Scale of (float * float) * float * float  (*r centre, x scale, y scale. *)
  | Rotate of (float * float) * float         (*r centre, angle in radians. *)
  | Translate of float * float                (*r change in x, change in y. *)
  | ShearX of (float * float) * float         (*r centre, x shear. *)
  | ShearY of (float * float) * float         (*r centre, y shear. *)

(* \intf A transform is a list of operations $t_n::t_{n-1}::\ldots::t_2::t_1$. which
means $t_1$ followed by $t_2$ etc. *)
type transform = transform_op list

(* \intf The matrix
\scalebox{.7}{\(\left( \begin{array}{ccc} a & c & e \\ b & d & f \\ 0 & 0 & 1
\end{array} \right) \)} for affine transforms in 2D homogeneous coordinates.*)
type transform_matrix =
  {a : float; b : float; c : float; d : float; e : float; f : float}

(* \section{Printers} *)

(* Debug printers for transformation operations. *)
let string_of_trop = function
  | Scale ((x, y), sx, sy) ->
      Printf.sprintf "Scale about (%f, %f) by %f in x and %f in y\n" x y sx sy
  | Rotate ((x, y), a) ->
      Printf.sprintf "Rotate by %f about (%f, %f)\n" a x y
  | Translate (dx, dy) ->
      Printf.sprintf "Translate by %f, %f\n" dx dy
  | ShearX ((x, y), sx) ->
      Printf.sprintf "Shear in X about (%f, %f), proportionality constant %f\n" x y sx
  | ShearY ((x, y), sy) ->
      Printf.sprintf "Shear in Y about (%f, %f), proportionality constant %f\n" x y sy

(* \intf Same for transforms. *)
let string_of_transform tr =
  fold_left ( ^ ) "" (rev_map string_of_trop tr)

(* \section{Building and manipulating transforms} *)

(* \intf The identity transform. *)
let i = []

(* \intf Compose a transformation operation [t] onto an existing transform [ts]. We
perform a simple optimisation --- combining like with like at the head. *)
let compose t = function
  | [] -> [t]
  | h::r ->
      match h, t with
      | Translate (dx, dy), Translate (dx', dy') ->
          Translate (dx +. dx', dy +. dy')::r
      | Scale (p, sx, sy), Scale (p', sx', sy') when p = p' ->
          Scale (p, sx *. sx', sy *. sy')::r
      | Rotate (p, a), Rotate (p', a') when p = p' ->
          Rotate (p, a +. a')::r
      | ShearX (p, a), ShearX (p', a') when p = p' ->
          ShearX (p, a +. a')::r
      | ShearY (p, a), ShearY (p', a') when p = p' ->
          ShearY (p, a +. a')::r
      | _ -> t::h::r

(* \intf Append two transforms. The result is all operations in the second argument
followed by all operations in the first. *)
let append = ( @ )

(* The identity transformation matrix \scalebox{.7}{\(\left( \begin{array}{ccc} 1
 & 0 & 0 \\ 0 & 1 & 0 \\ 0 & 0 & 1 \end{array} \right) \)}. *)
let i_matrix =
  {a = 1.; c = 0.; e = 0.; b = 0.; d = 1.; f = 0.}

(* \intf Compose two matrices. Applying the result is equivalent to applying [m]
then [m']. *)
let matrix_compose m' m =
  {a = m'.a *. m.a +. m'.c *. m.b;
   c = m'.a *. m.c +. m'.c *. m.d;
   e = m'.a *. m.e +. m'.c *. m.f +. m'.e;
   b = m'.b *. m.a +. m'.d *. m.b;
   d = m'.b *. m.c +. m'.d *. m.d;
   f = m'.b *. m.e +. m'.d *. m.f +. m'.f}

(* \intf String of matrix *)
let string_of_matrix m =
  Printf.sprintf "%f, %f, %f, %f, %f, %f" m.a m.b m.c m.d m.e m.f

(* \intf Invert a matrix. The exeption: *)
exception NonInvertable

(* \intf And the function. *)
let matrix_invert m =
  let det =
    let divisor = m.a *. m.d -. m.b *. m.c in
      if divisor = 0. then raise NonInvertable else
        match 1. /. divisor with
        | 0. -> raise NonInvertable
        | d -> ( *. ) d
  in
    {a = det m.d;
     b = det ~-.(m.b);
     c = det ~-.(m.c);
     d = det m.a;
     e = det (m.c *. m.f -. m.d *. m.e);
     f = det (m.b *. m.e -. m.a *. m.f)}

(* These functions build matrices for the transformation operations defined
above. \smallgap *)

(* \intf Translate by [(tx, ty)] *)
let mktranslate tx ty =
  {i_matrix with e = tx; f = ty}

(* \intf Scale about an origin [(ox, oy)] by x factor [sx] and y factor [sy]. *)
let mkscale (ox, oy) sx sy =
  let translate = mktranslate ~-.ox ~-.oy
  and translateback = mktranslate ox oy in
    matrix_compose
      translateback
      (matrix_compose {i_matrix with a = sx; d = sy} translate)

(* \intf Rotate about an origin [(ox, oy)] by angle (in radians) [angle]. *)
let mkrotate (ox, oy) angle =
  let translate = mktranslate ~-.ox ~-.oy
  and translateback = mktranslate ox oy in
    matrix_compose
      translateback
      (matrix_compose
         {i_matrix with a = cos angle; c = ~-. (sin angle);
         b = sin angle; d = cos angle}
         translate)

(* \intf Skew in x about an origin [(ox, oy)] by [factor]. *)
let mkshearx (ox, oy) factor =
  let translate = mktranslate ~-.ox ~-.oy
  and translateback = mktranslate ox oy in
    matrix_compose
      translateback
      (matrix_compose {i_matrix with c = factor} translate)
    
(* \intf Skew in y about an origin [(ox, oy)] by [factor]. *)
let mksheary (ox, oy) factor =
  let translate = mktranslate ~-.ox ~-.oy
  and translateback = mktranslate ox oy in
    matrix_compose
      translateback
      (matrix_compose {i_matrix with b = factor} translate)

(* \intf Use the preceeding functions to make a matrix from a transformation
operation. *)
let matrix_of_op = function
  | Scale (c, sx, sy) -> mkscale c sx sy
  | Rotate (c, a) -> mkrotate c a
  | Translate (dx, dy) -> mktranslate dx dy
  | ShearX (c, a) -> mkshearx c a
  | ShearY (c, a) -> mksheary c a

(* \intf Transform a point [(x, y)] with a matrix [m]. *)
let transform_matrix m (x, y) =
  x *. m.a +. y *. m.c +. m.e,
  x *. m.b +. y *. m.d +. m.f

(* \intf Method 1. When transforming many points, it makes sense to calculate the
composition of the transformation matrices and then apply this to each of the
points. *)
let matrix_of_transform tr =
  let matrices = map matrix_of_op tr in
    fold_left matrix_compose i_matrix matrices

(* \intf Method 2. Transform a point [p] by a transformation [ts]. This is faster
when we wish to transform a few points. It requires no state at the caller. *)
let transform ts (x, y) =
  let x = ref x and y = ref y in
    iter
      (function
       | Scale ((cx, cy), sx, sy) ->
             let x' = !x -. cx and y' = !y -. cy in
               let x'' = x' *. sx and y'' = y' *. sy in
                 x := x'' +. cx;
                 y := y'' +. cy
       | Rotate ((cx, cy), a) ->
           let cosine = cos a and sine = sin a in
             let invsine = ~-.sine in
               let x' = !x -. cx and y' = !y -. cy in
                 let x'' = x' *. cosine +. y' *. invsine
                 and y'' = x' *. sine +. y' *. cosine in
                   x := x'' +. cx;
                   y :=  y'' +. cy
       | Translate (dx, dy) ->
           x := !x +. dx; y := !y +. dy
       | ShearX ((cx, cy), a) ->
           let x' = !x -. cx and y' = !y -. cy in
             let x'' = x' +. y' *. a and y'' = y' in
               x := x'' +. cx;
               y := y'' +. cy
       | ShearY ((cx, cy), a) ->
           let x' = !x -. cx and y' = !y -. cy in
             let x'' = x' and y'' = x' *. a +. y' in
               x := x'' +. cx;
               y :=  y'' +. cy)
      (rev ts);
    !x, !y

(* \section{Decomposition and Recomposition} *)
    
(* \intf Decompose a matrix to a scale, aspect, rotation, shear and translation. *)
let decompose m =
  let axb = m.a *. m.d -. m.c *. m.b
  and moda = sqrt (m.a *. m.a +. m.b *. m.b)
  and modb = sqrt (m.c *. m.c +. m.d *. m.d)
  and adotb = m.a *. m.c +. m.b *. m.d in
    let scale = axb /. moda in
      let aspect =
        if fabs scale = 0. then 1. else moda /. fabs scale
      and rotation =
        atan2 m.b m.a
      and shear =
        if moda *. modb = 0. then 0. else
          pi /. 2. -. acos (adotb /. (moda *. modb))
      in
        safe_float scale,
        safe_float aspect,
        safe_float rotation,
        safe_float shear,
        safe_float m.e,
        safe_float m.f

(* \intf Rebuild a matrix from those components. *)
let recompose scale aspect rotation shear tx ty =
  let scale_aspect_shear =
    {i_matrix with
      a = fabs scale *. aspect;
      c = scale *. tan shear;
      d = scale}
  in
    let rotated =
      matrix_compose (mkrotate (0., 0.) rotation) scale_aspect_shear
    in
      matrix_compose (mktranslate tx ty) rotated

