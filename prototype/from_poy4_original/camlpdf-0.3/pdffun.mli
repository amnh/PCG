(** Parsing and Evaluating PDF functions *)
type calculator =
  | If of calculator list
  | IfElse of calculator list * calculator list
  | Bool of bool
  | Float of float
  | Int of int32
  | Abs
  | Add
  | Atan
  | Ceiling
  | Cos
  | Cvi
  | Cvr
  | Div
  | Exp
  | Floor
  | Idiv
  | Ln
  | Log
  | Mod
  | Mul
  | Neg
  | Round
  | Sin
  | Sqrt
  | Sub
  | Truncate
  | And
  | Bitshift
  | Eq
  | Ge
  | Gt
  | Le
  | Lt
  | Ne
  | Not
  | Or
  | Xor
  | Copy
  | Exch
  | Pop
  | Dup
  | Index
  | Roll

type sampled =
  {size : int list; 
   order : int;
   encode : float list;
   decode : float list;
   bps : int;
   samples : int32 array}

and interpolation =
  {c0 : float list;
   c1 : float list;
   n : float}

and stitching =
  {functions : pdf_fun list;
   bounds : float list;
   stitch_encode : float list}

and pdf_fun_kind =
  | Interpolation of interpolation
  | Stitching of stitching
  | Sampled of sampled
  | Calculator of calculator list

and pdf_fun =
  {func : pdf_fun_kind;
   domain : float list;
   range : float list option}
(** The type of functions. *)

val parse_function : Pdf.pdfdoc -> Pdf.pdfobject -> pdf_fun

(** Raised from [eval_function] (see below) in the case of inputs which don't
match the evaluation *)
exception BadFunctionEvaluation of string

(** Evaluate a function given a list of inputs. *)
val eval_function : pdf_fun -> float list -> float list

(**/**)
val print_function : pdf_fun -> unit

