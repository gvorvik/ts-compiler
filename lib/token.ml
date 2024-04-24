type t =
  | EOF
  | Arrow
  (* Literals *)
  | Iden of string
  | Int of int
  (* Keywords *)
  | Const
  (* Operators *)
  | BinOp of string
  | Assign
  | Inc_Dec
  | Equality
  (* Delimiters *)
  | Semi
