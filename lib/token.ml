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
  | Assign of string
  | Inc_Dec of string
  | Equality of string
  (* Delimiters *)
  | Semi
