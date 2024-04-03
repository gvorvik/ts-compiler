type t =
  | EOF
  (* Literals *)
  | Iden of string
  | Int of int
  (* Keywords *)
  | Const
  (* Operators *)
  | Add
  | Equal
  (* Delimiters *)
  | Semi
