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

(* Helpers for building lexical tokens *)
module TokenHelpers = struct
  let is_number = function '0' .. '9' -> true | _ -> false
  let is_letter = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
  let acceptable_chars = function '_' -> true | _ -> false

  let is_valid_char x =
    x |> is_number || x |> is_letter || x |> acceptable_chars
end
