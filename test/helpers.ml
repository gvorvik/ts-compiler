(** [string_of_list formatter l] returns a string representation of the list [l]
    where each element is formatted using the function [formatter]. Used for pretty
    printing failed assertions. *)
let string_of_list formatter = function
  | [] -> "[]"
  | l -> "[ " ^ (l |> List.map formatter |> String.concat "; ") ^ " ]"
