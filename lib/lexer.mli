(** The [Lexer] module provides functions for tokenizing (a subset of) TypeScript source code.

    It defines a lexer type [t] and functions for creating a lexer and consuming tokens.

    The lexer is designed to be immutable: consuming a token returns a new lexer and the
    consumed token. When there are no more tokens, the lexer returns [Token.EOF]. *)

type t

val of_string : string -> t
(** [of_string s] creates a new lexer for string [s]. *)

val next_token : t -> t * Token.t
(** [next_token lexer] consumes the next token from [lexer] and returns a pair
    of the updated lexer and the consumed token. Returns the input [lexer] and [Token.EOF] if
    there are no more tokens. *)

val peek_token : t -> Token.t
(** [peek_token lexer] returns the next token from [lexer]
    without consuming it. Returns [Token.EOF] if there are no more tokens. *)

val show_token : Token.t -> string
(** [show_token token] returns a string representation of [token] for pretty printing. *)
