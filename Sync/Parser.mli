exception ParseError of string

type 'a parser
 and ('a, 'b) alt_result = Left of 'a | Right of 'b
 and kind =
   | String of string
   | Regex of Str.regexp
   | Alt of kind * kind
   | Transform of kind
   | Seq of kind * kind
   | Star of kind
   | List of kind * kind
   | Before of kind
   | NotBefore of kind
   | Ref
   | Undef
   | Const

module Builder :
sig
  val parse_transform : ('a -> 'b) -> 'a parser -> 'b parser
  val parse_string : string -> string parser
  val parse_regex : Str.regexp -> string parser
  val parse_before : 'a parser -> unit parser
  val parse_not_before : 'a parser -> unit parser
  val parse_alt : 'a parser -> 'b parser -> ('a, 'b) alt_result parser
  val parse_seq : 'a parser -> 'b parser -> ('a * 'b) parser
  val parse_star : 'a parser -> 'a list parser
  val parse_plus : 'a parser -> 'a list parser
  val parse_ignore : 'a parser -> unit parser
  val parse_list : 'a parser -> 'b parser -> 'a list parser
  val parse_const : 'a -> 'a parser
  val parse_opt : 'a parser -> 'a -> 'a parser
  val parse_ref : unit -> 'a parser * ('a parser -> unit)
end

module Infix :
sig
  (** Transform parser *)
  val ( %$ ) : 'a parser -> ('a -> 'b) -> 'b parser

  (** String parser *)
  val ( !@ ) : string -> string parser

  (** Regexp parser *)
  val ( !% ) : string -> string parser

  (** Before *)
  val ( ?= ) : 'before parser -> unit parser

  (** Not before *)
  val ( ?<> ) : 'not_before parser -> unit parser

  (** Alt parser / different types *)
  val ( %| ) : 'a parser -> 'b parser -> ('a, 'b) alt_result parser

  (** Alt parser / same types *)
  val ( %|% ) : 'a parser -> 'a parser -> 'a parser

  (** Sequence parser *)
  val ( %> ) : 'a parser -> 'b parser -> ('a * 'b) parser

  (** Star parser *)
  val ( !* ) : 'a parser -> 'a list parser

  (** Plus parser *)
  val ( !+ ) : 'a parser -> 'a list parser

  (** List parser *)
  val ( %% ) : 'a parser -> 'sep parser -> 'a list parser

  (** Const parser *)
  val ( != ) : 'const -> 'const parser

  (** Optional parser *)
  val ( %? ) : 'a parser -> 'a -> 'a parser

  (** Sequnce parser, ignore second *)
  val ( %>. ) : 'x parser -> 'ignored parser -> 'x parser

  (** Sequnce parser, ignore first *)
  val ( %.> ) : 'ignored parser -> 'x parser -> 'x parser

  (** Run parser *)
  val ( %< ) : 'value parser -> string -> 'value

  val space : string parser
  val ispace : string parser
  val string : string parser
  val integer : int parser
  val float : float parser
end

module Template :
sig
  type token = { scope : string; name : string; }
  type 'a t =
    | Static of 'a
    | Token of token

  (** Creates a parser that matches tokens of the given scope. *)
  val token : string -> 'a t parser

  (** [template scope p] Creates a parser that parses either tokens or values. *)
  val template : string -> 'a parser -> 'a t parser
end

module Rope :
sig
  type 'a t
  val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  module Infix :
  sig
    val ( ++ ) : 'a t -> 'a t -> 'a t
    val ( ++. ) : 'a t -> 'a -> 'a t
    val ( ++@ ) : 'a t -> 'a t list -> 'a t
    val ( !. ) : 'a -> 'a t
    val ( !@ ) : 'a t list -> 'a t
  end
end
