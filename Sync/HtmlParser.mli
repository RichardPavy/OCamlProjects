type expr = string Parser.Template.t
 and attributes = (expr * expr) list
 and element = Tag of tag | Text of expr | Blank
 and tag = { name : expr ;
	     attributes : attributes ;
	     content : element Parser.Template.t list }

type 'a template =
  (** Token: $widget. String replacement in the template, before compilation. *)
  ?widget: (string * (string -> string)) list ->

  (** Token: $html. No transformation. *)
  ?const: (string * ('a -> string)) list ->

  (** Token: $name. No transformation. Tag or attribute name only. *)
  ?name: (string * ('a -> string)) list ->

  (** Token: $expr. Escapes html entities. Attribute value or htmltext. *)
  ?expr: (string * ('a -> string)) list ->

  (** Token: $html. No transformation, same as const. *)
  ?html: (string * ('a -> Buffer.t -> unit)) list ->

  unit ->
  'a -> Buffer.t -> unit

val compile : template: string -> 'a template

val page :
  title: string ->
  body: string ->
  'a template
