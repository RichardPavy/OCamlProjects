open Parser.Infix
module Rope = Parser.Rope
module Template = Parser.Template

type expr = string Template.t
 and attributes = (expr * expr) list
 and element = Tag of tag | Text of expr | Blank
 and tag = { name : expr ;
	     attributes : attributes ;
	     content : element Template.t list }

let template = Template.template
let name = !%"[a-zA-Z:_][a-zA-Z0-9:_.-]*" |> template "name"
let expr = string |> template "expr"

let attribute = name %>. ispace %>. !@"=" %>. ispace %> expr
let attributes = attribute %% ispace

let element, set_element_parser = Parser.Builder.parse_ref ()
let elements = !*(Template.token "widget" %|% template "html" element)

let tag =
  let open_tag = (?<> !%"<[/!]") %.> !@"<" %>. ispace %.> name %>. ispace %> attributes %>. ispace
  and end_self_closing = !@"/>" %$ fun _ -> `SelfClosing
  and tag_content = !@">" %.> elements %>. !@"</" %>. ispace %> name %>. ispace %>. !@">"
		    %$ fun x -> `TagContent x
  in open_tag %> (end_self_closing %|% tag_content)
     %$ begin fun ((name, attributes), t) ->
	      match t with
	      | `SelfClosing -> { name ; attributes ; content = [] }
	      | `TagContent (content, name2) ->
		 if name = name2
		 then { name ; attributes ; content }
		 else failwith "Closing tag not found."
	end

let text = (!%"[^ \r\n\t<]+"
	    %|% (!@"<!DOCTYPE" %> !%"[^>]*>" %$ fun (a, b) -> a ^ b))
	   |> template "expr"

let () =
  (tag %$ fun t -> Tag t)
  %|% (space %$ fun _ -> Blank)
  %|% (text %$ fun t -> Text t)
  |> set_element_parser

module Flatten =
  struct
    open Rope.Infix
    let flatten_template (f : 'a -> string Template.t Rope.t) template =
      match template with
      | Template.Static n -> f n
      | Template.Token t -> !.(Template.Token t)

    let flatten_name (s : string) = !.(Template.Static s)
    let flatten_expr (s : string) = !.(Template.Static s)

    let flatten_attributes attributes =
      List.map begin fun (key, value) ->
		     !.(Template.Static " ")
		     ++ (flatten_template flatten_name key)
		     ++. (Template.Static "='")
		     ++ (flatten_template flatten_expr value)
		     ++. (Template.Static "'")
	       end
	       attributes
      |> (!@)

    let rec flatten_tag tag =
      !.(Template.Static "<")
      ++ (flatten_template flatten_name tag.name)
      ++ (flatten_attributes tag.attributes)
      ++ begin
	  if tag.content = []
	  then !.(Template.Static "/>")
	  else !.(Template.Static ">")
	       ++ (flatten_content tag.content)
	       ++.(Template.Static "</")
	       ++ (flatten_template flatten_name tag.name)
	       ++.(Template.Static ">")
	end
    and flatten_content elements =
      (List.map (flatten_template flatten_element) elements) |> (!@)
    and flatten_element element =
      match element with
      | Blank -> !.(Template.Static " ")
      | Text t -> flatten_template flatten_expr t
      | Tag t -> flatten_tag t

    let optimize rope =
      Rope.fold
	begin fun accu item ->
	      match item with
	      | Template.Token _ -> item :: accu
	      | Template.Static s ->
		 match accu with
		 | Template.Static s0 :: q -> Template.Static (s ^ s0) :: q
		 | _ -> Template.Static s :: accu
	end
	[]
	rope

    type 'context compiled_template =
      | Static of string
      | Token of ('context -> Buffer.t -> unit)

    let compile ~template ~args =
      let template = (elements %< template)
		     |> flatten_content
		     |> optimize in
      let nbargs = List.length args in
      let unused_args = Hashtbl.create nbargs in
	let h = Hashtbl.create nbargs in
      let arg_to_fun =
	List.iter (fun (arg, f) -> Hashtbl.add h arg f) args;
	List.iter (fun (arg, _) -> Hashtbl.replace unused_args arg ()) args;
	fun token -> try let f = Hashtbl.find h token in
			 Hashtbl.remove unused_args token;
			 f
		     with Not_found -> Printf.printf "Not found: %s\n" token.Template.name;
				       raise Not_found
      in
      let compiled_template =
	List.map
	  begin function
	    | Template.Static value -> Static value
	    | Template.Token token -> Token (arg_to_fun token)
	  end
	  template
	|> Array.of_list
      in
      if Hashtbl.length unused_args <> 0 then
	(Hashtbl.iter
	   (fun k v -> Printf.printf "Unused: %s:%s\n" k.Template.scope k.Template.name)
	   unused_args;
	 failwith "There are unused arguments.");
      fun context b ->
      Array.iter
	begin function
	  | Token t -> t context b
	  | Static s -> Buffer.add_string b s
	end
	compiled_template
  end

type 'a template =
  ?widget: (string * (string -> string)) list ->
  ?const: (string * ('a -> string)) list ->
  ?name: (string * ('a -> string)) list ->
  ?expr: (string * ('a -> string)) list ->
  ?html: (string * ('a -> Buffer.t -> unit)) list ->
  unit ->
  'a -> Buffer.t -> unit

let compile ~template ?(widget = []) ?(const = []) ?(name = []) ?(expr = []) ?(html = []) () =
  let template =
    match widget with
    | [] -> template
    | _ ->
       let widgets =
	 let t = Hashtbl.create 10 in
	 List.iter (fun (name, widget) -> Hashtbl.add t name widget) widget; t
       in
       let transformed_template =
	 List.rev_map
	   begin fun item ->
		 match item with
		 | Template.Token { Template.scope = scope ; name } ->
		    if scope = "widget" && Hashtbl.mem widgets name
		    then Hashtbl.find widgets name name
		    else Printf.sprintf "$%s:%s" scope name
		 | Template.Static string -> string
	   end
	   begin (elements %< template)
		 |> Flatten.flatten_content
		 |> Flatten.optimize
	   end
	 |> List.rev
       in
       let b = Buffer.create (String.length template) in
       List.iter (Buffer.add_string b) transformed_template;
       Buffer.contents b
  in
  let fold accu f l = List.fold_left f l accu in
  let args =
    []
    |> (fold
	  const
          begin fun accu (name, f) ->
		let f context b = context |> f |> Buffer.add_string b in
		({ Template.scope = "html" ; name }, f) :: accu
	  end)
    |> (fold
	  name
          begin fun accu (name, f) ->
		let f context b = context |> f |> Buffer.add_string b in
		({ Template.scope = "name" ; name }, f) :: accu
	  end)
    |> (fold
	  expr
          begin fun accu (name, f) ->
		let f context b = context |> f |> HtmlEntities.encode |> Buffer.add_string b in
		({ Template.scope = "expr" ; name }, f) :: accu
	  end)
    |> (fold
	  html
          begin fun accu (name, f) ->
		({ Template.scope = "html" ; name }, f) :: accu
	  end)
  in Flatten.compile ~template ~args

let page_aux =
  compile ~template: {|<!DOCTYPE html>
<html>
  <head>
    <title>$html:title</title>
  </head>
  <body>$html:body</body>
</html>|}
	  ~const: [ "title", fst ; "body", snd ]
	  ()

let page ~title ~body =
  let b = Buffer.create 80 in
  page_aux (title, body) b;
  compile ~template: (Buffer.contents b)

let () =
  assert begin
      let module Test = struct
	  type t = { link : string ; title : string ;
	             link2 : string ; title2 : string }
	end
      in
      let open Test in
      let simple_page =
	compile
	  ~template: {html|<p>
		      <a href=$expr:link>Go to
		      <$name:style>$expr:title</$name:style>
		      </a>
		      <br/>
		      <a href=$expr:link2>Go to
                      <$name:style2>$expr:title2</$name:style2>
		      </a>
		      </p>|html}
	  ~expr: [ "link", (fun data -> data.link) ;
		   "title", (fun data -> data.title) ;
		   "link2", (fun data -> data.link2) ;
		   "title2", (fun data -> data.title2) ]
	  ~name: [ "style", (fun _ -> "i") ;
		   "style2", (fun _ -> "b") ]
	  ()
      in
      ( = )
	("<p> <a href='link &amp;1'>Go to <i>title &amp;1</i> </a> <br/> "
	 ^ "<a href='link &amp;2'>Go to <b>title &amp;2</b> </a> </p>")
	(let b = Buffer.create 80 in
	 simple_page
	   { link = "link &1" ; title = "title &1" ;
	     link2 = "link &2" ; title2 = "title &2" }
	   b;
	Buffer.contents b)
    end

let () =
  assert begin
      let input_text name =
	Str.global_replace
	  (Str.regexp_string "$$name")
	  name
	  {|<input type="text" name="$$name" value=$expr:$$name/>|}
      in
      let module Test = struct
	  type t = { page_title: string ;
		     name: string ;
		     last_name: string ;
		     title: string option }
	end
      in
      let open Test in
      let form =
	let with_default d x = match x with None -> d | Some x -> x in
	compile
	  ~template: {|<div>
		      <p>Welcome to $expr:page-title</p>
		      $widget:name
		      $widget:last-name
		      $widget:title
		      </div>|}
	  ~widget: [ "name", input_text ;
		     "last-name", input_text ;
		     "title", input_text ]
	  ~expr: [ "page-title", (fun form -> form.page_title) ;
		   "name", (fun form -> form.name) ;
		   "last-name", (fun form -> form.last_name) ;
		   "title", (fun form -> with_default "" form.title) ]
	  ()
      in
      let actual =
	let b = Buffer.create 80 in
	form { page_title = "<Test page>" ;
	       name = "<Richard>" ;
	       last_name = "<Pavy>" ;
	       title = None}
	     b;
	Buffer.contents b
      and expected =
	"<div> "
	^ "<p>Welcome to &lt;Test page&gt;</p> "
	^ "<input type='text' name='name' value='&lt;Richard&gt;'/> "
	^ "<input type='text' name='last-name' value='&lt;Pavy&gt;'/> "
	^ "<input type='text' name='title' value=''/> "
	^ "</div>"
      in
      actual = expected
    end
