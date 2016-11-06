type t = { name : string ;
	   content : string }

let files : (string, t) Hashtbl.t = Hashtbl.create 10
