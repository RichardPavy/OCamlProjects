open Utils

type kind = ..

let flags = Cache.fn (fun (* kind *) _ -> Property.create ())

let add ~kind
        ?package
        ?predicate
        generator =
  let generator =
    match predicate with
    | None -> generator
    | Some p -> fun file -> if p file
                            then generator file
                            else Iterable.empty ()
  in
  (flags kind).Property.add ?package generator

let add_file ~kind ~file ~generator =
  add ~kind
      ~package: (File.parent file)
      ~predicate: (Predicate.equals file)
      generator

let get kind ?(sep = " ") target =
  (flags kind).Property.get target
  |> Utils.join sep
