(**
 * Implementation for file properties.
 *
 * Properties are attached to {!Utils_File.t}. Each value {!t} contains a list
 * of generators that are used to generate properties. Properties are used to
 * implement flags.  *)

open Utils
module LL = Container_LinkedList

(** Type for generators: functions that generate properties. *)
type 'a generator = File.t -> 'a Iterable.t

(** Type for handles, allowing to deregister previously added generators. *)
type 'a handle = 'a generator LL.handle

(** Type for properties *)
type 'a t =
  { add: ?package: File.t ->
         'a generator ->
         'a handle ;
    (** Adds a property generator. If package is not given, the generator will
     * be called for all folders. *)

    get: File.t -> 'a Iterable.t
    (** Returns all the properties that apply to a file.
     *
     * Generators are called in insertion order.*)
  }

(** Creates a new property. *)
let create () =
  let generators = Cache.fn (fun (* package option *) _ -> LL.create()) in
  let add ?package generator =
    LL.add
      (generators package)
      generator
  and get target =
    let queue = Queue.create () in
    Iterable.iter
      (fun generator -> generator target
			|> Iterable.iter (fun e -> Queue.add e queue))
      (Iterable.concat
         (None |> generators |> LL.to_iterable)
         (Some (target |> File.parent) |> generators |> LL.to_iterable));
    Iterable.of_queue queue
  in { add ; get }

(**
 * Runs the given function and then removes all handles.
 *
 * Usage: {[
 *   [ property.add x ; priperty.add y ; ... ]
 *   |> process begin fun f () -> ... end ]} *)
let process f handles =
  try let result = f () in
      List.iter LL.remove handles;
      result
  with e ->
    List.iter LL.remove handles;
    raise e

let () =
  assert (Log.dlog "Testing Property");
  assert begin
      [ "test add/get",
        begin fun () ->
        let props = create () in
        props.add (fun file -> file |> File.to_string
                               |> Iterable.singleton)
        |> ignore;
        props.add (fun file -> (File.child file "x1") |> File.to_string
                               |> Iterable.singleton)
        |> ignore;
        props.add (fun file -> (File.child file "x2") |> File.to_string
                               |> Iterable.singleton)
        |> ignore;
        props.get (File.parse "hello") |> Utils.join ";"
        = "hello;hello/x1;hello/x2"
        end ;

        "test process",
        begin fun () ->
        let props = create () in
        [ props.add (fun file -> (File.child file "x2") |> File.to_string
                                 |> Iterable.singleton) ;
          props.add (fun file -> (File.child file "x1") |> File.to_string
                                 |> Iterable.singleton) ;
          props.add (fun file -> file |> File.to_string
                                 |> Iterable.singleton) ;
        ] |> process (fun () -> props.get (File.parse "hello") |> Utils.join ";"
                                = "hello;hello/x1;hello/x2")
        && props.get (File.parse "hello") |> Utils.join ";"
           = ""
        end ;
      ] |> Asserts.test
    end
