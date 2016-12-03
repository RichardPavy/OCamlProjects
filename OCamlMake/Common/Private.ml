(** Helper functions to deal with private files (intermediary build files). *)

open Utils

(** Name of the private folder that will contain intermediary build files. *)
let private_folder = "build"

(** Returns true if the given file is in the private folder. *)
let is_private =
  let aux = Utils.starts_with (private_folder ^ "/") in
  fun file -> let file = File.to_string file in
	      aux file || file = private_folder

(** Returns true if the given file is not in the private folder. *)
let is_public file = not (is_private file)

(**
 * Returns the public {!Utils_File.t}
 * that corresponds to the given private file.
 *
 * Fails if the given file is already a public file. *)
let to_public file =
  assert (Utils.dcheck (is_private file)
		       "File %s is not in the private folder (%s/...)"
		       (File.to_string file) private_folder);
  let file = File.to_string file in
  if file = private_folder then
    File.root
  else
    let pfl = String.length private_folder + 1
    and fl = String.length file in
    String.sub file pfl (fl-pfl)
    |> File.parse

(**
 * Returns the private {!Utils_File.t}
 * that corresponds to the given private file.
 *
 * Fails if the given file is already a private file. *)
let to_private file =
  assert (Utils.dcheck (is_public file)
		       "File %s is in the private folder (%s/...)"
		       (File.to_string file) private_folder);
  File.parsef "%s/%s" private_folder (File.to_string file)

let () =
  assert (Log.dlog "Testing Private");
  assert begin
      [ "is_private",
        (fun () -> File.parsef "%s/file.ml" private_folder |> is_private) ;

        "is_public",
        (fun () -> File.parsef "public/%s/file.ml" private_folder |> is_public) ;

        "to_public",
        begin fun () ->
        (File.parse "abc/file.ml")
        = (File.parsef "%s/abc/file.ml" private_folder |> to_public)
        end;
      ] |> Asserts.test
    end
