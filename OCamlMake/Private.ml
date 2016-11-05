open Utils

let private_folder = "build"

let is_private =
  let aux = Utils.starts_with (private_folder ^ "/") in
  fun file -> let file = File.to_string file in
	      aux file || file = private_folder

let is_public file = not (is_private file)

let () =
  assert (File.parsef "%s/file.ml" private_folder |> is_private);
  assert (File.parsef "public/%s/file.ml" private_folder |> is_public)

let to_public file =
  assert (Utils.dcheck (is_private file)
		       "File %s is not in the private folder (%s/...)"
		       (File.to_string file) private_folder);
  let file = File.to_string file in
  if file = private_folder
  then File.root
  else
    let pfl = String.length private_folder + 1
    and fl = String.length file in
    String.sub file pfl (fl-pfl) |> File.parse

let to_private file =
  assert (Utils.dcheck (is_public file)
		       "File %s is in the private folder (%s/...)"
		       (File.to_string file) private_folder);
  File.parsef "%s/%s" private_folder (File.to_string file)

let () =
  assert begin
      "abc/file.ml"
      = (File.parsef "%s/abc/file.ml" private_folder
	 |> to_public |> File.to_string)
    end

