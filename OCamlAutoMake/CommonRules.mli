val noop_rule : File.t -> OCamlBuild.rule
val folder_rule : File.t -> OCamlBuild.rule
module Private :
sig
  val private_folder : string
  val is_private : File.t -> bool
  val is_public : File.t -> bool
  val to_public : File.t -> File.t
  val to_private : File.t -> File.t
end
val private_file_rule : File.t -> OCamlBuild.rule
val public_file_rule : File.t -> OCamlBuild.rule
