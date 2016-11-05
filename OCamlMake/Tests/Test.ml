type t = OCamlMake_Tests_PackageA.A1.t
let () = Printf.printf "a1 = %s\n" (OCamlMake_Tests_PackageA.A1.value);
         Printf.printf "a2 = %s\n" (OCamlMake_Tests_PackageA.A2.value);
         Printf.printf "b1 = %s\n" (OCamlMake_Tests_PackageB_B1.value);
         Printf.printf "b2 = %s\n" (OCamlMake_Tests_PackageB_B2.value)
