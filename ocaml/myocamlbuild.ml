open Ocamlbuild_plugin
open Command

(* Configuration section *)
let have_udbm = "-DHAVE_UDBM"
let udbm_lib = "-ludbm"
let udbm_libdir = "-L/home/mihir/uppaal/lib"
let ocamlgraph_lib = "-lgraph"
let ocamlgraph_libdir = "-L+ocamlgraph"
let udbm_include = "-I/home/mihir/uppaal/include"
let ocamlgraph_include = "+ocamlgraph"
let ocamlgraph_archive = "ocamlgraph/graph.cmxa"
let cpp_lib = "-lstdc++"

let static = true

(* List of headers *)
let headers =
  []

;;

dispatch begin function
| After_rules ->

  (* When one is making a C library that uses the udbm library with
     ocamlmklib,then issue these flags. *)
  flag ["ocamlmklib"; "c"; "use_udbm"]
    (S[A udbm_libdir; A udbm_lib; A cpp_lib; A"-custom"]);

  (* When one compiles C code using the udbm library *)
  flag ["c"; "compile"; "include_udbm"]
    (S[A"-cc"; A"g++"; A"-ccopt"; A udbm_include; A"-ccopt"; A have_udbm]);

  flag ["link"; "ocaml"; "library"; "use_udbm"]
    (S[A"-ccopt"; A udbm_libdir; A"-cclib"; A udbm_lib]);

  flag ["compile"]
    (S[A"-I"; A ocamlgraph_include]);

  (* flag ["link"; "ocaml"] *)
  (*   (S[A"-ccopt"; A ocamlgraph_libdir; A"-cclib"; A ocamlgraph_lib]); *)

  flag ["link"; "ocaml"]
    (S[A ocamlgraph_archive]);

  (* flag ["link"; "ocaml"] *)
  (*   (S[A"-lib"; A"graph"]); *)

  (* If `static' is true then every ocaml link in bytecode will add
     -custom *)
  if static then flag ["link"; "ocaml"; "byte"] (A"-custom");

  (* zone_stubs is an ocaml library.
     This will declare use_zone_stubs and include_zone_stubs *)
  ocaml_lib "zone_stubs";

  flag ["link"; "library"; "ocaml"; "byte"; "use_zone"]
    (S[A"-dllib"; A"-lzone_stubs"; A"-cclib"; A"-lzone_stubs"]);

  flag ["link"; "ocaml"; "native"; "use_zone"]
    (S[A"-cclib"; A"-L."; A"-cclib"; A"-lzone_stubs"; A"-cclib"; A
      udbm_libdir; A"-cclib"; A udbm_lib; A"-cclib"; A cpp_lib]);

  (* When ocaml is linking something that use the libzone library,
     then one need that file to be up to date. *)
  dep  ["link"; "ocaml"; "use_zone"] ["libzone_stubs.a"];

  (* As an approximation all our C files use the headers.
     Note: This will import headers in the build directory. *)
  dep  ["compile"; "c"] headers;
| _ -> ()
end
