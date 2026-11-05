(* Invocation-owned policy, shared by the CLI, compiler driver and library. *)
type mode =
  | Static
  | Static_pie
  | Dynamic of { pie : bool; interpreter : string }
  | Shared

let requires_static_inputs = function Static | Static_pie -> true | _ -> false

type hash_style = Sysv | Gnu | Both
type build_id = No_id | Sha1 | Md5

type t = {
  gc_sections : bool;
  undefined : string list;
  mode : mode;
  runpath : string option;
  new_dtags : bool;
  allow_shlib_undefined : bool;
  rpath_link : string list;
  export_dynamic : bool;
  library_paths : string list;
  sysroot : string option;
  nostdlib : bool;
  relro : bool;
  soname : string option;
  no_undefined : bool;
  allow_multiple_definition : bool;
  version_script : Version_script.t option;
  hash_style : hash_style;
  build_id : build_id;
}

let default =
  {
    gc_sections = false;
    undefined = [];
    mode = Static;
    runpath = None;
    new_dtags = true;
    allow_shlib_undefined = true;
    rpath_link = [];
    export_dynamic = false;
    library_paths = [];
    sysroot = None;
    nostdlib = false;
    relro = true;
    soname = None;
    no_undefined = false;
    allow_multiple_definition = false;
    version_script = None;
    hash_style = Sysv;
    build_id = No_id;
  }
