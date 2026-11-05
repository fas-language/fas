(* LLVM applies C adapter names while rendering. Native emission consumes IR
   directly, so carry the same symbol mapping through declarations and uses. *)
let apply ~redirect (ir : Ir.module_) =
  if List.for_all (fun (f : Ir.func) -> redirect f.name = f.name) ir.funcs then Ok ir
  else
    let value = function
      | Ir.Global (name, ty) -> Ir.Global (redirect name, ty)
      | value -> value
    in
    let instruction instruction =
      match Ir_fold.map_instr value instruction with
      | Ir.Call (id, extension, ty, name, args) ->
          Ir.Call (id, extension, ty, redirect name, args)
      | Ir.Global_ptr (id, name, ty) -> Ir.Global_ptr (id, redirect name, ty)
      | instruction -> instruction
    in
    let globals =
      List.map
        (function
          | Ir.String_global global ->
              Ir.String_global { global with name = redirect global.name }
          | Ir.Array_global global ->
              Ir.Array_global { global with name = redirect global.name }
          | Ir.Storage_global global ->
              Ir.Storage_global
                {
                  global with
                  name = redirect global.name;
                  pointers =
                    List.map
                      (fun (offset, name, addend) -> (offset, redirect name, addend))
                      global.pointers;
                })
        ir.globals
    in
    let funcs =
      List.map
        (fun (f : Ir.func) ->
          {
            f with
            name = redirect f.name;
            blocks =
              List.map
                (fun (block : Ir.block) ->
                  {
                    block with
                    instrs = List.map instruction block.instrs;
                    terminator = Ir_fold.map_term value block.terminator;
                  })
                f.blocks;
          })
        ir.funcs
    in
    let ir =
      {
        ir with
        globals;
        funcs;
        no_inline_functions = List.map redirect ir.no_inline_functions;
        inline_functions = List.map redirect ir.inline_functions;
      }
    in
    Result.map (fun () -> ir) (Ir.validate ir)
