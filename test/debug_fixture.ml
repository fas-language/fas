let module_ ~markers =
  let mark line column = if markers then [ Ir.Location (line, column) ] else [] in
  let block id instrs terminator = { Ir.id; label = "b"; instrs; terminator } in
  let helper =
    {
      Ir.name = "helper";
      params = [ { Ir.name = "x"; ty = Ir.I32; extension = Ir.No_extension } ];
      ret = Ir.I32;
      ret_extension = Ir.No_extension;
      blocks =
        [
          block 0
            (List.concat
               [
                 mark 11 3;
                 [
                   Ir.Bin
                     (0, Ir.Add, Ir.I32, Ir.Param ("x", Ir.I32), Ir.Const (Ir.I32, 1L));
                 ];
                 mark 12 3;
                 [
                   Ir.Cmp
                     (1, Ir.Slt, Ir.I32, Ir.Local (0, Ir.I32), Ir.Const (Ir.I32, 0L));
                 ];
               ])
            (Ir.CondBr (Ir.Local (1, Ir.I1), 1, 2));
          block 1 (mark 13 5) (Ir.Br 2);
          block 2 (mark 14 3) (Ir.Ret (Some (Ir.I32, Ir.Param ("x", Ir.I32))));
        ];
      linkage = Ir.Internal;
      variadic = false;
      source = Some ("/work/lib/util.fas", 10);
      display = None;
    }
  in
  let main =
    {
      Ir.name = "main";
      params = [];
      ret = Ir.I32;
      ret_extension = Ir.No_extension;
      blocks =
        [
          block 0
            (List.concat
               [
                 [
                   Ir.Bin
                     (0, Ir.Add, Ir.I32, Ir.Const (Ir.I32, 1L), Ir.Const (Ir.I32, 2L));
                 ];
                 mark 4 5;
                 [
                   Ir.Call
                     ( Some 1,
                       Ir.No_extension,
                       Ir.I32,
                       "helper",
                       [ (Ir.I32, Ir.No_extension, Ir.Local (0, Ir.I32)) ] );
                 ];
                 mark 6 3;
               ])
            (Ir.Ret (Some (Ir.I32, Ir.Local (1, Ir.I32))));
        ];
      linkage = Ir.External;
      variadic = false;
      source = Some ("/work/app/main.fas", 3);
      display = None;
    }
  in
  let plain =
    {
      Ir.name = "plain";
      params = [];
      ret = Ir.Void;
      ret_extension = Ir.No_extension;
      blocks = [ block 0 (mark 1 1) (Ir.Ret None) ];
      linkage = Ir.Internal;
      variadic = false;
      source = None;
      display = None;
    }
  in
  {
    Ir.target_triple = (Target_layout.current ()).triple;
    data_layout = (Target_layout.current ()).llvm_data_layout;
    structs = [];
    globals = [];
    funcs = [ main; helper; plain ];
    no_inline_functions = [];
  }
