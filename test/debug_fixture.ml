(* A small Fir module with source locations, built through Lower_emit the
   way lower.ml builds one: [main] calls [helper], which branches; [plain]
   has no source. With [markers:false] no instruction has a location. *)

let module_ ~markers =
  let lay : Lower_emit.layouts = Hashtbl.create 1 in
  let mc = Lower_emit.create_module ~triple:(Target_layout.current ()).triple lay in
  let mark fn line column = if markers then Lower_emit.location fn line column in
  let finish fn = match Lower_emit.finish fn with Ok () -> () | Error b -> failwith (Printf.sprintf "b%d open" b) in
  let i32 n = Low.Const (Low.I32, n) in
  (* main: %0 = 1 + 2; %1 = helper(%0); ret %1 *)
  let main =
    Lower_emit.start mc ~name:"main" ~linkage:Fir.Global ~params:[] ~ret:Low.I32
      ~ret_extension:Low.No_extension ~variadic:false ~display:None
      ~source:(Some ("/work/app/main.fas", 3))
  in
  Lower_emit.bin main 0 Low.Add Low.I32 (i32 1L) (i32 2L);
  mark main 4 5;
  Lower_emit.call main (Some 1) Low.No_extension Low.I32 "helper"
    [ (Low.I32, Low.No_extension, Low.Local (0, Low.I32)) ];
  mark main 6 3;
  Lower_emit.ret main (Some (Low.I32, Low.Local (1, Low.I32)));
  finish main;
  (* helper(x): branches on x + 1 < 0, both ways reach the return *)
  let helper =
    Lower_emit.start mc ~name:"helper" ~linkage:Fir.Local
      ~params:[ ("x", Low.I32, Low.No_extension) ]
      ~ret:Low.I32 ~ret_extension:Low.No_extension ~variadic:false ~display:None
      ~source:(Some ("/work/lib/util.fas", 10))
  in
  let b1 = Lower_emit.new_block helper and b2 = Lower_emit.new_block helper in
  let x = Low.Param ("x", Low.I32) in
  mark helper 11 3;
  Lower_emit.bin helper 0 Low.Add Low.I32 x (i32 1L);
  mark helper 12 3;
  Lower_emit.cmp helper 1 Low.Slt Low.I32 (Low.Local (0, Low.I32)) (i32 0L);
  Lower_emit.cond_br helper (Low.Local (1, Low.I1)) b1 b2;
  Lower_emit.enter helper b1 None;
  mark helper 13 5;
  Lower_emit.br helper b2;
  Lower_emit.enter helper b2 None;
  mark helper 14 3;
  Lower_emit.ret helper (Some (Low.I32, x));
  finish helper;
  (* plain: no source, so its marker has no file *)
  let plain =
    Lower_emit.start mc ~name:"plain" ~linkage:Fir.Local ~params:[] ~ret:Low.Void
      ~ret_extension:Low.No_extension ~variadic:false ~display:None ~source:None
  in
  mark plain 1 1;
  Lower_emit.ret plain None;
  finish plain;
  mc.m
