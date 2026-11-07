let () = print_string (Fir_llvm.render ~debug_info:true (Debug_fixture.module_ ~markers:true))
