(* The native backend's object contents, before Native_object writes them as
   an ELF64 relocatable file for the Fas linker.

   Symbols are offsets in one of the fixed sections (.text, .rodata, .data,
   .bss) or in an extra section (a named section or .tdata/.tbss, see
   Native_object.extra). Relocations are the encoder's (X64.reloc).

   (This module used to hold a static executable writer for the first
   backend; the Fas linker replaced it.) *)

type section = Text | Rodata | Data | Bss | Extra of int  (** index in [extra] *)

type symbol = {
  name : string;
  section : section;
  offset : int;
  size : int;
  func : bool;
}

type input = {
  text : bytes;
  text_relocs : X64.reloc list;
  rodata : bytes;
  rodata_relocs : X64.reloc list;  (** only [Abs64] *)
  data : bytes;
  data_relocs : X64.reloc list;  (** only [Abs64] *)
  bss_size : int;
  symbols : symbol list;
  entry : string;
}
