; Symbol attributes: aliases, weak definitions and references, visibility.
; Checked by symbols.c, which overrides some of the weak definitions, and
; against the same declarations in C (__attribute__((alias/weak/visibility))).
triple "x86_64-unknown-linux-gnu"

global @table size 24 align 8 global = {bytes "\0b\00\00\00\00\00\00\00\16\00\00\00\00\00\00\00\21\00\00\00\00\00\00\00"}
; C defines a strong `wvar`, which wins
global @wvar size 8 align 8 weak = {bytes "\07\00\00\00\00\00\00\00"}
; nobody else defines `wkeep`
global @wkeep size 8 align 8 weak = {bytes "\05\00\00\00\00\00\00\00"}
global @hidden_data size 8 align 8 global hidden = {bytes "\2a\00\00\00\00\00\00\00"}

; a static function exported only through an alias (C's
; `static long twice(long); long twice_alias(long) __attribute__((alias("twice")))`)
alias @twice_alias global = @twice
; the middle and last element of @table under their own names
alias @table_mid global = @table + 8
alias @table_end_local local = @table + 16
; a weak alias C replaces, and one it does not
alias @walias weak = @dflt
alias @wkeep_alias weak = @dflt
alias @hidden_alias global hidden = @twice

; weak, never defined: its address is null
declare @maybe () -> (i64) weak
global @maybe_data size 8 align 8 weak = extern

func @twice (i64) -> (i64) local {
^e(%x: i64):
  %y = add i64 %x, %x
  ret %y
}

; C defines a strong `hook`; the call below must reach it
func @hook () -> (i64) weak {
^e:
  ret i64 1
}

func @dflt () -> (i64) weak {
^e:
  ret i64 5
}

func @fir_call_hook () -> (i64) global {
^e:
  %r = call () -> (i64) @hook()
  ret %r
}

func @fir_call_aliases () -> (i64) global {
^e:
  %a = call (i64) -> (i64) @twice_alias(i64 21)
  %b = call () -> (i64) @walias()
  %c = call () -> (i64) @wkeep_alias()
  %d = call (i64) -> (i64) @hidden_alias(i64 1)
  %ab = mul i64 %a, i64 1000000
  %bb = mul i64 %b, i64 10000
  %cb = mul i64 %c, i64 100
  %s1 = add i64 %ab, %bb
  %s2 = add i64 %s1, %cb
  %s3 = add i64 %s2, %d
  ret %s3
}

func @fir_read_data () -> (i64) global {
^e:
  %w = load i64 ptr @wvar, align 8
  %k = load i64 ptr @wkeep, align 8
  %m = load i64 ptr @table_mid, align 8
  %l = load i64 ptr @table_end_local, align 8
  %h = load i64 ptr @hidden_data, align 8
  %wb = mul i64 %w, i64 100000000
  %kb = mul i64 %k, i64 1000000
  %mb = mul i64 %m, i64 1000
  %s1 = add i64 %wb, %kb
  %s2 = add i64 %s1, %mb
  %s3 = add i64 %s2, %l
  %s4 = add i64 %s3, %h
  ret %s4
}

func @fir_maybe () -> (i64) global {
^e:
  %p = ptrtoint i64 ptr @maybe
  %q = ptrtoint i64 ptr @maybe_data
  %o = or i64 %p, %q
  ret %o
}

func @fir_addr (i64) -> (ptr) global {
^e(%k: i64):
  %z = icmp eq i1 %k, i64 0
  %a = select ptr %z, ptr @table_mid, ptr @twice_alias
  ret %a
}
