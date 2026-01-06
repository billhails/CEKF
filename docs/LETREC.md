# Behaviour of the `let` and `in` keywords

> clarification on typesetting, `let` means the keyword in F-Natural,
> "let" in double quotes refers to the standard semantics of "let" in scheme.
> Likewise for `letrec` / "letrec"

Documenting this while I remember it, the `let` keyword
actually introduces a "letrec" (let-recursive) construct
in which each value is evaluated in parallel. This differs
from the normal semantics of a "let" because the values are
evaluated in an environment where the variables are already
bound, or at least visible.

For that reason the values in a "letrec" should all be lambdas
or constants, since it is not safe yet to do actual variable
lookup.

However the section introduced by a `let` keyword also permits
assignment of function calls to variables etc. and this is
supported in a very hacky manner.

In the ANF conversion step, in the proceedure `normalizeLetRecBindings`
in `anf_normalize.c`
the final body of the letrec (the `in` section) has already
been converted to ANF and the individual declarations are being
converted. Most importantly this conversion is done in reverse
order, rightmost first. If the value is a lambda then all proceeds as
normal, but otherwise the growing "letrec" is closed off, wrapped in
a "let" with the new binding and that "let" in turn wrapped in a new
"letrec" with initially empty bindings.
Any "letrec" that has no bindings, when wrapped in a "let" is stripped
of its letrec wrapper and reduced to its body.

the upshot of this is a `let` with no lambdas is completely transformed
into a nest of "let"s (effectively a "let*") while a `let` with only
lambdas is transformed into a single "letrec".

The big problem is that interjecting a non-lambda declaration between
lambdas in a `let` breaks mutual recursion and typechecking because
the functions in the resulting outer "letrec" don't know anything
about the functions in the inner "letrec".

## Notes

### AST

```text
preamble: {
rand = fn { (a$0) { builtin$rand(a$0); } };
assertion = fn { () { builtin$assertion(); } };
ord = fn { (a$0) { builtin$ord(a$0); } };
unicode_category = fn { (a$0) { builtin$unicode_category(a$0); } };
chr = fn { (a$0) { builtin$chr(a$0); } };
putc = fn { (a$0) { builtin$putc(a$0); } };
fputc = fn { (a$0, a$1) { builtin$fputc(a$0, a$1); } };
getc = fn { () { builtin$getc(); } };
fgetc = fn { (a$0) { builtin$fgetc(a$0); } };
putn = fn { (a$0) { builtin$putn(a$0); } };
fputn = fn { (a$0, a$1) { builtin$fputn(a$0, a$1); } };
putv = fn { (a$0) { builtin$putv(a$0); } };
fputv = fn { (a$0, a$1) { builtin$fputv(a$0, a$1); } };
puts = fn { (a$0) { builtin$puts(a$0); } };
fputs = fn { (a$0, a$1) { builtin$fputs(a$0, a$1); } };
gets = fn { () { builtin$gets(); } };
fgets = fn { (a$0) { builtin$fgets(a$0); } };
open = fn { (a$0, a$1) { builtin$open(a$0, a$1); } };
openmem = fn { () { builtin$openmem(); } };
close = fn { (a$0) { builtin$close(a$0); } };
opendir = fn { (a$0) { builtin$opendir(a$0); } };
readdir = fn { (a$0) { builtin$readdir(a$0); } };
closedir = fn { (a$0) { builtin$closedir(a$0); } };
ftype = fn { (a$0) { builtin$ftype(a$0); } };
sqlite3_open = fn { (a$0) { builtin$sqlite3_open(a$0); } };
sqlite3_close = fn { (a$0) { builtin$sqlite3_close(a$0); } };
sqlite3_prepare = fn { (a$0, a$1) { builtin$sqlite3_prepare(a$0, a$1); } };
sqlite3_finalize = fn { (a$0) { builtin$sqlite3_finalize(a$0); } };
sqlite3_bind = fn { (a$0, a$1) { builtin$sqlite3_bind(a$0, a$1); } };
sqlite3_fetch = fn { (a$0) { builtin$sqlite3_fetch(a$0); } };
sqlite3_names = fn { (a$0) { builtin$sqlite3_names(a$0); } };
argv = fn { (a$0) { builtin$argv(a$0); } };
getenv = fn { (a$0) { builtin$getenv(a$0); } };
com_real = fn { (a$0) { builtin$com_real(a$0); } };
com_imag = fn { (a$0) { builtin$com_imag(a$0); } };
com_mag = fn { (a$0) { builtin$com_mag(a$0); } };
com_theta = fn { (a$0) { builtin$com_theta(a$0); } };
exit = fn { (a$0) { builtin$exit(a$0); } };
macro op$0(x, y) { amb(x, y); };
macro op$1(x, y) { EQUALTO(x, y); };
macro op$2(x, y) { NOTEQUALTO(x, y); };
macro op$3(x, y) { NOTEQUALTO(x, y); };
macro op$4(x, y) { GREATERTHAN(x, y); };
macro op$5(x, y) { LESSTHAN(x, y); };
macro op$6(x, y) { LESSTHANOREQUALTO(x, y); };
macro op$7(x, y) { GREATERTHANOREQUALTO(x, y); };
macro op$8(x, y) { COMPARISON(x, y); };
macro op$9(x, y) { COMPARISON(x, y); };
macro op$10(x, y) { ADDITION(x, y); };
macro op$11(x, y) { SUBTRACTION(x, y); };
macro op$12(x) { NEGATION(x); };
macro NUMERICIDENTITY(x) { op$10/*orig:ADDITION*/(x, 0); };
macro op$13(x) { NUMERICIDENTITY(x); };
macro op$14(x, y) { MULTIPLICATION(x, y); };
macro op$15(x, y) { MULTIPLICATION(x, y); };
macro op$16(x, y) { DIVISION(x, y); };
macro op$17(x, y) { DIVISION(x, y); };
macro op$18(x, y) { MODULUS(x, y); };
macro op$19(x, y) { EXPONENTIAL(x, y); };
macro op$20(x) { callcc(x); };
typedef cmp {lt | eq | gt};
typedef bool {false | true};
typedef list(t) {nil | cons(t, list(t))};
alias string = list(char);
typedef maybe(t) {nothing | some(t)};
typedef try(f, s) {failure(f) | success(s)};
typedef basic_type {basic_null | basic_number(number) | basic_string(string) | basic_char(char)};
typedef io_mode {io_read | io_write | io_append}; typedef ftype_type {ftype_socket | ftype_symlink | ftype_regular | ftype_block | ftype_dir | ftype_char | ftype_fifo}; typedef unicode_general_category_type {GC_Ll | GC_Lm | GC_Lo | GC_Lt | GC_Lu | GC_Mc | GC_Me | GC_Mn | GC_Nd | GC_Nl | GC_No | GC_Pc | GC_Pd | GC_Pe | GC_Pf | GC_Pi | GC_Po | GC_Ps | GC_Sc | GC_Sk | GC_Sm | GC_So | GC_Zl | GC_Zp | GC_Zs | GC_Cc | GC_Cf | GC_Co | GC_Cs | GC_Cn};
NOT = fn { (true) { false; } (false) { true; } };
macro op$21(x) { NOT(x); };
macro AND(a, b) { if (a) { b; } else { false; }; };
macro op$22(x, y) { AND(x, y); };
macro OR(a, b) { if (a) { true; } else { b; }; };
macro op$23(x, y) { OR(x, y); };
XOR = fn { (true, true) { false; } (true, false) { true; } (false, true) { true; } (false, false) { false; } };
macro op$24(x, y) { XOR(x, y); };
macro NAND(a, b) { op$21/*orig:NOT*/(op$22/*orig:AND*/(a, b)); };
macro op$25(x, y) { NAND(x, y); };
macro NOR(a, b) { op$21/*orig:NOT*/(op$23/*orig:OR*/(a, b)); };
macro op$26(x, y) { NOR(x, y); };
macro XNOR(a, b) { op$21/*orig:NOT*/(op$24/*orig:XOR*/(a, b)); };
macro op$27(x, y) { XNOR(x, y); };
macro op$28(x, y) { cons(x, y); };
__assert__ = fn { (line, file, condition) { if (condition) { true; } else { puts(cons('a', cons('s', cons('s', cons('e', cons('r', cons('t', cons('i', cons('o', cons('n', cons(' ', cons('f', cons('a', cons('i', cons('l', cons('e', cons('d', cons(' ', cons('i', cons('n', cons(' ', nil()))))))))))))))))))))); puts(file); puts(cons(' ', cons('l', cons('i', cons('n', cons('e', cons(' ', nil()))))))); putn(line); putc(' '); assertion(); }; } };
factorial = fn { (n) { let h = fn { (0, a) { a; } (n, a) { h(op$11/*orig:SUBTRACTION*/(n, 1), op$14/*orig:MULTIPLICATION*/(n, a)); } }; in h(n, 1); } };
macro op$29(x) { factorial(x); };
__error__ = fn { (line, file, message) { puts(cons('e', cons('r', cons('r', cons('o', cons('r', cons(':', cons(' ', nil())))))))); puts(message); puts(cons(' ', cons('a', cons('t', cons(' ', nil()))))); puts(file); puts(cons(' ', cons('l', cons('i', cons('n', cons('e', cons(' ', nil()))))))); putn(line); putc(' '); exit(1); } };
macro op$30(x, y) { fn { (f, g, x) { f(g(x)); } }(x, y); };
macro op$31(x) { THUNK(x); };
macro THUNK(x) { fn { () { x; } }; };
macro op$32(x) { FORCE(x); };
FORCE = fn { (thunk) { thunk(); } };
append = fn { (nil(), b) { b; } (cons(h, t), b) { op$28/*orig:cons*/(h, append(t, b)); } };
macro op$33(x, y) { append(x, y); };
car = unsafe fn { (cons(h, _)) { h; } };
macro op$34(x) { car(x); };
cdr = unsafe fn { (cons(_, t)) { t; } };
macro op$35(x) { cdr(x); };
identity = fn { (x) { x; } };
print$list = fn { (helper, l) { let h1 = fn { (nil()) { true; } (cons(h, t)) { helper(h); h2(t); } }; h2 = fn { (nil()) { true; } (cons(h, t)) { puts(cons(',', cons(' ', nil()))); helper(h); h2(t); } }; in puts(cons('[', nil())); h1(l); puts(cons(']', nil())); l; } };
print$function = fn { (f) { puts(cons('<', cons('f', cons('u', cons('n', cons('c', cons('t', cons('i', cons('o', cons('n', cons('>', nil()))))))))))); f; } };
print$opaque = fn { (f) { puts(cons('<', cons('o', cons('p', cons('a', cons('q', cons('u', cons('e', cons('>', nil()))))))))); f; } };
print$int = fn { (n) { putn(n); n; } };
print$character = fn { (c) { putc('''); putc(c); putc('''); c; } };
__print__ = fn { (v) { putv(v); v; } };
print$string = fn { (s) { putc('"'); puts(s); putc('"'); s; } };
print$tuple_0 = fn { (t) { puts(cons('#', cons('(', cons(')', nil())))); t; } };
print$tuple_1 = fn { (pa, t = <tuple>(a)) { puts(cons('#', cons('(', nil()))); pa(a); puts(cons(')', nil())); t; } };
print$tuple_2 = fn { (pa, pb, t = <tuple>(a, b)) { puts(cons('#', cons('(', nil()))); pa(a); puts(cons(',', cons(' ', nil()))); pb(b); puts(cons(')', nil())); t; } };
print$tuple_3 = fn { (pa, pb, pc, t = <tuple>(a, b, c)) { puts(cons('#', cons('(', nil()))); pa(a); puts(cons(',', cons(' ', nil()))); pb(b); puts(cons(',', cons(' ', nil()))); pc(c); puts(cons(')', nil())); t; } };
print$tuple_4 = fn { (pa, pb, pc, pd, t = <tuple>(a, b, c, d)) { puts(cons('#', cons('(', nil()))); pa(a); puts(cons(',', cons(' ', nil()))); pb(b); puts(cons(',', cons(' ', nil()))); pc(c); puts(cons(',', cons(' ', nil()))); pd(d); puts(cons(')', nil())); t; } };
}
namespaces: []
body: {{ 1; }; }
```

### Lambda

```scheme
(typedefs
  ((cmp gt eq lt)
   (bool true false)
   ((list t) (cons t (list t)) nil)
   ((maybe t) (some t) nothing)
   ((try f s) (success s) (failure f))
   (basic_type
     (basic_char char)
     (basic_string (list char))
     (basic_number int)
     basic_null)
   (io_mode io_append io_write io_read)
   (ftype_type
     ftype_fifo ftype_char ftype_dir ftype_block ftype_regular
     ftype_symlink ftype_socket)
   (unicode_general_category_type
     GC_Cn GC_Cs GC_Co GC_Cf GC_Cc GC_Zs GC_Zp GC_Zl GC_So GC_Sm
     GC_Sk GC_Sc GC_Ps GC_Po GC_Pi GC_Pf GC_Pe GC_Pd GC_Pc GC_No
     GC_Nl GC_Nd GC_Mn GC_Me GC_Mc GC_Lu GC_Lt GC_Lo GC_Lm GC_Ll))
  (letrec ((print$cmp
             (lambda (thing)
               (begin
                 (match thing
                        ((#{2:gt:-1}#)
                         (puts "gt"))
                        ((#{1:eq:-1}#)
                         (puts "eq"))
                        ((#{0:lt:-1}#)
                         (puts "lt")))
                 thing)))
           (print$bool
             (lambda (thing)
               (begin
                 (match thing
                        ((#{1:true:-1}#)
                         (puts "true"))
                        ((#{0:false:-1}#)
                         (puts "false")))
                 thing)))
           (print$maybe
             (lambda (printt thing)
               (begin
                 (match (tag thing)
                        ((#{1:some:-1}#)
                         (begin
                           (puts "some")
                           (puts "(")
                           (printt (deconstruct some (1) thing))
                           (puts ")")))
                        ((#{0:nothing:-1}#)
                         (puts "nothing")))
                 thing)))
           (print$try
             (lambda (printf prints thing)
               (begin
                 (match (tag thing)
                        ((#{1:success:-1}#)
                         (begin
                           (puts "success")
                           (puts "("))
                           (prints (deconstruct success (1) thing))
                           (puts ")")))
                        ((#{0:failure:-1}#)
                         (begin
                           (puts "failure")
                           (puts "(")
                           (printf (deconstruct failure (1) thing))
                           (puts ")")))
                 thing)))
           (print$basic_type
             (lambda (thing)
               (begin
                 (match (tag thing)
                        ((#{3:basic_char:-1}#)
                         (begin
                           (puts "basic_char")
                           (puts "(")
                           (print$character
                             (deconstruct basic_char (1) thing))
                           (puts ")")))
                        ((#{2:basic_string:-1}#)
                         (begin
                           (puts "basic_string")
                           (puts "(")
                           (print$string
                             (deconstruct basic_string (1) thing))
                           (puts ")")))
                        ((#{1:basic_number:-1}#)
                         (begin
                           (puts "basic_number")
                           (puts "(")
                           (print$int (deconstruct basic_number (1) thing))
                           (puts ")")))
                        ((#{0:basic_null:-1}#)
                         (puts "basic_null")))
                 thing)))
           (print$io_mode
             (lambda (thing)
               (begin
                 (match thing
                        ((#{2:io_append:-1}#)
                         (puts "io_append"))
                        ((#{1:io_write:-1}#)
                         (puts "io_write"))
                        ((#{0:io_read:-1}#)
                         (puts "io_read")))
                 thing)))
           (print$ftype_type
             (lambda (thing)
               (begin
                 (match thing
                        ((#{6:ftype_fifo:-1}#)
                         (puts "ftype_fifo"))
                        ((#{5:ftype_char:-1}#)
                         (puts "ftype_char"))
                        ((#{4:ftype_dir:-1}#)
                         (puts "ftype_dir"))
                        ((#{3:ftype_block:-1}#)
                         (puts "ftype_block"))
                        ((#{2:ftype_regular:-1}#)
                         (puts "ftype_regular"))
                        ((#{1:ftype_symlink:-1}#)
                         (puts "ftype_symlink"))
                        ((#{0:ftype_socket:-1}#)
                         (puts "ftype_socket")))
                 thing)))
           (print$unicode_general_category_type
             (lambda (thing)
               (begin
                 (match thing
                        ((#{29:GC_Cn:-1}#)
                         (puts "GC_Cn"))
                        ((#{28:GC_Cs:-1}#)
                         (puts "GC_Cs"))
                        ((#{27:GC_Co:-1}#)
                         (puts "GC_Co"))
                        ((#{26:GC_Cf:-1}#)
                         (puts "GC_Cf"))
                        ((#{25:GC_Cc:-1}#)
                         (puts "GC_Cc"))
                        ((#{24:GC_Zs:-1}#)
                         (puts "GC_Zs"))
                        ((#{23:GC_Zp:-1}#)
                         (puts "GC_Zp"))
                        ((#{22:GC_Zl:-1}#)
                         (puts "GC_Zl"))
                        ((#{21:GC_So:-1}#)
                         (puts "GC_So"))
                        ((#{20:GC_Sm:-1}#)
                         (puts "GC_Sm"))
                        ((#{19:GC_Sk:-1}#)
                         (puts "GC_Sk"))
                        ((#{18:GC_Sc:-1}#)
                         (puts "GC_Sc"))
                        ((#{17:GC_Ps:-1}#)
                         (puts "GC_Ps"))
                        ((#{16:GC_Po:-1}#)
                         (puts "GC_Po"))
                        ((#{15:GC_Pi:-1}#)
                         (puts "GC_Pi"))
                        ((#{14:GC_Pf:-1}#)
                         (puts "GC_Pf"))
                        ((#{13:GC_Pe:-1}#)
                         (puts "GC_Pe"))
                        ((#{12:GC_Pd:-1}#)
                         (puts "GC_Pd"))
                        ((#{11:GC_Pc:-1}#)
                         (puts "GC_Pc"))
                        ((#{10:GC_No:-1}#)
                         (puts "GC_No"))
                        ((#{9:GC_Nl:-1}#)
                         (puts "GC_Nl"))
                        ((#{8:GC_Nd:-1}#)
                         (puts "GC_Na"))
                        ((#{7:GC_Mn:-1}#)
                         (puts "GC_Mn"))
                        ((#{6:GC_Me:-1}#)
                         (puts "GC_Me"))
                        ((#{5:GC_Mc:-1}#)
                         (puts "GC_Mc"))
                        ((#{4:GC_Lu:-1}#)
                         (puts "GC_Lu"))
                        ((#{3:GC_Lt:-1}#)
                         (puts "GC_Lt"))
                        ((#{2:GC_Lo:-1}#)
                         (puts "GC_Lo"))
                        ((#{1:GC_Lm:-1}#)
                         (puts "GC_Lm"))
                        ((#{0:GC_Ll:-1}#)
                         (puts "GC_Ll")))
                 thing)))
           (rand (lambda (p$86) (builtin$rand p$86)))
           (assertion builtin$assertion)
           (ord (lambda (p$85) (builtin$ord p$85)))
           (unicode_category
             (lambda (p$84) (builtin$unicode_category p$84)))
           (chr (lambda (p$83) (builtin$chr p$83)))
           (putc (lambda (p$82) (builtin$putc p$82)))
           (fputc (lambda (p$80 p$81) (builtin$fputc p$80 p$81)))
           (getc builtin$getc)
           (fgetc (lambda (p$79) (builtin$fgetc p$79)))
           (putn (lambda (p$78) (builtin$putn p$78)))
           (fputn (lambda (p$76 p$77) (builtin$fputn p$76 p$77)))
           (putv (lambda (p$75) (builtin$putv p$75)))
           (fputv (lambda (p$73 p$74) (builtin$fputv p$73 p$74)))
           (puts (lambda (p$72) (builtin$puts p$72)))
           (fputs (lambda (p$70 p$71) (builtin$fputs p$70 p$71)))
           (gets builtin$gets)
           (fgets (lambda (p$69) (builtin$fgets p$69)))
           (open (lambda (p$67 p$68) (builtin$open p$67 p$68)))
           (openmem builtin$openmem)
           (close (lambda (p$66) (builtin$close p$66)))
           (opendir (lambda (p$65) (builtin$opendir p$65)))
           (readdir (lambda (p$64) (builtin$readdir p$64)))
           (closedir
             (lambda (p$63) (builtin$closedir p$63)))
           (ftype (lambda (p$62) (builtin$ftype p$62)))
           (sqlite3_open
             (lambda (p$61) (builtin$sqlite3_open p$61)))
           (sqlite3_close
             (lambda (p$60) (builtin$sqlite3_close p$60)))
           (sqlite3_prepare
             (lambda (p$58 p$59)
               (builtin$sqlite3_prepare p$58 p$59)))
           (sqlite3_finalize
             (lambda (p$57) (builtin$sqlite3_finalize p$57)))
           (sqlite3_bind
             (lambda (p$55 p$56)
               (builtin$sqlite3_bind p$55 p$56)))
           (sqlite3_fetch
             (lambda (p$54) (builtin$sqlite3_fetch p$54)))
           (sqlite3_names
             (lambda (p$53) (builtin$sqlite3_names p$53)))
           (argv (lambda (p$52) (builtin$argv p$52)))
           (getenv (lambda (p$51) (builtin$getenv p$51)))
           (com_real
             (lambda (p$50) (builtin$com_real p$50)))
           (com_imag
             (lambda (p$49) (builtin$com_imag p$49)))
           (com_mag (lambda (p$48) (builtin$com_mag p$48)))
           (com_theta
             (lambda (p$47) (builtin$com_theta p$47)))
           (exit (lambda (p$46) (builtin$exit p$46)))
           (op$0 (lambda (x y) (amb (x) (y))))
           (op$1 (lambda (x y) (eq (x) (y))))
           (op$2 (lambda (x y) (ne (x) (y))))
           (op$3 (lambda (x y) (ne (x) (y))))
           (op$4 (lambda (x y) (gt (x) (y))))
           (op$5 (lambda (x y) (lt (x) (y))))
           (op$6 (lambda (x y) (le (x) (y))))
           (op$7 (lambda (x y) (ge (x) (y))))
           (op$8 (lambda (x y) (cmp (x) (y))))
           (op$9 (lambda (x y) (cmp (x) (y))))
           (op$10 (lambda (x y) (add (x) (y))))
           (op$11 (lambda (x y) (sub (x) (y))))
           (op$12 (lambda (x) (sub 0 (x))))
           (NUMERICIDENTITY
             (lambda (x) (op$10 x (lambda () 0))))
           (op$13 (lambda (x) (NUMERICIDENTITY x)))
           (op$14 (lambda (x y) (mul (x) (y))))
           (op$15 (lambda (x y) (mul (x) (y))))
           (op$16 (lambda (x y) (div (x) (y))))
           (op$17 (lambda (x y) (div (x) (y))))
           (op$18 (lambda (x y) (mod (x) (y))))
           (op$19 (lambda (x y) (pow (x) (y))))
           (op$20 (lambda (x) (call/cc (x))))
           (NOT (lambda (p$45)
                  (match p$45
                         ((#{1:true:-1}#) constructor:false)
                         ((#{0:false:-1}#) constructor:true))))
           (op$21 (lambda (x) (NOT (x))))
           (AND (lambda (a b) (if (a) (b) constructor:false)))
           (op$22 (lambda (x y) (AND x y)))
           (OR (lambda (a b) (if (a) constructor:true (b))))
           (op$23 (lambda (x y) (OR x y)))
           (XOR (lambda (p$43 p$44)
                  (match p$43
                         ((#{1:true:-1}#)
                          (match p$44
                                 ((#{1:true:-1}#) constructor:false)
                                 ((#{0:false:-1}#) constructor:true)))
                         ((#{0:false:-1}#)
                          (match p$44
                                 ((#{1:true:-1}#) constructor:true)
                                 ((#{0:false:-1}#) constructor:false))))))
           (op$24 (lambda (x y) (XOR (x) (y))))
           (NAND (lambda (a b) (op$21 (lambda () (op$22 a b)))))
           (op$25 (lambda (x y) (NAND x y)))
           (NOR (lambda (a b) (op$21 (lambda () (op$23 a b)))))
           (op$26 (lambda (x y) (NOR x y)))
           (XNOR (lambda (a b) (op$21 (lambda () (op$24 a b)))))
           (op$27 (lambda (x y) (XNOR x y)))
           (op$28 (lambda (x y) (constructor:cons (x) (y))))
           (__assert__
             (lambda (p$40 p$41 p$42)
               (if p$42
                 constructor:true
                 (begin
                   (puts "assertion failed in ")
                   (puts p$41)
                   (puts " line ")
                   (putn p$40)
                   (putc "\n")
                   (assertion)))))
           (factorial
             (lambda (p$39)
               (letrec ((h (lambda (p$37 p$38)
                             (cond p$37
                                   (0 p$38)
                                   (<null>
                                    (h (op$11 (lambda () p$37) (lambda () 1))
                                       (op$14 (lambda () p$37)
                                              (lambda () p$38))))))))
                 (h p$39 1))))
           (op$29 (lambda (x) (factorial (x))))
           (__error__
             (lambda (p$34 p$35 p$36)
               (begin
                 (puts "error: ")
                 (puts p$36)
                 (puts " at ")
                 (puts p$35)
                 (puts " line ")
                 (putn p$34)
                 (putc " ")
                 (exit 1))))
           (op$30 (lambda (x y)
                    ((lambda (p$31 p$32 p$33) (p$31 (p$32 p$33)))
                     (x)
                     (y))))
           (op$31 (lambda (x) (THUNK x)))
           (THUNK (lambda (x) x))
           (op$32 (lambda (x) (FORCE (x))))
           (FORCE (lambda (p$30) (p$30)))
           (append
             (lambda (p$28 p$29)
               (match (tag p$28)
                      ((#{0:nil:-1}#) p$29)
                      ((#{1:cons:-1}#)
                       (let (p$28$1 (deconstruct cons (2) p$28))
                         (let (p$28$0 (deconstruct cons (1) p$28))
                           (constructor:cons p$28$0 (append p$28$1 p$29))))))))
           (op$33 (lambda (x y) (append (x) (y))))
           (car (lambda (p$27)
                  (match (tag p$27)
                         ((#{1:cons:-1}#)
                          (let (p$27$0 (deconstruct cons (1) p$27)) p$27$0))
                         ((#{0:cons:-1}#) (error)))))
           (op$34 (lambda (x) (car (x))))
           (cdr (lambda (p$26)
                  (match (tag p$26)
                         ((#{1:cons:-1}#)
                          (let (p$26$1 (deconstruct cons (2) p$26)) p$26$1))
                         ((#{0:cons:-1}#) (error)))))
           (op$35 (lambda (x) (cdr (x))))
           (identity (lambda (p$25) p$25))
           (print$list
             (lambda (p$23 p$24)
               (letrec ((h1 (lambda (p$22)
                              (match (tag p$22)
                                     ((#{0:nil:-1}#) constructor:true)
                                     ((#{1:cons:-1}#)
                                      (let (p$22$1 (deconstruct cons (2) p$22))
                                        (let (p$22$0
                                              (deconstruct cons (1) p$22))
                                          (begin
                                            (p$23 p$22$0)
                                            (h2 p$22$1))))))))
                        (h2 (lambda (p$21)
                              (match (tag p$21)
                                     ((#{0:nil:-1}#) constructor:true)
                                     ((#{1:cons:-1}#)
                                      (let (p$21$1 (deconstruct cons (2) p$21))
                                        (let (p$21$0
                                              (deconstruct cons (1) p$21))
                                          (begin
                                            (puts ", ")
                                            (p$23 p$21$0)
                                            (h2 p$21$1)))))))))
                 (begin
                   (puts "[")
                   (h1 p$24)
                   (puts "]")
                   p$24))))
           (print$function
             (lambda (p$20)
               (begin
                 (puts "<function>")
                 p$20)))
           (print$opaque
             (lambda (p$19)
               (begin
                 (puts "<opaque>")
                 p$19)))
           (print$int
             (lambda (p$18) (begin (putn p$18) p$18)))
           (print$character
             (lambda (p$17)
               (begin (putc "'") (putc p$17) (putc "'") p$17)))
           (__print__
             (lambda (p$16) (begin (putv p$16) p$16)))
           (print$string
             (lambda (p$15)
               (begin (putc "\"") (puts p$15) (putc "\"") p$15)))
           (print$tuple_0
             (lambda (p$14)
               (begin
                 (puts "#()")
                 p$14)))
           (print$tuple_1
             (lambda (p$12 p$13)
               (let (p$13$0 (index 0 p$13))
                 (begin
                   (puts "#(")
                   (p$12 p$13$0)
                   (puts ")")
                   p$13))))
           (print$tuple_2
             (lambda (p$9 p$10 p$11)
               (let (p$11$1 (index 1 p$11))
                 (let (p$11$0 (index 0 p$11))
                   (begin
                     (puts "#(")
                     (p$9 p$11$0)
                     (puts ", ")
                     (p$10 p$11$1)
                     (puts ")")
                     p$11)))))
           (print$tuple_3
             (lambda (p$5 p$6 p$7 p$8)
               (let (p$8$2 (index 2 p$8))
                 (let (p$8$1 (index 1 p$8))
                   (let (p$8$0 (index 0 p$8))
                     (begin
                       (puts "#(")
                       (p$5 p$8$0)
                       (puts ", ")
                       (p$6 p$8$1)
                       (puts ", ")
                       (p$7 p$8$2)
                       (puts ")")
                       p$8))))))
           (print$tuple_4
             (lambda (p$0 p$1 p$2 p$3 p$4)
               (let (p$4$3 (index 3 p$4))
                 (let (p$4$2 (index 2 p$4))
                   (let (p$4$1 (index 1 p$4))
                     (let (p$4$0 (index 0 p$4))
                       (begin
                         (puts "#(")
                         (p$0 p$4$0)
                         (puts ", ")
                         (p$1 p$4$1)
                         (puts ", ")
                         (p$2 p$4$2)
                         (puts ", ")
                         (p$3 p$4$3)
                         (puts ")")
                         p$4))))))))
    1))
```

### ANF showing nested lets

```scheme
(letrec ((barrels_of_fun
           (lambda ()
             (let (barrels [30 32 36 38 40 62])
               (let (beer
                     (let (anf$15 (lookup 0 one_of)) (anf$15 barrels)))
                 (let (wine
                       (let (anf$14 (lookup 1 exclude))
                         (anf$14 (make-vec 1 beer (make-vec 0)) barrels)))
                   (let (barrel_1
                         (let (anf$13 (lookup 0 one_of)) (anf$13 wine)))
                     (let (barrel_2
                           (let (anf$10 (lookup 0 one_of))
                             (let (anf$11
                                   (let (anf$12 (lookup 1 exclude))
                                     (anf$12
                                       (make-vec 1 barrel_1 (make-vec 0))
                                       wine)))
                               (anf$10 anf$11))))
                       (let (purchase
                             (let (anf$8
                                   (let (anf$9 (lookup 1 exclude))
                                     (anf$9 (make-vec
                                              1
                                              barrel_1
                                              (make-vec
                                                1
                                                barrel_2
                                                (make-vec 0)))
                                            wine)))
                               (let (anf$7 (lookup 0 some_of)) (anf$7 anf$8))))
                         (let (anf$3 (lookup 0 require))
                           (let (anf$4
                                 (op$1 (lambda ()
                                         (op$14 (lambda ()
                                                  (op$10 (lambda () barrel_1)
                                                         (lambda () barrel_2)))
                                                (lambda () 2)))
                                       (lambda ()
                                         (let (anf$6 (lookup 1 sum))
                                           (anf$6 purchase)))))
                             (let (anf$5 (anf$3 anf$4)) beer))))))))))))
  (let (anf$2 (barrels_of_fun))
    ((lambda (x$0)
       (let (anf$1 (print$int x$0))
         (let (anf$0 (putc "\n")) x$0)))
     anf$2)))
```
