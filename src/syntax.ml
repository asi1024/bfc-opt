module A = Array
module L = List
module S = String

type mem = int
type mult = mem list * int
type plus = mult list * int
type exp = plus

type instr = { ptr : int; mem : (int * exp) list }

type com =
  | Input
  | Output
  | Instr of instr
  | If of com list
  | Loop of com list

let sp = Printf.sprintf

let string_of_comlist cs =
  let max_strnum = ref 0 in
  let to_char x = Char.escaped (char_of_int (x mod 26 + int_of_char 'a')) in
  let rec to_str x =
    max_strnum := max !max_strnum x;
    if x < 0 then "" else to_str (x / 26 - 1) ^ to_char x in
  let decls () =
    A.init (!max_strnum + 1) (fun x -> x) |> A.to_list
    |> L.map (fun x -> "char " ^ to_str x ^ " = 0; ") |> S.concat " " in
  let exp e =
    let int c = string_of_int c in
    let mem e = sp "*(ptr + %d)" e in
    let mult (e, c) = "(" ^ S.concat " * " (int c :: L.map mem e) ^ ")" in
    let plus (e, c) = "(" ^ S.concat " + " (int c :: L.map mult e) ^ ")" in
    plus e in
  let instr instr =
    let memf f = L.mapi f instr.mem |> S.concat " " in
    memf (fun i (_, e) -> sp "%s = %s;" (to_str i) (exp e))
    ^ memf (fun i (p, _) -> sp "*(ptr + %d) = %s;" p (to_str i))
    ^ sp " ptr += %d; " instr.ptr in
  let rec com = function
    | Input -> "*ptr = getchar();"
    | Output -> "putchar(*ptr);"
    | Instr i -> instr i
    | If c -> sp "if (*ptr) { %s }" (comlist c)
    | Loop c -> sp "while (*ptr) { %s }" (comlist c)
  and comlist cs =
    L.map com cs |> S.concat " " in
  let cs = comlist cs in
  "#include <stdio.h>\n"
  ^ "int mem[1 << 21]; int main() { int *ptr = mem + (1 << 20); "
  ^ decls () ^ cs ^ " return 0; }\n"
