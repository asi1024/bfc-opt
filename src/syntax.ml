type exp =
  | Const of int
  | Mem of int
  | Plus of exp * exp
  | Mult of exp * exp

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
    Array.init (!max_strnum + 1) (fun x -> x) |> Array.to_list
    |> List.map (fun x -> "char " ^ to_str x ^ " = 0; ") |> String.concat " " in
  let rec exp = function
    | Const i -> string_of_int i
    | Mem i -> sp "*(ptr + %d)" i
    | Plus (e1, e2) -> sp "(%s + %s)" (exp e1) (exp e2)
    | Mult (e1, e2) -> sp "(%s * %s)" (exp e1) (exp e2) in
  let instr instr =
    let memf f = List.map f instr.mem |> String.concat " " in
    memf (fun (i, e) -> sp "%s = %s;" (to_str i) (exp e))
    ^ sp " ptr += %d; " instr.ptr
    ^ memf (fun (i, e) -> sp "*(ptr + %d) = %s;" i (exp e)) in
  let rec com = function
    | Input -> "*ptr = getchar();"
    | Output -> "putchar(*ptr);"
    | Instr i -> instr i
    | If c -> sp "if (*ptr) { %s }" (comlist c)
    | Loop c -> sp "while (*ptr) { %s }" (comlist c)
  and comlist cs =
    List.map com cs |> String.concat " " in
  let cs = comlist cs in
  "#include <stdio.h>\n"
  ^ "int mem[1 << 21]; int main() { int *ptr = mem + (1 << 20); "
  ^ decls () ^ cs ^ " return 0; }\n"
