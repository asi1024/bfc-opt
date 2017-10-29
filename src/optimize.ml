open Syntax

let opt cs =
  let merge_instr instr1 instr2 =
    let rec latter = function
      | Mem m ->
         ( try List.assoc (m + instr1.ptr) instr1.mem
           with Not_found -> Mem (m + instr1.ptr) )
      | Plus (e, i) -> Plus (List.map latter e, i)
      | Mult (e, i) -> Mult (List.map latter e, i) in
    let add (index, exp) assoc2 =
      if List.mem_assoc index assoc2 then assoc2 else (index, exp) :: assoc2 in
    { ptr = instr1.ptr + instr2.ptr;
      mem = List.map (fun (i, e) -> (i + instr1.ptr, latter e)) instr2.mem
            |> List.fold_right add instr1.mem } in
  let rec comlist = function
    | (Instr instr1) :: (Instr instr2) :: tail ->
       comlist (Instr (merge_instr instr1 instr2) :: tail)
    | If cs :: tail -> If (comlist cs) :: comlist tail
    | Loop cs :: tail -> Loop (comlist cs) :: comlist tail
    | [] -> []
    | head :: tail -> head :: comlist tail in
  comlist cs
