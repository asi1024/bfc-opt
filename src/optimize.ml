open Syntax

let opt cs =
  let plus_eval (e1, c1) (e2, c2) =
    let units = List.map fst (e1 @ e2) |> List.sort_uniq compare in
    let coef unit =
      let c = (try List.assoc unit e1 with Not_found -> 0) +
                (try List.assoc unit e2 with Not_found -> 0) in
      if c = 0 then [] else [ (unit, c) ] in
    (List.map coef units |> List.concat, c1 + c2) in
  let mult_eval e1 e2 =
    let mult_cm c (e1, c1) = (e1, c1 * c) in
    let mult_pc (e1, c1) c = (List.map (mult_cm c) e1, c1 * c) in
    let mult_mm (e1, c1) (e2, c2) =
      ([List.sort_uniq compare (e1 @ e2), c1 * c2], 0) in
    let mult_pm (e1, c1) e =
      List.fold_left plus_eval ([mult_cm c1 e], 0) (List.map (mult_mm e) e1) in
    let mult_pp e (e2, c2) =
      List.fold_left plus_eval (mult_pc e c2) (List.map (mult_pm e) e2) in
    mult_pp e1 e2 in
  let merge_instr instr1 instr2 =
    let latter e =
      let mem m =
        ( try List.assoc (m + instr1.ptr) instr1.mem
          with Not_found ->  ([([m + instr1.ptr], 1)], 0) ) in
      let mult (e, c) =
        List.fold_left mult_eval ([], c) (List.map mem e) in
      let plus (e, c) =
        List.fold_left plus_eval ([], c) (List.map mult e) in
      plus e in
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
