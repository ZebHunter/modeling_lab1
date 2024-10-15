let read_file filename =
  let ic = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line ic in
      read_lines ((float_of_string @@ String.trim line) :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  read_lines []

let list_to_nlist lst n = List.to_seq lst |> Seq.take n |> List.of_seq

let mean_val lst = 
  let len = float_of_int (List.length lst) in
  List.fold_left (fun acc x -> acc +. (x /. len)) 0. lst

let dispersion lst = 
  let len = float_of_int (List.length lst) in
  let mean = mean_val lst in
  List.fold_left (fun acc x -> acc +. ((x -. mean) *. (x -. mean) /. len)) 0. lst

let norm_dispersion lst = 
  (dispersion lst) *. (float_of_int @@ List.length @@ lst) /. (float_of_int @@ ((List.length lst) - 1))

let sko lst = sqrt @@ norm_dispersion @@ lst 

let cov lst = 
  let mean = mean_val lst in
  let sko = sko lst in
  sko /. mean

(*
  f - функция определяющая p -> tp (находится в main.ml)
*)

let apply_interval f p lst =
  let sko_val = (sko lst) /. (sqrt @@ float_of_int @@ List.length @@ lst) in
  let tp = f p in
  tp *. sko_val

let relative f lst n =
  let nlist = list_to_nlist lst n in
  let etalon = f lst in
  let value = f nlist in
  let rel = value /. etalon in
  if rel > 1. then (int_of_float (1. -. rel)) * 100 else (int_of_float rel) * 100


let shift_left = function
  | [] -> []
  | h :: t -> t @ [h]

let rec gen_shift_list lst = function
  | 0 -> lst
  | n -> gen_shift_list (shift_left lst) (n - 1)

let autocov lst = 
  let rec loop l n acc =
    match n with
    | 0 -> acc
    | _ -> let slst = gen_shift_list l n in
           let seqslst = List.to_seq slst in
           let mean = mean_val l in
           let meanY = mean_val slst in
           let seqlst = List.to_seq l in
           let xylist = Seq.zip seqlst seqslst |> List.of_seq in
           let part = List.fold_left (fun acc x -> acc +. (((fst x) -. mean) *. ((snd x) -. meanY))) 0. xylist in
           let secpart = part /. ((sko l) *. (sko slst)) in
           loop l (n - 1) (secpart :: acc)
  in loop lst 10 []

let generate_by_func