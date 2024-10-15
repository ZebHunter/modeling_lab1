open Plplot

let ( -- ) a b = 
  let rec loop a b acc = 
    match compare a b with
    | 1 -> List.rev acc
    | _ -> loop (a +. 1.) b (a :: acc)
  in Array.of_list @@ loop a b []

let posibility = function
  | 0.9 -> 1.643
  | 0.95 -> 1.960
  | 0.99 -> 2.576
  | _ -> failwith "Unknown Value!!!"

let data = Mod_lab1.Statistics.read_file "model1.csv"

let plot_early () =
  
  let xs = 0. -- 300. in
  let ys = Array.of_list @@ Mod_lab1.Statistics.read_file @@ "model1.csv" in

  plinit ();
  
  plscolbg 255 255 255;
  plcol0 0;  
  plenv 0.0 300.0 1.0 1200.0 0 0;
  pllab "X-axis" "Y-axis" "Sine Wave";
  
  plcol0 2;
  plline xs ys;

  plend ();
  ()


let print_statistics () =
    let sample_sizes = [10; 20; 50; 100; 200; 300] in
  
    (* Функция для получения результатов статистических вычислений для заданного размера выборки *)
    let get_stats_for_n n =
      let data_n = Mod_lab1.Statistics.list_to_nlist data n in
      let mean = Mod_lab1.Statistics.mean_val data_n in
      let variance = Mod_lab1.Statistics.norm_dispersion data_n in
      let sko = Mod_lab1.Statistics.sko data_n in
      let cov = Mod_lab1.Statistics.cov data_n in
      let interval_90 = Mod_lab1.Statistics.apply_interval posibility 0.90 data_n in
      let interval_95 = Mod_lab1.Statistics.apply_interval posibility 0.95 data_n in
      let interval_99 = Mod_lab1.Statistics.apply_interval posibility 0.99 data_n in
      (mean, variance, sko, cov, interval_90, interval_95, interval_99)
    in
  
    (* Формируем заголовок таблицы *)
    Printf.printf "| Характеристика |____|__10__|__20__|__50__|__100__|__200__|__300__|\n";
    
    (* Печать строки для каждой характеристики *)
    let print_row label get_stat =
      Printf.printf "| %-16s |Знач|" label;
      List.iter (fun n ->
        let stat = get_stat n in
        Printf.printf " %6.3f|" stat
      ) sample_sizes;
      print_endline ""  (* завершить строку таблицы *)
    in
  
    (* Печатаем статистику для каждого параметра *)
    print_row "Мат. ожидание" (fun n -> let (mean, _, _, _, _, _, _) = get_stats_for_n n in mean);
    print_row "Дисперсия" (fun n -> let (_, variance, _, _, _, _, _) = get_stats_for_n n in variance);
    print_row "СКО" (fun n -> let (_, _, sko, _, _, _, _) = get_stats_for_n n in sko);
    print_row "Коэф. вариации" (fun n -> let (_, _, _, cov, _, _, _) = get_stats_for_n n in cov);
  
    (* Печать доверительных интервалов *)
    print_row "Дов. инт. 0.90" (fun n -> let (_, _, _, _, interval_90, _, _) = get_stats_for_n n in interval_90);
    print_row "Дов. инт. 0.95" (fun n -> let (_, _, _, _, _, interval_95, _) = get_stats_for_n n in interval_95);
    print_row "Дов. инт. 0.99" (fun n -> let (_, _, _, _, _, _, interval_99) = get_stats_for_n n in interval_99);
  
    ()
  

                     

(*
(* Пример данных *)
let data = [|1.0; 2.3; 2.9; 3.5; 4.0; 5.0; 5.1; 6.7; 7.8; 8.5; 9.9|]

(* Функция для расчёта частот для гистограммы *)
let histogram data num_bins =
  let min_val = Array.fold_left min max_float data in
  let max_val = Array.fold_left max min_float data in
  let bin_width = (max_val -. min_val) /. float_of_int num_bins in
  let bins = Array.make num_bins 0.0 in
  
  Array.iter (fun x ->
    let index = int_of_float ((x -. min_val) /. bin_width) in
    let index = if index >= num_bins then num_bins - 1 else index in
    bins.(index) <- bins.(index) +. 1.0
  ) data;
  
  (bins, min_val, max_val, bin_width)

(* Функция для построения гистограммы *)
let plot_histogram data num_bins =
  let (bins, min_val, max_val, bin_width) = histogram data num_bins in

  (* Инициализация PLplot *)
  plinit ();

  (* Настройка осей и области построения *)
  plenv min_val max_val 0.0 (Array.fold_left max 0.0 bins) 0 0;
  pllab "X-axis" "Y-axis" "Histogram";

  (* Рисование столбцов гистограммы *)
  Array.iteri (fun i count ->
    let x0 = min_val +. float_of_int i *. bin_width in
    let x1 = x0 +. bin_width in
    let xs = [|x0; x1; x1; x0|] in
    let ys = [|0.0; 0.0; count; count|] in
    plfill xs ys  (* Рисуем заполненный прямоугольник *)
  ) bins;

  (* Завершение сессии PLplot *)
  plend ()
*)

let () = 
    plot_early ();
    print_statistics ()
