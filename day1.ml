open Base
open Stdio

let rec process_lines current_sum acc lines =
  match lines with
  | [] -> current_sum :: acc
  | "" :: xs -> process_lines 0 (current_sum :: acc) xs
  | x :: xs -> process_lines (current_sum + Int.of_string x) acc xs

let () =
  let chan = In_channel.create "./input1.txt" in
  let all = In_channel.input_all chan in
  let sums = process_lines 0 [] @@ String.split all ~on:'\n' in
  let sorted = List.sort sums ~compare:(fun x y -> -Int.compare x y) in
  let solution1 = Option.value ~default:0 @@ List.nth sorted 0 in
  let solution2 = List.fold_left ~init:0 ~f:( + ) (List.take sorted 3) in
  printf "\nSolution 1: %d\nSolution 2: %d\n" solution1 solution2