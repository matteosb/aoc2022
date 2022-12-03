open Base
open Stdio

let score_char c =
  if Char.is_lowercase c then Char.to_int c - 96 else Char.to_int c - 38

let score_intersection intersection = 
  match Set.to_list intersection with
  | [] -> failwith "Empty intersection!"
  | x :: [] -> score_char x
  | _ -> failwith "Too much overlap"

let score_line_part_1 line =
  let char_list = String.to_list line in
  let list_len = List.length char_list in
  let lhs, rhs = List.split_n char_list (list_len / 2) in
  let lhs_set = Set.of_list (module Char) lhs in
  let rhs_set = Set.of_list (module Char) rhs in
  score_intersection (Set.inter lhs_set rhs_set)

let score_chunk_part_2 chunk =
  let to_char_set l = Set.of_list (module Char) (String.to_list l) in
  let chunk_set = List.map ~f:to_char_set chunk in
  let intersection = List.reduce ~f:Set.inter chunk_set in
  score_intersection (Option.value_exn intersection)
let () =
  let lines = In_channel.read_lines "./input3.txt" in
  let solution_1 =
    List.map ~f:score_line_part_1 lines |> List.fold_left ~init:0 ~f:( + )
  in
  let solution_2 =
    List.chunks_of ~length:3 lines
    |> List.map ~f:score_chunk_part_2
    |> List.fold_left ~init:0 ~f:( + )
  in
  printf "\nSolution Part 1: %d\nSolution Part 2: %d\n" solution_1 solution_2