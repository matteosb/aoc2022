open Base
open Stdio

(* TODO: show *)
type move = Rock | Paper | Scissors
type outcome = Won | Lost | Tied

let parse_move = function
  | "A" -> Rock
  | "B" -> Paper
  | "C" -> Scissors
  | _ -> failwith "Unrecognized char (move)"

let parse_response1 _ = function
  | "X" -> Rock
  | "Y" -> Paper
  | "Z" -> Scissors
  | _ -> failwith "Unrecognized char (response)"

let parse_line line parse_response =
  let split = String.split ~on:' ' line in
  match split with
  | [] -> failwith "Empty Line"
  | _ :: [] -> failwith "Too few characters"
  | [ x; y ] -> (parse_move x, parse_response (parse_move x) y)
  | _ -> failwith "Too many characters"

let loses_to = function Rock -> Paper | Paper -> Scissors | Scissors -> Rock

let wins_against = function
  | Rock -> Scissors
  | Paper -> Rock
  | Scissors -> Paper

let parse_response2 m = function
  | "X" -> wins_against m
  | "Y" -> m
  | "Z" -> loses_to m
  | _ -> failwith "Unrecognized char (response)"

let outcome (m, r) =
  if phys_equal m (loses_to r) then Lost
  else if phys_equal m (wins_against r) then Won
  else Tied

let score_round round =
  let score_for_move =
    match round with _, Rock -> 1 | _, Paper -> 2 | _, Scissors -> 3
  in
  let score_for_outcome =
    match outcome round with Won -> 6 | Lost -> 0 | Tied -> 3
  in
  score_for_move + score_for_outcome

let () =
  let ic = In_channel.read_all "./input2.txt" in
  let split = String.split ~on:'\n' ic in
  let lines = List.filter ~f:(Fn.compose not String.is_empty) split in
  let score_fn parser l = score_round (parse_line l parser) in
  let scores1 = List.map lines ~f:(score_fn parse_response1) in
  let scores2 = List.map lines ~f:(score_fn parse_response2) in
  let solution1 = List.fold_left ~init:0 ~f:( + ) scores1 in
  let solution2 = List.fold_left ~init:0 ~f:( + ) scores2 in
  printf "\nSolution 1: %d\nSolution 2: %d\n" solution1 solution2