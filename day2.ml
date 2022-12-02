open Base
open Stdio

(* TODO: show *)
type move =
  | Rock
  | Paper
  | Scissors

type outcome =
  | Won
  | Lost
  | Tied

let parse_move = function
  | "A" -> Rock
  | "B" -> Paper
  | "C" -> Scissors
  | _   -> failwith "Unrecognized char (move)"

let parse_response = function
  | "X" -> Rock
  | "Y" -> Paper
  | "Z" -> Scissors
  | _   -> failwith "Unrecognized char (response)"

let parse_line line =
  let split = String.split ~on:' ' line in
  match split with
  | []       -> failwith "Empty Line"
  | _::[]    -> failwith "Too few characters"
  | x::y::[] -> (parse_move x, parse_response y)
  | _        -> failwith "Too many characters"

let outcome = function
  | (Rock, Scissors)  -> Lost
  | (Rock, Paper)     -> Won
  | (Paper, Rock)     -> Lost
  | (Paper, Scissors) -> Won
  | (Scissors, Paper) -> Lost
  | (Scissors, Rock)  -> Won
  | _                 -> Tied

let score_round round =
  let score_for_move = match round with
    | (_, Rock) -> 1
    | (_, Paper) -> 2
    | (_, Scissors) -> 3
   in
   let score_for_outcome = match outcome round with
     | Won -> 6
     | Lost -> 0
     | Tied -> 3
   in
   score_for_move + score_for_outcome

let () =
  let ic = In_channel.read_all "./input2.txt" in
  let split = String.split ~on:'\n' ic in
  let lines = List.filter ~f:(Fn.compose not String.is_empty) split in
  let scores = List.map lines ~f:(fun l -> parse_line l |> score_round) in
  let part_1 = List.fold_left ~init:0 ~f:(+) scores in
  printf "\nPart 1: %d" part_1
