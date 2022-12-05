open Base
open Stdio

(*
            [Q]     [G]     [M]    
            [B] [S] [V]     [P] [R]
    [T]     [C] [F] [L]     [V] [N]
[Q] [P]     [H] [N] [S]     [W] [C]
[F] [G] [B] [J] [B] [N]     [Z] [L]
[L] [Q] [Q] [Z] [M] [Q] [F] [G] [D]
[S] [Z] [M] [G] [H] [C] [C] [H] [Z]
[R] [N] [S] [T] [P] [P] [W] [Q] [G]
 1   2   3   4   5   6   7   8   9 
*)

let make_initial_state () =
  [|
    String.to_list "QFLSR";
    String.to_list "TPGQZN";
    String.to_list "BQMS";
    String.to_list "QBCHJZGT";
    String.to_list "SFNBMHP";
    String.to_list "GVLSNQCP";
    String.to_list "FCW";
    String.to_list "MPVWZGHQ";
    String.to_list "RNCLDZG";
  |]

type instruction = { n : int; from_ : int; to_ : int }

let parse_instruction line =
  let rex = Re.Pcre.regexp {|move (\d+) from (\d+) to (\d+)|} in
  let ext = Re.Pcre.extract ~rex line in
  {
    n = Int.of_string (Array.get ext 1);
    from_ = Int.of_string (Array.get ext 2) - 1;
    to_ = Int.of_string (Array.get ext 3) - 1;
  }

let execute_instruction_part_1 state { n; from_; to_ } =
  let f = Array.get state from_ in
  let t = Array.get state to_ in
  let new_f = List.drop f n in
  let new_t = List.rev_append (List.take f n) t in
  Array.set state from_ new_f;
  Array.set state to_ new_t

let execute_instruction_part_2 state { n; from_; to_ } =
  let f = Array.get state from_ in
  let t = Array.get state to_ in
  let new_f = List.drop f n in
  let new_t = List.append (List.take f n) t in
  Array.set state from_ new_f;
  Array.set state to_ new_t

let get_topline state =
  String.of_char_list @@ Array.to_list
  @@ Array.map state ~f:(fun l -> List.hd_exn l)

let () =
  let lines = In_channel.read_lines "./input5.txt" in
  let instructions = List.map ~f:parse_instruction lines in
  let state1 = make_initial_state () in
  let state2 = make_initial_state () in
  List.iter ~f:(execute_instruction_part_1 state1) instructions;
  printf "\nSolution part 1: %s\n" (get_topline state1);
  List.iter ~f:(execute_instruction_part_2 state2) instructions;
  printf "Solution part 2: %s\n" (get_topline state2)
