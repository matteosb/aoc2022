open! Core

type position = int * int 
[@@deriving show, sexp ] 

let position_cmp (a1, b1) (a2, b2) =
  let a_cmp = Int.compare a1 a2 in
  if a_cmp = 0 then Int.compare b1 b2 else a_cmp

module PositionSet = Set.Make (struct
  type t = position
  [@@deriving sexp]

  let compare = position_cmp
end)

type state = {
  head_position : position;
  tail_position : position;
  visited_set : PositionSet.t;
}

let initial_state =
  {
    head_position = (0, 0);
    tail_position = (0, 0);
    visited_set = PositionSet.of_list [ (0, 0) ];
  }

type direction = Up | Down | Left | Right
(* type inst = direction * int *)

let parse_inst line =
  match String.split ~on:' ' line with
  | [ "D"; n ] -> (Down, Int.of_string n)
  | [ "U"; n ] -> (Up, Int.of_string n)
  | [ "L"; n ] -> (Left, Int.of_string n)
  | [ "R"; n ] -> (Right, Int.of_string n)
  | _ -> failwith "failed to parse line"

let move_head (x, y) = function
  | Down -> (x, y - 1)
  | Up -> (x, y + 1)
  | Left -> (x - 1, y)
  | Right -> (x + 1, y)

let move_tail (hx, hy) (tx, ty) =
  let xdiff = hx - tx in
  let ydiff = hy - ty in
  match (xdiff, ydiff) with
  | 2, 0 -> (tx + 1, ty)
  | 2, 1 -> (tx + 1, hy)
  | 2, -1 -> (tx + 1, hy)
  | -2, 0 -> (tx - 1, ty)
  | -2, 1 -> (tx - 1, hy)
  | -2, -1 -> (tx - 1, hy)
  | 0, 2 -> (tx, ty + 1)
  | 1, 2 -> (hx, ty + 1)
  | -1, 2 -> (hx, ty + 1)
  | 0, -2 -> (tx, ty - 1)
  | 1, -2 -> (hx, ty - 1)
  | -1, -2 -> (hx, ty - 1)
  | _ -> (tx, ty)

let move_1 state direction =
  let next_head = move_head state.head_position direction in
  let next_tail = move_tail next_head state.tail_position in
  {
    head_position = next_head;
    tail_position = next_tail;
    visited_set = PositionSet.add state.visited_set next_tail;
  }

let rec exec_inst state (direction, n) =
  if n = 0 then state else exec_inst (move_1 state direction) (direction, n - 1)

let solve_part_1 instructions =
  let final_state = List.fold ~init:initial_state ~f:exec_inst instructions in
  PositionSet.length final_state.visited_set

let () =
  let lines = In_channel.read_lines "./input9.txt" in
  let instructions = List.map ~f:parse_inst lines in
  printf "\nSolution Part 1: %d\n" (solve_part_1 instructions);
  printf "Solution Part 2: %d\n" 0
