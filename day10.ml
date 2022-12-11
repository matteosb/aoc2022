open! Core

let interesting_cycles = [ 20; 60; 100; 140; 180; 220 ]
let is_interesting_cycle i = List.exists ~f:(fun x -> x = i) interesting_cycles

type instruction = Noop | Addx of int [@@deriving show]

let cost = function Noop -> 1 | Addx _ -> 2

type state = {
  x : int;
  program : instruction list;
  current_instruction_cycle : int;
  total_cycles : int;
}
[@@deriving show]

let parse_instruction line =
  match String.split ~on:' ' line with
  | "noop" :: [] -> Noop
  | [ "addx"; n ] -> Addx (Int.of_string n)
  | _ -> failwith "Failed to parse instruction"

let next_state { x; program; current_instruction_cycle; total_cycles } =
  let total_cycles = total_cycles + 1 in
  if current_instruction_cycle > 1 then
    {
      x;
      program;
      current_instruction_cycle = current_instruction_cycle - 1;
      total_cycles;
    }
  else
    let completed_inst = List.hd_exn program in
    let next_program = List.tl_exn program in
    let next_x = match completed_inst with Noop -> x | Addx n -> x + n in
    {
      x = next_x;
      program = next_program;
      current_instruction_cycle =
        Option.value ~default:(-1) @@ Option.map ~f:cost (List.hd next_program);
      total_cycles;
    }

let signal_strength { x; total_cycles; _ } = x * total_cycles

let run_part_1 init_state ncycles =
  let range = List.init ncycles ~f:Fun.id in
  List.fold ~init:(init_state, 0) range ~f:(fun (prev_state, total) i ->
      let new_total =
        if is_interesting_cycle i then total + signal_strength prev_state
        else total
      in
      (next_state prev_state, new_total))

let run_part_2 init_state ncycles =
  let state = ref init_state in
  for i = 0 to ncycles - 1 do
    if i mod 40 = 0 then printf "\n";
    state := next_state !state;
    if Int.abs ((!state.x mod 40) - (i mod 40)) < 2 
    then printf "#"
    else printf "."
  done

let () =
  let lines = In_channel.read_lines "./input10.txt" in
  let instructions = List.map ~f:parse_instruction lines in
  let init_state =
    {
      x = 1;
      program = Noop :: instructions;
      current_instruction_cycle = 1;
      total_cycles = 0;
    }
  in
  let _, part1 = run_part_1 init_state 221 in
  printf "\nSolution Part 1: %d\n" part1;
  run_part_2 init_state 240;
