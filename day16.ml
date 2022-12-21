open! Core
open! Aoc

type valve = { name : string; flow_rate : int; leads_to : string list }
[@@deriving show, eq, compare]

type valve_list = valve list [@@deriving show]

module ValveMap = Map.Make (String)
module VisitedSet = Set.Make (String)

let valve_parser =
  Angstrom.(
    let valve_p =
      take_while1 (fun c -> Char.is_alpha c && Char.is_uppercase c)
    in
    let mk_valve name flow_rate leads_to = { name; flow_rate; leads_to } in
    let line_parser =
      mk_valve
      <$> string "Valve " *> valve_p
      <* string " has flow rate=" <*> integer_parser <* pluralized_p "; "
      <* pluralized_p "tunnel" <* whitespace <* pluralized_p "lead"
      <* whitespace <* pluralized_p "to valve" <* whitespace
      <*> (sep_by (string ", ") valve_p <|> map ~f:(fun v -> [ v ]) valve_p)
    in
    sep_by end_of_line line_parser)

let mk_valve_map valves =
  List.map valves ~f:(fun v -> (v.name, v)) |> ValveMap.of_alist_exn

module MemoKey = struct
  type t = string * int * string list [@@deriving compare, sexp, show, eq, hash]
end

module Memo = Hashtbl.Make (MemoKey)

let solve1 valves =
  let all_possible_active =
    ValveMap.data valves
    |> List.filter ~f:(fun { flow_rate; _ } -> flow_rate > 0)
    |> List.map ~f:(fun { name; _ } -> name)
    |> VisitedSet.of_list
  in
  let memo = Memo.of_alist_exn [] in
  let rec loop current_node turns_remaining active_set score =
    let k = (current_node, turns_remaining, VisitedSet.to_list active_set) in
    Memo.find_or_add memo k ~default:(fun () ->
        let current_value = ValveMap.find_exn valves current_node in
        if
          turns_remaining <= 0
          || VisitedSet.equal active_set all_possible_active
        then score
        else
          let next_score, next_tr, next_as =
            if
              current_value.flow_rate = 0
              || VisitedSet.mem active_set current_node
            then (score, turns_remaining - 1, active_set)
            else
              ( score + max 0 ((turns_remaining - 1) * current_value.flow_rate),
                turns_remaining - 2,
                VisitedSet.add active_set current_node )
          in
          current_value.leads_to
          |> List.map ~f:(fun n -> loop n next_tr next_as next_score)
          |> List.max_elt ~compare:Int.compare
          |> Option.value_exn)
  in
  loop "AA" 30 VisitedSet.empty 0

let () =
  let valves = parse_input_file "./input16.txt" valve_parser in
  printf "\nSolution Part 1: %d\n" @@ solve1 (mk_valve_map valves);
  printf "Solution Part 2: %d\n" 0
