open Base
open Stdio

let parse_re = Re.Pcre.regexp {|(\d+)-(\d+),(\d+)-(\d+)|}

let parse_line line =
  let a = Re.Pcre.extract ~rex:parse_re line in
  let get n = Int.of_string (Array.get a n) in
  let range1 = (get 1, get 2) in
  let range2 = (get 3, get 4) in
  (range1, range2)

let range_contains (l1, u1) (l2, u2) = l1 <= l2 && u1 >= u2

let part_1 =
  List.count ~f:(fun (r1, r2) -> range_contains r1 r2 || range_contains r2 r1)

let ranges_overlap (l1, u1) (l2, u2) =
  (l1 <= l2 && l2 <= u1) || (l2 <= l1 && l1 <= u2)

let part_2 = List.count ~f:(fun (r1, r2) -> ranges_overlap r1 r2)

let () =
  let lines = In_channel.read_lines "./input4.txt" in
  let parsed = List.map ~f:parse_line lines in
  printf "\nSolution part 1: %d\nSolution part 2: %d\n" (part_1 parsed)
    (part_2 parsed)
