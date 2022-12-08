open! Core

let dims a =
  let dimx = Array.length a in
  let dimy = Array.length a.(0) in
  (dimx, dimy)

let iter_array a f =
  let dimx, dimy = dims a in
  for i = 0 to dimx - 1 do
    for j = 0 to dimy - 1 do
      f (i, j) a.(i).(j)
    done
  done

let mk_array lines =
  let dimx = List.length lines in
  let dimy = String.length @@ List.nth_exn lines 0 in
  let a = Array.make_matrix ~dimx ~dimy 0 in
  iter_array a (fun (i, j) _ ->
      let v = Int.of_string @@ String.slice (List.nth_exn lines i) j (j + 1) in
      a.(i).(j) <- v);
  a

type loop_direction = Right | Left | Up | Down

let is_visible ba ci cj =
  let rec loop needle direction i j =
    let nexti, nextj =
      match direction with
      | Right -> (i, j - 1)
      | Left -> (i, j + 1)
      | Up -> (i - 1, j)
      | Down -> (i + 1, j)
    in
    match Option.try_with (fun () -> ba.(nexti).(nextj)) with
    | None -> true
    | Some n -> if n >= needle then false else loop needle direction nexti nextj
  in
  let nd = ba.(ci).(cj) in
  loop nd Right ci cj || loop nd Left ci cj || loop nd Up ci cj
  || loop nd Down ci cj

let solve_part_1 ba =
  let count = ref 0 in
  iter_array ba (fun (i, j) _ -> if is_visible ba i j then count := !count + 1);
  !count

let score ba ci cj =
  let rec loop needle direction i j =
    let nexti, nextj =
      match direction with
      | Right -> (i, j - 1)
      | Left -> (i, j + 1)
      | Up -> (i - 1, j)
      | Down -> (i + 1, j)
    in
    match Option.try_with (fun () -> ba.(nexti).(nextj)) with
    | None -> 0
    | Some n -> if n >= needle then 1 else 1 + loop needle direction nexti nextj
  in
  let nd = ba.(ci).(cj) in
  loop nd Right ci cj * loop nd Left ci cj * loop nd Up ci cj
  * loop nd Down ci cj

let solve_part_2 ba =
  let max_score = ref 0 in
  iter_array ba (fun (i, j) _ ->
      let s = score ba i j in
      if s > !max_score then max_score := s);
  !max_score

let () =
  let lines = In_channel.read_lines "./input8.txt" in
  let in_array = mk_array lines in
  printf "\nSolution Part 1: %d\n" @@ solve_part_1 in_array;
  printf "Solution Part 2: %d\n" @@ solve_part_2 in_array
