open! Core

let ak = Bigarray.Int
let al = Bigarray.c_layout

let mk_array lines =
  Bigarray.Array2.init ak al (List.length lines)
    (String.length @@ List.nth_exn lines 0)
  @@ fun i j -> Int.of_string @@ String.slice (List.nth_exn lines i) j (j + 1)

let _print_2d_array ba =
  for i = 0 to Bigarray.Array2.dim1 ba - 1 do
    printf "\n";
    for j = 0 to Bigarray.Array2.dim2 ba - 1 do
      printf "%d" @@ Bigarray.Array2.get ba i j
    done
  done

(* let print_1d_array ba1 = *)
(*   for i = 0 to Bigarray.Array1.dim ba1 - 1 do *)
(*     printf "%d," @@ Bigarray.Array1.get ba1 i *)
(*   done *)

type loop_direction = Right | Left | Up | Down

let rec loop ba needle direction i j =
  let (nexti, nextj) = match direction with
  | Right -> (i, j - 1)
  | Left -> (i, j + 1)
  | Up -> (i - 1, j)
  | Down -> (i + 1, j)
  in
  let v = try Bigarray.Array2.get ba nexti nextj with Invalid_argument _ -> -1 in
  if v = -1 then true 
  else if v >= needle then false
  else loop ba needle direction nexti nextj

let is_visible ba ci cj =
  let max_i = Bigarray.Array2.dim1 ba - 1 in
  let max_j = Bigarray.Array2.dim2 ba - 1 in
  let needle = ba.{ci, cj} in
  if ci = max_i || ci = 0 || cj = max_j || cj = 0 then
    true
  else
    loop ba needle Right ci cj || loop ba needle Left ci cj || loop ba needle Up ci cj || loop ba needle Down ci cj

let count_visible ba =
  let count = ref 0 in
  for i = 0 to Bigarray.Array2.dim1 ba - 1 do
    for j = 0 to Bigarray.Array2.dim2 ba - 1 do
      if is_visible ba i j then count := !count + 1;
    done
  done;
  !count

let () =
  let lines = In_channel.read_lines "./input8.txt" in
  let in_array = mk_array lines in
  printf "\nSolution Part 1: %d\n" @@ count_visible in_array;
  printf "Solution Part 2: %d\n" 0
