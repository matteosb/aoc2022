open! Core

let first_n_unique n lst =
  if List.length lst < n then false
  else
    List.take lst n |> List.find_a_dup ~compare:Char.compare |> Option.is_none

let solve n signal =
  let rec loop inpt acc idx =
    if first_n_unique n acc then idx
    else
      match inpt with
      | [] -> failwith "Reached end of signal"
      | x :: xs -> loop xs (x :: acc) (idx + 1)
  in
  loop signal [] 0

let () =
  let signal = In_channel.read_all "./input6.txt" |> String.to_list in
  printf "\nSolution Part 1: %d\n" @@ solve 4 signal;
  printf "Solution Part 2: %d\n" @@ solve 14 signal
