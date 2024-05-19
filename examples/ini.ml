open Ocom

type key_t = string
type value_t = string
type pair_t = key_t * value_t
type section_t =
  { name : string
  ; pairs : pair_t list
  }

let show_pair ((key, value) : pair_t) : string =
  Printf.sprintf "(%s, %s)" key value

let show_pairs (pairs : pair_t list) : string =
  pairs
  |> List.map show_pair
  |> String.concat ","
  |> Printf.sprintf "[%s]"

let show_section (section : section_t) : string =
  Printf.sprintf
  	"{ name = %s; pairs = %s }"
  	section.name
  	(show_pairs section.pairs)

let show_sections (sections : section_t list) : string =
  sections
  |> List.map show_section
  |> String.concat ","
  |> Printf.sprintf "[%s]"

let read_whole_file (file_path : string) : string =
	let ch = open_in file_path in
	let n = in_channel_length ch in
	let s = really_input_string ch n in
	close_in ch;
	s

let section_name : string parser =
  prefix "[" *> parse_while (( != ) ']') <* prefix "]"

let is_space (x : char) = x == ' ' || x == '\n'

let ws : string parser =
  parse_while is_space

let pair: pair_t parser =
  let name = parse_while (fun x -> not (is_space x) && x != '=') in
	(ws *> name <* ws <* prefix "=" <* ws) <*> (name <* ws)

let section: section_t parser =
  section_name <*> many pair
  |> map (fun (name, pairs) -> { name; pairs })

let ini: section_t list parser =
  many section

let () =
  match Sys.argv |> Array.to_list with
  | _ :: file_path :: _ ->
		let result = file_path
              	 |> read_whole_file
              	 |> make_input
              	 |> ini.run
    in
    (match result with
    | Ok (_, sections) -> sections |> show_sections |> print_endline
    | Error error -> Printf.printf
    								   "Error during parding at position %d: %s\n"
                       error.pos
                       error.desc)
  | _ -> failwith "Expected path to an ini file"
