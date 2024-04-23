open Parser
open Interpreter
open Lexer

let read_file filename =
  let channel = open_in filename in
  let content = really_input_string channel (in_channel_length channel) in
  close_in channel;
  content

let evaluate_query program query =
  let outcome = interpret_program program query in
  if fst outcome then
    if snd outcome = "" then print_string "true" else print_string (snd outcome)
  else print_string "false";
  print_endline ".\n"

let main () =
  try
    if Array.length Sys.argv <> 2 then failwith "Usage: ./main <filename>"
    else
      let filename = Sys.argv.(1) in
      let program_text = read_file filename in
      let lexbuf = Lexing.from_string program_text in
      let program = Parser.program Lexer.read lexbuf in
      print_endline "Prolog program loaded successfully!";
      print_endline "\nWelcome to Prolog Interpreter.";
      print_endline "Enter queries to evaluate, or type 'quit' to exit.\n";
      print_string "?- ";
      flush stdout;

      let rec process_queries program =
        let query_text = read_line () in
        if query_text = "quit" then ()
        else (
          (try
             let query =
               Parser.goal Lexer.read (Lexing.from_string query_text)
             in
             evaluate_query program query
           with Stdlib.Parsing.Parse_error | Lexer.Failed _ ->
             print_endline "Please enter a valid goal.\n");

          print_string "?- ";
          flush stdout;
          process_queries program)
      in
      process_queries program
  with
  | Lexer.Failed msg -> prerr_endline msg
  | Stdlib.Parsing.Parse_error -> prerr_endline "Program couldn't be parsed."

let () = main ()
