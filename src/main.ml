let compile in_name =
  let out_name =
    ( String.split_on_char '/' in_name |> List.rev |> List.hd
      |> String.split_on_char '.' |> List.rev |> List.tl |> List.rev
      |> String.concat "." ) ^ ".c" in
  let in_chan = open_in in_name in
  let out_chan = open_out out_name in
  ( try
      let com = Parser.toplevel Lexer.main (Lexing.from_channel in_chan) in
      output_string out_chan (Syntax.string_of_comlist com)
    with
    | Lexer.LexerError mes -> print_endline ("LexerError : " ^ mes)
    | Parsing.Parse_error -> print_endline "Parse_error" );
  close_in in_chan;
  close_out out_chan

let arg_specs = []

let anon_fun = (fun s -> compile s)

let usage_msg = ""

let () = Arg.parse arg_specs anon_fun usage_msg
