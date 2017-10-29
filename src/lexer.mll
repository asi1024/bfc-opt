{
exception LexerError of string
}

rule comment_line = parse
| ['\n'] { main lexbuf }
| _ { comment_line lexbuf }

and comment_nested = parse
| "*/" { main lexbuf }
| _ { comment_nested lexbuf }

and main = parse
| [' ' '\009' '\012' '\n']+  { main lexbuf }
| "//" { comment_line lexbuf }
| "/*" { comment_nested lexbuf }

| ">" { Parser.RIGHT }
| "<" { Parser.LEFT }
| "+" { Parser.INCR }
| "-" { Parser.DECR }
| "." { Parser.OUTPUT }
| "," { Parser.INPUT }
| "[" { Parser.BEGIN }
| "]" { Parser.END }
| eof { Parser.EOF }
