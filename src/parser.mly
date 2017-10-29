%{
open Syntax
%}

%token RIGHT LEFT INCR DECR OUTPUT INPUT BEGIN END EOF

%start toplevel
%type <Syntax.com list> toplevel
%%

toplevel : c=ComList EOF { c }

ComList :
    { [] }
  | head=Com tail=ComList { head :: tail }

Com :
    RIGHT  { Instr { ptr = 1; mem = [] } }
  | LEFT   { Instr { ptr = -1; mem = [] } }
  | INCR   { Instr { ptr = 0; mem = [ (0, Plus (Mem 0, Const 1)) ] } }
  | DECR   { Instr { ptr = 0; mem = [ (0, Plus (Mem 0, Const (-1))) ] } }
  | OUTPUT { Output }
  | INPUT  { Input }
  | BEGIN c=ComList END { Loop c }
