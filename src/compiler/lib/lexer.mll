rule token = parse
        | _
                { token lexbuf }
