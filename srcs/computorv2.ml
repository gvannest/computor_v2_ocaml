
let main () =
    while true do
        print_string "> ";
        let str = read_line() in
        if str = "q" then exit 0;
        if String.length str <> 0 then
            Printf.printf "%f\n" (Lexer_Parser.process_line str)
        (* let expr = read_expression str in
        let res = eval expr in *)
        (* Printf.printf " = %s\n%!" str; *)
    done

let () = main ()