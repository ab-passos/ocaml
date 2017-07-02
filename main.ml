let stack init = object
    val mutable v = init

    method pop =
      match v with
      | hd :: tl ->
        v <- tl;
        Some hd
      | [] -> None

    method push hd =
      v <- hd :: v
  end ;;

let main () =
   let s = stack [3; 2; 1] in
   match s#pop with
   | Some x -> Printf.printf "Pop value is %d!\n" x
   | None -> print_string "None\n"
;;

let () = main()
