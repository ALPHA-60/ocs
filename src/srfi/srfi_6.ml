open Ocs_error
open Ocs_types
open Ocs_env
  
let open_input_string =
  function
    Sstring s -> Sport (Ocs_port.string_input_port s)
  | _ -> raise (Error "open_input_string: not a string")
;;

let open_output_string e =
    Sport (Ocs_port.string_output_port ())
;;
  
let get_output_string =
  function
    Sport p -> let module P = Ocs_port in
	       if not (P.is_string_port p) then
		 raise (Error "get-output-string: not a string port")
	       else if P.is_closed p then
		 raise (Error "get-output-string: port is closed")
	       else
		 Sstring (P.get_string p)
  | _ -> raise (Error "get-output-string: not a string output port")
;;


let init e =
  set_pf1 e open_input_string "open-input-string";
  set_pf0 e open_output_string "open-output-string";
  set_pf1 e get_output_string "get-output-string";
;;
