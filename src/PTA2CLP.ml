(***************************************************
 *
 *                     TA2CLP
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2011/11/03 (after first draft created in 10/2010)
 * Last modified: 2011/11/03
 *
 **************************************************)


open Global
open AbstractModel


(**************************************************)
(** Program *)
(**************************************************)

(*(* Convert a var_type into a string *)
let string_of_var_type = function
	| Var_type_clock -> "clock"
	| Var_type_discrete -> "discrete"
	| Var_type_parameter -> "parameter"*)


let all_vars = ref ""

let update_all_vars program = 
	let all_variables = list_append program.clocks program.parameters in
	let all_vars_string =
		if List.length all_variables > 0 then
			(", " ^ (string_of_list_of_string_with_sep ", " (List.map (LinearConstraint.string_of_var program.variable_names) all_variables)) ^ "")
			else ""
	in all_vars := all_vars_string

	

(* Add a header to the program *)
let string_of_header program =
	(* Retrieve the input options *)
	let options = Input.get_options () in
		"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
	^ "\n" ^"% Program " ^ options#file
	^ "\n" ^"%"
	^ "\n" ^"% Automatically generated by TA2CLP"
(* 	^ "\n" ^" * Generated at time " ^ time? *)
	^ "\n" ^"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
	^ "\n" ^"%"
	^ "\n" ^"% CLP for reals"
	^ "\n" ^":- use_module(library(clpr))."



(* Convert the invariant of a location into a string *)
let string_of_invariant program automaton_index location_index =
(*	"while "
	^ (LinearConstraint.string_of_linear_constraint program.variable_names (program.invariants automaton_index location_index))
	^ " wait"*)
	"\n\t% INVARIANT"
	^ "\n\tinv(" ^ (program.location_names automaton_index location_index) ^ ")"
(* 	(LinearConstraint.string_of_linear_constraint program.variable_names (program.invariants automaton_index location_index)) *)
(* 	^ " wait" *)
	^ "."


(* Convert a sync into a string *)
let string_of_sync program action_index = "not implemented" (*
	match program.action_types action_index with
	| Action_type_sync -> " sync " ^ (program.action_names action_index)
	| Action_type_nosync -> " (* sync " ^ (program.action_names action_index) ^ "*) "
*)

(* Convert a list of updates into a string *)
let string_of_updates program updates = "not implemented" (*
	string_of_list_of_string_with_sep ", " (List.map (fun (variable_index, linear_term) ->
		(* Convert the variable name *)
		(program.variable_names variable_index)
		^ "' = "
		(* Convert the linear_term *)
		^ (LinearConstraint.string_of_linear_term program.variable_names linear_term)
	) updates)
*)

(* Convert a transition of a location into a string *)
let string_of_transition program automaton_index action_index (guard, clock_updates, discrete_updates, destination_location) =
"not implemented" (*
	"\n\t" ^ "when "
	(* Convert the guard *)
	^ (LinearConstraint.string_of_linear_constraint program.variable_names guard)
	(* Convert the updates *)
	^ " do {"
	^ (string_of_updates program (list_append clock_updates discrete_updates))
	^ "} "
	(* Convert the sync *)
	^ (string_of_sync program action_index)
	(* Convert the destination location *)
	^ " goto " ^ (program.location_names automaton_index destination_location)
	^ ";"
*)

(* Convert the transitions of a location into a string *)
let string_of_transitions program automaton_index location_index = "not implemented" (*
	string_of_list_of_string (
	(* For each action *)
	List.map (fun action_index -> 
		(* Get the list of transitions *)
		let transitions = program.transitions automaton_index location_index action_index in
		(* Convert to string *)
		string_of_list_of_string (
			(* For each transition *)
			List.map (string_of_transition program automaton_index action_index) transitions
			)
		) (program.actions_per_location automaton_index location_index)
	)*)


(* Convert a location of an automaton into a string *)
let string_of_location program automaton_index location_index = "not implemented" (*
	"\n" ^ "loc: "
	^ (program.location_names automaton_index location_index)
	^ ": "
	^ (string_of_invariant program automaton_index location_index)
	^ (string_of_transitions program automaton_index location_index)
*)

(* Convert the locations of an automaton into a string *)
let string_of_locations program automaton_index =
	string_of_list_of_string_with_sep "\n " (List.map (fun location_index ->
		string_of_location program automaton_index location_index
	) (program.locations_per_automaton automaton_index))


(* Convert an automaton into a string *)
let string_of_automaton program automaton_index =
	"\n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
	^ "\n automaton " ^ (program.automata_names automaton_index)
	^ "\n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
(* 	^ "\n " ^ (string_of_synclabs program automaton_index) *)
(* 	^ "\n " ^ (string_of_initially program automaton_index) *)
	^ "\n " ^ (string_of_locations program automaton_index)
	^ "\n end (* " ^ (program.automata_names automaton_index) ^ " *)"
	^ "\n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"


(* Convert the automata into a string *)
let string_of_automata program =
	string_of_list_of_string_with_sep "\n\n" (
		List.map (fun automaton_index -> string_of_automaton program automaton_index
	) program.automata)

(* Convert an automaton into a string *)
let string_of_program program =
	(* Update the list of vars *)
	update_all_vars program;
	(* Return the program *)
	string_of_header program
	^  "\n" ^ string_of_automata program
	^ !all_vars

