(*****************************************************************
 *
 *                     HYMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2010/03/10
 * Last modified: 2010/03/16
 *
 ****************************************************************)

(**************************************************)
(* Modules *)
(**************************************************)
open Global



(**************************************************)
(** {2 Types} *)
(**************************************************)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Variables} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

type variable_index = int
type clock_index = variable_index
type parameter_index = variable_index
type discrete_index = variable_index
type variable_name = string
type value = NumConst.t

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Locations} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
type location_index = int
type location_name = string

(* Array automaton_index -> location_index *)
type locations = location_index array

(* Array discrete_index -> NumConst.t *)
type discrete = NumConst.t array

type location = locations * discrete

exception NotEqual

let location_equal loc1 loc2 =
	let (locs1, discr1) = loc1 in
	let (locs2, discr2) = loc2 in
	(* can use polymorphic = for locations here *)
	if not (locs1 = locs2) then false else (
		(* check all discrete values *)
		if not ((Array.length discr1) = (Array.length discr2)) then false else (
			try (
				Array.iteri (fun i d1 -> 
					if not (discr2.(i) = d1) then raise NotEqual
				) discr1;
				true
			) with _ -> false
			(* all entries equal *)			
		) 
	)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Automata} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

type automaton_index = int
type automaton_name = string




(**************************************************)
(** Global variables *)
(**************************************************)

(* The minimum discrete_index *)
let min_discrete_index = ref 0

(* The number of discrete variables *)
let nb_discrete = ref 0

(* The number of automata *)
let nb_automata = ref 0

(**************************************************)
(** Useful functions *)
(**************************************************)

let get_locations (locations, _) = locations

let get_discrete (_, discrete) = discrete

let hash_code location =
	let locations, discrete = location in
	let loc_hash = Array.fold_left (fun h loc -> 2*h + loc) 0 locations in
	let discr_hash = Array.fold_left (fun h q -> 
		2*h + (Gmp.Z.to_int (NumConst.get_num q))
	) 0 discrete in
	loc_hash + 3 * discr_hash

(* Replace a discrete variable by its name, considering the offset *)
let string_of_discrete names index =
	names (index + !min_discrete_index)


(**************************************************)
(** {2 Locations} *)
(**************************************************)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Initialization} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** 'initialize nb_automata min_discrete_index max_discrete_index' initializes the min and max discrete indexes and the number of automata. *)
let initialize nb_auto min_discrete max_discrete =
	min_discrete_index := min_discrete;
	nb_discrete := max_discrete - min_discrete + 1;
	nb_automata := nb_auto


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Creation} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** 'make_location locations discrete_values' creates a new location. All automata should be given a location. Discrete variables may not be given a value (in which case they will be initialized to 0). *)
let make_location locations_per_automaton discrete_values =
	(* Create an array for locations *)
	let locations = Array.make !nb_automata 0 in
	(* Create an array for discrete *)
	let discrete = Array.make !nb_discrete NumConst.zero in
	(* Iterate on locations *)
	List.iter (fun (automaton_index, location_index) -> locations.(automaton_index) <- location_index) locations_per_automaton;
	(* Iterate on discrete *)
	List.iter (fun (discrete_index, value) -> discrete.(discrete_index - !min_discrete_index) <- value) discrete_values;
	(* Return the new location *)
	locations, discrete


(** 'copy_location location' creates a fresh location identical to location. *)
let copy_location location =
	(* Create an array for locations *)
	let locations = Array.copy (get_locations location) in
	(* Create an array for discrete *)
	let discrete = Array.copy (get_discrete location) in
	(* Return the new location *)
	locations, discrete


(** 'update_location locations discrete_values location' creates a new location from the original location, and update the given automata and discrete variables. *)
let update_location locations_per_automaton discrete_values location =
	(* Create an array for locations *)
	let locations = Array.copy (get_locations location) in
	(* Create an array for discrete *)
	let discrete = Array.copy (get_discrete location) in
	(* Iterate on locations *)
	List.iter (fun (automaton_index, location_index) -> locations.(automaton_index) <- location_index) locations_per_automaton;
	(* Iterate on discrete *)
	List.iter (fun (discrete_index, value) -> discrete.(discrete_index - !min_discrete_index) <- value) discrete_values;
	(* Return the new location *)
	locations, discrete

(** Side-effet version of 'update_location'. *)
let update_location_with locations_per_automaton discrete_values (locations, discrete) =
	(* Iterate on locations *)
	List.iter (fun (automaton_index, location_index) -> locations.(automaton_index) <- location_index) locations_per_automaton;
	(* Iterate on discrete *)
	List.iter (fun (discrete_index, value) -> discrete.(discrete_index - !min_discrete_index) <- value) discrete_values


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Access} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Get the location associated to some automaton *)
let get_location location automaton_index =
	let locations = get_locations location in
	locations.(automaton_index)

(** Get the value associated to some discrete variable *)
let get_discrete_value location discrete_index =
	let discrete = get_discrete location in
	(* Do not forget the offset *)
	discrete.(discrete_index - !min_discrete_index)


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Conversion} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** 'string_of_location automata_names location_names discrete_names location' converts a location to a string. *)
let string_of_location automata_names location_names discrete_names location =
	(* Get the locations per automaton *)
	let locations = get_locations location in
	(* Get the values for discrete variables *)
	let discrete = get_discrete location in
	(* Convert the locations *)
	let string_array = Array.mapi (fun automaton_index location_index ->
		(automata_names automaton_index) ^ ": " ^ (location_names automaton_index location_index)
	) locations in
	let location_string = string_of_array_of_string_with_sep ", " string_array in
	(* Convert the discrete *)
	let string_array = Array.mapi (fun discrete_index value ->
		(string_of_discrete discrete_names discrete_index) ^ " = " ^ (NumConst.string_of_numconst value)
	) discrete in
	let discrete_string = string_of_array_of_string_with_sep ", " string_array in
	(* Return the string *)
	location_string ^ (if !nb_discrete > 0 then ", " else "") ^ discrete_string
	

	