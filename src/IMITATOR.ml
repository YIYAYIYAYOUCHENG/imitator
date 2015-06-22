(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * LIPN, Universite Paris 13, Sorbonne Paris Cite (France)
 * 
 * Author:        Ulrich Kuehne, Etienne Andre
 * 
 * Created:       2009/09/07
 * Last modified: 2015/04/12
 *
 ****************************************************************)


(**************************************************)
(* External modules *)
(**************************************************)
open Gc


(**************************************************)
(* Internal modules *)
(**************************************************)
open Exceptions
open CamlUtilities

open ImitatorUtilities
open AbstractModel
(* open Arg *)
open ModelPrinter
open Options
open Reachability


(**************************************************

TAGS POUR CHOSES A FAIRE
- (*** TO DO ***)
- (*** BAD PROG ***)
- (*** TO OPTIMIZE ***)
- (*** OPTIMIZED ***)
- (*** WARNING ***)

<>

**************************************************)

;;


(**************************************************)
(**************************************************)
(* STARTING PROGRAM *)
(**************************************************)
(**************************************************)


(* TEST !! *)
(*LinearConstraint.test_PDBMs();
terminate_program();*)


(**************************************************)
(* BEGIN EXCEPTION MECHANISM *)
(**************************************************)
begin
try(


(**************************************************)
(* Get the arguments *)
(**************************************************)
(* object with command line options *)
let options = new imitator_options in

options#parse;

(* Set the options (for other modules) *)
Input.set_options options;


(**************************************************)
(**************************************************)
(* Print startup message *)
(**************************************************)
(**************************************************)
  
(* Print header *)
print_header_string();

(* Print date *)
print_message Verbose_standard ("Analysis time: " ^ (now()) ^ "\n");

(* Recall the arguments *)
options#recall(); 
    

(**************************************************)
(* Get input *)
(**************************************************)
let model, pi0, v0 = ParsingUtility.compile options in

Input.set_model model;
Input.set_pi0 pi0;
Input.set_v0 v0;


(**************************************************)
(* Debug print: model *)
(**************************************************)
if debug_mode_greater Verbose_total then
	print_message Verbose_total ("\nModel:\n" ^ (ModelPrinter.string_of_model model) ^ "\n");


(**************************************************)
(* Case distributed *)
(**************************************************)
(*** WARNING:  Do not modify the following lines! (used by an external script to compile the non-distributed version of IMITATOR) ***)
(* ** *** **** ***** ******    BEGIN FORK PaTATOR    ****** ***** **** *** ** *)
begin
match options#distribution_mode with
	(* Fork if distributed *)
	| Non_distributed -> ()
	| _ -> (PaTATOR.run(); exit(0))
end;
(* ** *** **** ***** ******    END FORK PaTATOR    ****** ***** **** *** ** *)
(*** WARNING:  Do not modify the previous lines! (used by an external script to compile the non-distributed version of IMITATOR) ***)



(**************************************************)
(* HERE IS THE BEGINNING OF GIA'S WORK *)
(**************************************************)

List.iter (fun automaton_index -> 
	print_message Verbose_standard (" Starting investigating automaton " ^ (model.automata_names automaton_index) );
	List.iter (fun location_index ->
		print_message Verbose_standard ("   Starting investigating location " ^ (model.location_names automaton_index location_index) );
		List.iter (fun action_index ->
			print_message Verbose_standard ("     Starting investigating action " ^ (model.action_names action_index) );
			List.iter (fun (guard, clock_updates, _, destlocation_index) ->
				print_message Verbose_standard ("       Starting investigating some transition guarded by " ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names guard));
					List.iter (fun clock_index ->
					print_message Verbose_standard ("         Starting checking clock " ^ (model.variable_names clock_index));
						if LinearConstraint.pxd_is_constrained guard clock_index then (print_message Verbose_standard "This clock is bound!!");
						
						
						(** MORE WORK HERE **)
						
						
					) model.clocks;
			) (model.transitions automaton_index location_index action_index);
		) (model.actions_per_location automaton_index location_index);
	) (model.locations_per_automaton automaton_index);
	

) model.automata;

terminate_program();



(**************************************************)
(* HERE IS THE END OF GIA'S WORK *)
(**************************************************)



(**************************************************)
(* Case translation *)
(**************************************************)

(* Translation to CLP (work in progress) *)
if options#pta2clp then(
	print_message Verbose_standard ("Translating model to CLP.");
	print_warning ("Work in progress!!!!");
	print_message Verbose_standard ("\nmodel in CLP:\n" ^ (PTA2CLP.string_of_model model) ^ "\n");
	terminate_program()
);

(* Translation to GrML (experimental) *)
if options#pta2gml then(
	print_message Verbose_standard ("Translating model to GrML.");
	let translated_model = PTA2GrML.string_of_model model in
	let gml_file = options#files_prefix ^ ".grml" in
	if debug_mode_greater Verbose_total then(
		print_message Verbose_total ("\n" ^ translated_model ^ "\n");
	);
	(* Write *)
	write_to_file gml_file translated_model;
	terminate_program()
);

(* Translation to JPG *)
if options#pta2jpg then(
	print_message Verbose_standard ("Translating model to a graphics.");
	let translated_model = PTA2JPG.string_of_model model in
	if debug_mode_greater Verbose_high then(
		print_message Verbose_high ("\n" ^ translated_model ^ "\n");
	);
	Graphics.dot model (options#files_prefix ^ "-pta") translated_model;
	terminate_program()
);

(* Translation to TikZ *)
if options#pta2tikz then(
	print_message Verbose_standard ("Translating model to LaTeX TikZ code.");
	let translated_model = PTA2TikZ.tikz_string_of_model model in
	let latex_file = options#files_prefix ^ ".tex" in
	if debug_mode_greater Verbose_high then(
		print_message Verbose_high ("\n" ^ translated_model ^ "\n");
	);
	(* Write *)
	write_to_file latex_file translated_model;
	terminate_program()
);
(* Direct cartography output *)
if options#cartonly then(
	print_message Verbose_standard ("Direct output of a cartography (no analysis will be run).");
	(* Get the parameters *)
	let constraints , (p1_min , p1_max) , (p2_min , p2_max) = model.carto in
	(* Transform the constraint for cartography *)
	let constraints = List.map (fun (linear_constraint , tile_nature) ->
		Convex_constraint (linear_constraint , tile_nature)
	) constraints in
	(* Create the v0 *)
	let v0 = new HyperRectangle.hyper_rectangle in
	v0#set_min 0 p1_min;
	v0#set_max 0 p1_max;
	v0#set_min 1 p2_min;
	v0#set_max 1 p2_max;
	(* Call the cartography *)
	Graphics.cartography model (*[| (p1_min , p1_max); (p2_min , p2_max) |]*) v0 constraints options#files_prefix;
	(* The end *)
	terminate_program()
);



(**************************************************)
(* Preliminary checks *)
(**************************************************)

if options#imitator_mode = EF_synthesis then(
	match model.correctness_condition with
		(* Synthesis only works w.r.t. (un)reachability *)
		| Some (Unreachable _) -> ()
		| _ -> print_error ("EF-synthesis can only be run if an unreachability property is defined in the model.");
			abort_program();
);


if (options#imitator_mode = Border_cartography && model.correctness_condition = None) then(
	print_error ("In border cartography mode, a correctness property must be defined.");
	abort_program();
);



(**************************************************)
(* EXPERIMENTAL: dynamic clock elimination *)
(**************************************************)
(* Need to be called before initial state is created! *)
if options#dynamic_clock_elimination then (
	Reachability.prepare_clocks_elimination model
);







(*(* TESTS *) 
print_message Verbose_standard ("\nInitial constraint:\n" ^ (LinearConstraint.string_of_linear_constraint model.variable_names initial_constraint_after_time_elapsing) ^ "\n");

(*let n = ref 1 in

List.iter (fun parameter_id ->
	LinearConstraint.time_elapse_assign [parameter_id] (list_diff model.parameters [parameter_id]) initial_constraint_after_time_elapsing;
	
	print_message Verbose_standard ("\nAfter time elapsing #" ^ (string_of_int !n) ^ " on parameter '" ^ (model.variable_names parameter_id) ^ "' :\n" ^ (LinearConstraint.string_of_linear_constraint model.variable_names initial_constraint_after_time_elapsing) ^ "\n");
	
	Graphics.cartography model v0 [Convex_constraint initial_constraint_after_time_elapsing] (options#file ^ "-carto" ^ (string_of_int !n));

	n := !n + 1;

) model.parameters;
(* Graphics.cartography model v0 [Convex_constraint initial_constraint_after_time_elapsing] (options#file ^ "-carto"); *)
terminate_program();*)


LinearConstraint.grow_to_zero_assign model.parameters model.clocks_and_discrete initial_constraint_after_time_elapsing;
print_message Verbose_standard ("\nFinal constraint:\n" ^ (LinearConstraint.string_of_linear_constraint model.variable_names initial_constraint_after_time_elapsing) ^ "\n");
Graphics.cartography model v0 [Convex_constraint initial_constraint_after_time_elapsing] (options#file ^ "-cartoz");
terminate_program();*)



(*(**************************************************)
(* EXPERIMENTAL: branch and bound *)
(**************************************************)

if options#imitator_mode = Inverse_method && options#branch_and_bound then(
	Reachability.branch_and_bound model pi0 init_state_after_time_elapsing;
	terminate_program();
);*)




(**************************************************)
(* Execute IMITATOR *)
(**************************************************)

begin
(* 	let zones = *)
	match options#imitator_mode with
		| Translation -> raise (InternalError "Translation cannot be executed here; program should already have terminated at this point.");

		
		(* Exploration *)
		| State_space_exploration
			-> Reachability.full_state_space_exploration model;
(* 			[] *)
			
		(* Synthesis *)
		| EF_synthesis 
			->
			(*[*)Reachability.ef_synthesis model(*]*)

			
		(* Inverse Method *)
		| Inverse_method ->
			if options#efim then
				(
					(*** WARNING!!! Why a dedicated function here, whereas for BC+EFIM this function is not (?) called? ***)
				Reachability.efim model;
(* 				[] *)
				)
			else(
				Reachability.inverse_method model;
(* 				[] *)
			)


		| Cover_cartography | Border_cartography ->
		(* Behavioral cartography algorithm with full coverage *)
			Cartography.cover_behavioral_cartography model
			
			
		| Random_cartography nb ->
		(* Behavioral cartography algorithm with random iterations *)
			Cartography.random_behavioral_cartography model nb;

			(*
	in

	(* Computation of the cartography *)
	if options#cart then (
			(* No cartography if no zone *)
			if zones = [] then(
				print_message Verbose_standard ("\nNo cartography can be generated since the list of constraints is empty.\n");
			)else(
				print_message Verbose_standard ("\nGeneration of the graphical cartography...");
				Graphics.cartography model v0 zones (options#files_prefix ^ "_cart")
			)
		) else (
			print_message Verbose_high "Graphical cartography not asked: graph not generated."
		)
	;*)
end;


(**************************************************)
(* END EXCEPTION MECHANISM *)
(**************************************************)
) with
(*** TODO: factorize a bit ***)
	| InternalError e -> (
		print_error ("Fatal internal error: " ^ e ^ "\nPlease (kindly) insult the developers.");
		abort_program ();
		(* Safety *)
		exit 1
	);
	| Failure msg -> (
		print_error ("'Failure' exception: '" ^ msg ^ "'\nPlease (kindly) insult the developers.");
		abort_program ();
		(* Safety *)
		exit 1
	);
	| Invalid_argument msg -> (
		print_error ("'Invalid_argument' exception: '" ^ msg ^ "'\nPlease (kindly) insult the developers.");
		abort_program ();
		(* Safety *)
		exit 1
	);
	| Not_found -> (
		print_error ("'Not_found' exception!\nPlease (kindly) insult the developers.");
		abort_program ();
		(* Safety *)
		exit 1
	);
	| _ -> (
		print_error ("An unknown exception occurred. Please (kindly) insult the developers.");
		abort_program ();
		(* Safety *)
		exit 1
	);
end; (* try *)



(**************************************************)
(* Bye bye! *)
(**************************************************)

(* Reachability.print_stats (); *)

terminate_program()
