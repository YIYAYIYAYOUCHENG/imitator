(*****************************************************************
 *
 *                       PaTATOR
 * 
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 * 
 * Author:        Etienne Andre, Camille Coti
 * 
 * Created:       2014/03/24
 * Last modified: 2014/04/25
 *
 ****************************************************************)
 
(* open Global *)
open Mpi






(****************************************************************)
(** Messages *)
(****************************************************************)


(****************************************************************)
(** Debug modes *)
(****************************************************************)

type debug_mode =
	| Debug_error (* c'est quoi ca ? *)
	| Debug_nodebug
	| Debug_standard
	| Debug_low
	| Debug_medium
	| Debug_high
	| Debug_total

(* Associate an integer to each debug mode *)
let level_of_debug = function
	| Debug_error -> 0
	| Debug_nodebug -> 0
	| Debug_standard -> 1
	| Debug_low -> 2
	| Debug_medium -> 3
	| Debug_high -> 4
	| Debug_total -> 5

(* The global debug mode *)
type global_debug_mode_type =
	| Debug_mode_not_set
	| Debug_mode_set of debug_mode

(* set to standard by default *)
(*** WARNING: hard coded ***)
let global_debug_mode = ref (Debug_mode_set Debug_medium)



(* Get the debug mode *)
let get_debug_mode () =
	match !global_debug_mode with
	| Debug_mode_not_set -> raise (Failure ("The debug mode has not yet been set."))
	| Debug_mode_set debug_mode -> debug_mode


(* Return true if the global debug mode is greater than 'debug_mode', false otherwise *)
let debug_mode_greater debug_mode =
	(* Get the global debug mode *)
	let global_debug_mode = get_debug_mode() in
	(* Compare *)
	(level_of_debug global_debug_mode) >= (level_of_debug debug_mode)

(* Print a string *)
let print_message_generic message =
	(* Print message *)
	print_string (message ^ "\n");
	(* Flush! *)
	flush Pervasives.stdout


(* Convert an array of string into a string *)
let string_of_array_of_string =
	Array.fold_left (fun the_string s -> the_string ^ s) ""

(* Returns a fresh string made of 'n' times 's' *)
let string_n_times n s =
	string_of_array_of_string (Array.make n s)


(* Print a message if global_debug_mode >= message_debug_mode *)
let print_message message_debug_mode message =
	(* Only print the message if its message_debug_mode is smaller or equal to the global_debug_mode *)
	if debug_mode_greater message_debug_mode then
		(* Compute the debug level *)
		let debug_level = level_of_debug message_debug_mode in
		(* Find number of blanks for indentation *)
		let nb_spaces = if debug_level-1 > 0 then debug_level-1 else 0 in
		(* Create blanks proportionnally to the debug_level (at least one space) *)
		let spaces = " " ^ (string_n_times nb_spaces "   ") in
		(* Add new lines and blanks everywhere *)
		let formatted_message = spaces ^ (Str.global_replace (Str.regexp "\n") ("\n" ^ spaces) message) in
		(* Print *)
		print_message_generic formatted_message


		
		

(****************************************************************)
(** Private types *)
(****************************************************************)
(** Tags sent by slave *)
type mpi_slave_tag =
	| Slave_result_tag







(****************************************************************)
(** Constants *)
(****************************************************************)
(* Who is the master? *)
let masterrank = 0 



(****************************************************************)
(** Serialization Functions *)
(****************************************************************)
(*------------------------------------------------------------*)
(* Pi0 *)
(*------------------------------------------------------------*)


;;
	

(****************************************************************)
(** MPI Functions *)
(****************************************************************)
(*** NOTE: le "ref 1" ne signifie rien du tout ***)
let weird_stuff() = ref 1



let int_of_slave_tag = function
	| Slave_result_tag -> 1

let slave_tag_of_int = function
	| 1 -> Slave_result_tag
	| other -> raise (Failure ("Impossible match '" ^ (string_of_int other) ^ "' in slave_tag_of_int."))


let size () = Mpi.comm_size Mpi.comm_world
let rank () = Mpi.comm_rank Mpi.comm_world


let message_MAX_SIZE = 100

(* Sends a result (first the size then the constraint), by the slave *)
let send_string (*linear_constraint*)im_result =
	let rank = rank() in

	print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] Entering send_constraint");
	let mlc = (*(*LinearConstraint.serialize_linear_constraint linear_constraint *) serialize_im_result*) im_result in
	let res_size = String.length mlc in

	print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] Serialized constraint '" ^ mlc ^ "'");
	
	(* Send the result: 1st send the data size, then the data *)
	print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] About to send the size (" ^ (string_of_int res_size) ^ ") of the constraint.");
	Mpi.send res_size masterrank (int_of_slave_tag Slave_result_tag) Mpi.comm_world;
	
	(*** HACK: cut the constraint to try to solve a strange bug with MPI ***)
	if res_size <= message_MAX_SIZE then(
		(* Normal situation *)
		print_message Debug_high ("[Worker " ^ (string_of_int rank) ^ "] About to send a constraint.");
		Mpi.send mlc masterrank (int_of_slave_tag Slave_result_tag) Mpi.comm_world ;
		print_message Debug_low ("[Worker " ^ (string_of_int rank) ^ "] Sent constraint '" ^ mlc ^ "'");
		()
	)else(
		(* Cutting situation *)
		print_message Debug_low ("[Worker " ^ (string_of_int rank) ^ "] About to cut a constraint into smaller parts.");
		let remainder = res_size mod message_MAX_SIZE in
		let nb_parts = res_size / message_MAX_SIZE in
		print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] There will be " ^ (string_of_int nb_parts) ^ " parts and " ^ (if remainder = 0 then "no" else "a") ^ " remainder.");
		for i = 0 to nb_parts - 1 do
			(* Cut the string *)
			let substring = String.sub mlc (i * message_MAX_SIZE) message_MAX_SIZE in
			print_message Debug_high ("[Worker " ^ (string_of_int rank) ^ "] About to send a piece of constraint.");
			Mpi.send substring masterrank (int_of_slave_tag Slave_result_tag) Mpi.comm_world ;
			print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] Sent piece of constraint #" ^ (string_of_int i) ^ " '" ^ substring ^ "'");
		done;
		
		(* Send the remainder if not null *)
		if remainder <> 0 then(
			(* Cut the string *)
			let substring = String.sub mlc (nb_parts * message_MAX_SIZE) remainder in
			print_message Debug_high ("[Worker " ^ (string_of_int rank) ^ "] About to send the last piece of a constraint.");
			Mpi.send substring masterrank (int_of_slave_tag Slave_result_tag) Mpi.comm_world ;
			print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] Sent (last) piece of constraint #" ^ (string_of_int nb_parts) ^ " '" ^ substring ^ "'");
		);
		
		print_message Debug_low ("[Worker " ^ (string_of_int rank) ^ "] Sent constraint '" ^ mlc ^ "'  in small pieces.");
		
		()
	)


let receive_pull_request () =
	print_message Debug_medium ("Entered function 'receive_pull_request'...");
	
	(* First receive the length of the data we are about to receive *)
    let (len, source_rank, tag) = 
		Mpi.receive_status Mpi.any_source Mpi.any_tag Mpi.comm_world
	in
	
	print_message Debug_medium ("MPI status received from " ^ ( string_of_int source_rank));
	let tag = slave_tag_of_int tag in
	print_message Debug_medium ("Tag decoded.");

	(* Is this a result or a simple pull ? *)
	match tag with
	| Slave_result_tag ->
		print_message Debug_medium ("[Master] Received Slave_result_tag from " ^ ( string_of_int source_rank) );
		print_message Debug_medium ("[Master] Expecting a result of size " ^ ( string_of_int len) );
		
		(*** HACK: cut the constraint to try to solve a strange bug with MPI ***)
		let res = 
		if len <= message_MAX_SIZE then(
			(* Normal situation *)
			print_message Debug_medium ("[Master] Constraint will be received in a single piece.");
			let res = Mpi.receive source_rank (int_of_slave_tag Slave_result_tag) Mpi.comm_world in
			print_message Debug_medium ("[Master] Reception done");
			res
		)else(
			(* Cutting situation *)
			print_message Debug_low ("[Master] About to reconstruct a constraint from small parts.");
			let remainder = len mod message_MAX_SIZE in
			let nb_parts = len / message_MAX_SIZE in
			
			print_message Debug_medium ("[Master] There will be " ^ (string_of_int nb_parts) ^ " parts and " ^ (if remainder = 0 then "no" else "a") ^ " remainder.");
			
			(* Create result *)
			let resulting_string = ref "" in
			
			for i = 0 to nb_parts - 1 do
				(* Get the string *)
				let res = Mpi.receive source_rank (int_of_slave_tag Slave_result_tag) Mpi.comm_world in
				resulting_string := !resulting_string ^ res;
				print_message Debug_medium ("[Master] Received piece of constraint #" ^ (string_of_int i) ^ ": '" ^ res ^ "'");
			done;
		
			(* Receive the remainder if not null *)
			if remainder <> 0 then(
				let res = Mpi.receive source_rank (int_of_slave_tag Slave_result_tag) Mpi.comm_world in
				resulting_string := !resulting_string ^ res;
				print_message Debug_medium ("[Master] Received (last) piece of constraint #" ^ (string_of_int nb_parts) ^ ": '" ^ res ^ "'");
			);
			
			print_message Debug_low ("[Master] Successfully received constraint '" ^ !resulting_string ^ "'  in small pieces.");
			
			(* Return result *)
			!resulting_string
		)
		
		in

		(* Print some information *)
		if debug_mode_greater Debug_medium then
			print_message Debug_medium ("[Master] Result was '" ^ (*!*)res ^ "'");
			
		(* Get the constraint *)
		let im_result = (*unserialize_im_result*) (*!*)res in
		source_rank , im_result
		



(* ** *** *** ***** *******       UTILS       ******* ***** *** *** ** *)




(* Initialize a slave                                                  *)

let init_slave rank size =
	print_message Debug_standard ("[Worker " ^ (string_of_int rank) ^ "] I am worker [" ^ (string_of_int rank) ^ "] in " ^ (string_of_int (size-1)) ^ ".");
;;


(* ** *** *** ***** *******       MASTER      ******* ***** *** *** ** *)


let master () = 

    print_message Debug_standard ("[Master] Hello world!");
    
	for i = 1 to 100 do
		print_message Debug_standard ("[Master] Waiting for a pull request");

		(* Get the pull_request *)
		let source_rank , result = receive_pull_request () in
	
		print_message Debug_standard ("[Master] Got a " ^ (string_of_int i) ^ "th pull request from slave " ^ (string_of_int source_rank) ^ "");
			
	done; (* while more pi0 *)

	print_message Debug_standard ("[Master] DONE" );
    
	()
	
;;
  
(* *** *** ***** *******      WORKER      ******* ***** *** *** *)

let worker () = 
    
	let rank = rank() in
	let size = size() in

    print_message Debug_standard ("[Worker " ^ (string_of_int rank) ^ "] Hello!");
	init_slave rank size;

    for i = 1 to 30 do

		(* Send the result *)
		send_string "1*0g8a1*1g3a-1*0>-17a1*0+1*1g24a1*0g8a-1*0>-17a-1*0>-17a1*0+1*1g17a1*0g8a1*1g3a-1*0>-17a1*0g8a1*1g3a-1*0>-17a1*1g3a1*0+1*1g17a1*0g8a1*0+1*1g24a1*0g8a-1*0>-17a1*1g3a-1*0>-17a1*0g8a1*0+1*1g17a-1*0>-17a1*1g3a1*0g8a-1*0>-17a1*1g3a1*0g8a-1*0>-17a1*1g3a1*0g8a1*1g3a1*0g8a1*1g3a1*0g8a1*1g3a1*0g8,B|B|false|13|12|9|0.0161981582642";
		print_message Debug_standard ("[Worker " ^ (string_of_int rank) ^ "] Sent a constraint #" ^ (string_of_int i));
    done;
	print_message Debug_standard ("[Worker " ^ (string_of_int rank) ^ "] I'm done.");
;;






	

(* * *** *** ***** *******    MAIN FUNCTION    ******* ***** *** *** * *)
let run () =
	let rank = Mpi.comm_rank comm_world in

	
	
 	(**************************************************)
	(* Starting here *)
	(**************************************************)

	(* Fork between master and slave *)
	if rank = masterrank then 
		master()
	else 
		worker()
	;
  
	(* At the end: synchronization barrier *)
	barrier comm_world;

	
 	(**************************************************)
	(* Bye bye! *)
	(**************************************************)
	exit 0


;;

(* Start here *)
run()
