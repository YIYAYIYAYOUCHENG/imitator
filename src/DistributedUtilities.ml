(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 * 
 * Author:        Etienne Andre, Camille Coti
 * 
 * Created:       2014/03/24
 * Last modified: 2014/04/24
 *
 ****************************************************************)

open Global
open Mpi


(****************************************************************)
(** Public types *)
(****************************************************************)
type rank = int

type pull_request =
	| PullOnly of rank
	| PullAndResult of rank * (*Reachability.im_result*)string
	| OutOfBound of rank

(****************************************************************)
(** Private types *)
(****************************************************************)
(** Tags sent by slave *)
type mpi_slave_tag =
	| Slave_result_tag
	| Slave_work_tag
	| Slave_outofbound_tag

(** Tags sent by master *)
type mpi_master_tag =
	| Master_data_tag
	| Master_finished_tag






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
	| Slave_work_tag -> 2
	| Slave_outofbound_tag -> 3

let int_of_master_tag = function
	| Master_data_tag -> 17
	| Master_finished_tag -> 18

let slave_tag_of_int = function
	| 1 -> Slave_result_tag
	| 2 -> Slave_work_tag
	| 3 -> Slave_outofbound_tag
	| other -> raise (InternalError ("Impossible match '" ^ (string_of_int other) ^ "' in slave_tag_of_int."))

let master_tag_of_int = function
	| 17 -> Master_data_tag 
	| 18 -> Master_finished_tag
	| other -> raise (InternalError ("Impossible match '" ^ (string_of_int other) ^ "' in master_tag_of_int."))


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
		(* receive the result itself *)
(*		let buff = String.create len in
		let res = ref buff in*)
(* 		print_message Debug_medium ("[Master] Buffer created with length " ^ (string_of_int len)); *)
(*		print_message Debug_medium ("[Master] Dodoooooooooooooooooo");
		Unix.sleep 1;*)
		
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
(*		print_message Debug_medium ("[Master] Reception done (oui oui)");
		let l = String.length res in
		print_message Debug_medium ("[Master] Calcul taille");
		print_int l;
		print_message Debug_medium ("[Master] Taille affichee");
		
		print_char res.[0];
		print_message Debug_medium ("[Master] Coucou j'ai ecrit le premier caractere !");*)

		(* Print some information *)
		if debug_mode_greater Debug_medium then
			print_message Debug_medium ("[Master] Result was '" ^ (*!*)res ^ "'");
			
		(* Get the constraint *)
		let im_result = (*unserialize_im_result*) (*!*)res in
		PullAndResult (source_rank , im_result)
		
	(* Case error *)
	| Slave_outofbound_tag ->
		print_message Debug_medium ("[Master] Received Slave_outofbound_tag");
		OutOfBound source_rank
	
	(* Case simple pull? *)
	| Slave_work_tag ->
		print_message Debug_medium ("[Master] Received Slave_work_tag");
		PullOnly source_rank


let send_finished source_rank = 
	Mpi.send (weird_stuff()) source_rank (int_of_master_tag Master_finished_tag) Mpi.comm_world 

