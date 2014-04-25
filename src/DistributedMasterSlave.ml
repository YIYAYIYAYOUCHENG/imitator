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
 * Last modified: 2014/04/22
 *
 ****************************************************************)
 
open Global
open Mpi
(* open Printf (* a terme : retirer tout ca *) *)
(* open Unix (* temporaire : necessaire pour sleep *) *)
open DistributedUtilities
	

(* Declaration of the tags we are going to use in 
   communications within the M/W pattern *)

(* tmp *)
let cnt = ref 0

(* ** *** *** ***** *******       UTILS       ******* ***** *** *** ** *)




(* Initialize a slave                                                  *)

let init_slave rank size =
	print_message Debug_standard ("[Worker " ^ (string_of_int rank) ^ "] I am worker [" ^ (string_of_int rank) ^ "] in " ^ (string_of_int (size-1)) ^ ".");
;;


(* ** *** *** ***** *******       MASTER      ******* ***** *** *** ** *)

let receive_pull_request_and_store_constraint () =
	print_message Debug_medium ("[Master] Entered function 'receive_pull_request_and_store_constraint'...");
	match receive_pull_request () with
	| PullOnly source_rank ->
		print_message Debug_medium ("[Master] Received PullOnly request...");
		raise (InternalError("OutOfBound not implemented."))
	
	| OutOfBound source_rank ->
		print_message Debug_medium ("[Master] Received OutOfBound request...");
		(* FAIRE QUELQUE CHOSE POUR DIRE QU'UN POINT N'A PAS MARCHÉ *)
		raise (InternalError("OutOfBound not implemented."))(*;
		source_rank, None*)

	| PullAndResult (source_rank , im_result) -> 
		print_message Debug_medium ("[Master] Received PullAndResult request...");
		(* Process the result by IM *)
(* 		Cartography.bc_process_im_result im_result; *)
		(* Return source rank *)
		source_rank, None


let master () = 

    print_message Debug_standard ("[Master] Hello world!");
    
	for i = 1 to 100 do
		print_message Debug_standard ("[Master] Waiting for a pull request");

		(* Get the pull_request *)
		let source_rank, tile_nature_option = receive_pull_request_and_store_constraint () in
	
		print_message Debug_standard ("[Master] Got a " ^ (string_of_int i) ^ "th pull request from slave " ^ (string_of_int source_rank) ^ "");
			
	done; (* while more pi0 *)


    print_message Debug_standard ("[Master] Done!" );

    (* I am done sending all my data. Receive the results of the last
       computations, and wrap up. *)
    
(* 	let size = Mpi.comm_size Mpi.comm_world in *)
	
	print_message Debug_standard ("[Master] All slaves done" );
    
	()
	
;;
  
(* *** *** ***** *******      WORKER      ******* ***** *** *** *)

let worker () = 
    
	let rank = rank() in
	let size = size() in

    print_message Debug_standard ("[Worker " ^ (string_of_int rank) ^ "] Hello!");
	init_slave rank size;

(*     let n = rank in *)
    let finished = ref false in

    (* Start: ask for some work *)
(*     send_work_request(); *)

    print_message Debug_standard ("[Worker " ^ (string_of_int rank) ^ "] sent pull request to the master.");

    for i = 1 to 30 do

		(* Send the result *)
		send_string "1*0g8a1*1g3a-1*0>-17a1*0+1*1g24a1*0g8a-1*0>-17a-1*0>-17a1*0+1*1g17a1*0g8a1*1g3a-1*0>-17a1*0g8a1*1g3a-1*0>-17a1*1g3a1*0+1*1g17a1*0g8a1*0+1*1g24a1*0g8a-1*0>-17a1*1g3a-1*0>-17a1*0g8a1*0+1*1g17a-1*0>-17a1*1g3a1*0g8a-1*0>-17a1*1g3a1*0g8a-1*0>-17a1*1g3a1*0g8a1*1g3a1*0g8a1*1g3a1*0g8a1*1g3a1*0g8,B|B|false|13|12|9|0.0161981582642";
		print_message Debug_standard ("[Worker " ^ (string_of_int rank) ^ "] Sent a constraint #" ^ (string_of_int i));
    done;
	print_message Debug_standard ("[Worker " ^ (string_of_int rank) ^ "] I'm done.");
;;
