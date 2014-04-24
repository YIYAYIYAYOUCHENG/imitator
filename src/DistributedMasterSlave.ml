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


(* Store the result of a computation.                                  *)

(*let store result source =
	print_string "MASTER - recv result" ; print_string result;
	print_string " from ";
	print_int source ;
	print_newline();
	0
;;*)

(* Returns the next set of input data                                  *)
(* This is where the master's intelligence will come.                  *)
(*                                                                     *)
(* Returns a couple: the first element is the size of the data, the    *)
(* second one is the data itself. The data is serialized and ready to  *)
(* be sent.                                                            *)

(*let get_data () =
	let size = 1 + ( Random.int 25 ) in
	let buff = String.create size in
	let alphanum = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" in
	let len = String.length alphanum in
	for i = 0 to pred size do
		buff.[i] <- alphanum.[Random.int len]
	done;
	cnt := !cnt + 1;
	(size, buff)
;;*)


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
    
	for i = 1 to 1000 do
		print_message Debug_standard ("[Master] Waiting for a pull request");

		(* Get the pull_request *)
		let source_rank, tile_nature_option = receive_pull_request_and_store_constraint () in
	
		print_message Debug_standard ("[Master] Got a pull request from slave " ^ (string_of_int source_rank) ^ "");
			
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
    send_work_request();

    print_message Debug_standard ("[Worker " ^ (string_of_int rank) ^ "] sent pull request to the master.");

    while true do

		(* Send the result *)
		send_string "dfgldfkglfdkglfdkgldkflgkdflgkfdlkgdflkgldfkgldfkgldfklgkdflgkldfklgkl";
		print_message Debug_standard ("[Worker " ^ (string_of_int rank) ^ "] Sent a constraint.");
    done;
	print_message Debug_standard ("[Worker " ^ (string_of_int rank) ^ "] I'm done.");
;;
