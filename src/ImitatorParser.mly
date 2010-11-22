/***********************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created       : 2009/09/07
 * Last modified : 2010/05/07
***********************************************/

%{
open ParsingStructure;;
open Global;;
  
  
let parse_error s =
	let symbol_start = symbol_start () in
	let symbol_end = symbol_end () in
	raise (ParsingError (symbol_start, symbol_end))
;;
 
%}

%token <int> INT
%token <string> NAME
%token <string> STRING

%token OP_PLUS OP_MINUS OP_MUL OP_DIV
%token OP_L OP_LEQ OP_EQ OP_GEQ OP_G OP_ASSIGN

%token LPAREN RPAREN LBRACE RBRACE LSQBRA RSQBRA
%token AMPERSAND APOSTROPHE COLON COMMA PIPE SEMICOLON

/* CT_ALL CT_ANALOG CT_ASAP CT_BACKWARD CT_CLDIFF CT_D  CT_ELSE CT_EMPTY  CT_ENDHIDE CT_ENDIF CT_ENDREACH CT_ENDWHILE CT_FORWARD CT_FREE CT_FROM  CT_HIDE CT_HULL CT_INTEGRATOR CT_ITERATE CT_NON_PARAMETERS CT_OMIT CT_POST CT_PRE CT_PRINT CT_PRINTS CT_PRINTSIZE CT_REACH  CT_STOPWATCH CT_THEN CT_TO CT_TRACE CT_USING  CT_WEAKDIFF CT_WEAKEQ CT_WEAKGE CT_WEAKLE  */

%token CT_AND CT_AUTOMATON CT_ANALOG CT_BAD CT_CLOCK CT_DISCRETE CT_DO CT_END CT_ENDREACH CT_FALSE CT_FORWARD CT_FROM CT_GOTO CT_IF CT_INIT CT_INITIALLY CT_IN CT_LOC CT_LOCATIONS CT_NOT CT_OR CT_PARAMETER CT_PRINT CT_REACH CT_REGION CT_SYNC CT_SYNCLABS CT_TRUE CT_VAR CT_WAIT CT_WHEN CT_WHILE

%token EOF

/*%left OP_L OP_LEQ OP_EQ OP_GEQ OP_G*/
%left PIPE CT_OR        /* lowest precedence */
%left AMPERSAND CT_AND  /* medium precedence */
%nonassoc CT_NOT        /* highest precedence * /


/* %left OP_PLUS OP_MINUS        /* lowest precedence */
/*
%left OP_DIV         /* medium precedence * /
%nonassoc UMINUS        /* highest precedence * /
*/

%start main             /* the entry point */
%type <ParsingStructure.parsing_structure> main
%%

/**********************************************/
main:
	 automata_descriptions commands EOF
	{
		let decl, automata = $1 in
		let init, bad = $2 in
		decl, automata, init, bad
	}
;

/***********************************************
  MAIN DEFINITIONS
***********************************************/

automata_descriptions:
	declarations automata { $1, $2 }
;

/**********************************************/

declarations:
	CT_VAR var_lists { $2 }
;


/**********************************************/

/**********************************************/

var_lists:
	var_list COLON var_type SEMICOLON var_lists { (($3, $1) :: $5) }
	| { [] }
;

/**********************************************/

var_list:
	  NAME { [$1] }
	| NAME COMMA var_list { $1 :: $3 }
;

/**********************************************/

var_type:
  | CT_ANALOG { Var_type_analog }
	| CT_CLOCK { Var_type_clock }
	| CT_DISCRETE { Var_type_discrete }
	| CT_PARAMETER { Var_type_parameter }
;

/**********************************************/

/**********************************************/

automata:
	automaton automata { $1 :: $2 }
	| { [] }
;

/**********************************************/

automaton:
	CT_AUTOMATON NAME prolog locations CT_END
	{
		($2, $3, $4)
	}
;

/**********************************************/

prolog:
	| initialization sync_labels { $2 }
	| sync_labels initialization { $1 }
	| sync_labels { $1 } /* L'initialisation n'est pas prise en compte, et est donc facultative */
;

/**********************************************/

initialization:
	| CT_INITIALLY NAME state_initialization SEMICOLON {}
;

/**********************************************/

state_initialization:
	AMPERSAND convex_predicate {}
	| {}
;

/**********************************************/

sync_labels:
	CT_SYNCLABS COLON sync_var_list SEMICOLON { $3 }
;

/**********************************************/

sync_var_list:
	sync_var_nonempty_list { $1 }
	| { [] }
;

/**********************************************/

sync_var_nonempty_list:
	NAME COMMA sync_var_nonempty_list { $1 :: $3}
	| NAME { [$1] }
;

/**********************************************/

locations:
	location locations { $1 :: $2}
	| { [] }
;

/**********************************************/

location:
  CT_LOC NAME COLON CT_WHILE convex_predicate CT_WAIT LBRACE rate_info_list RBRACE transitions { $2, $5, $8, $10 }
;

/**********************************************/
rate_info_list:
	rate_info_nonempty_list { $1 }
	| { [] }
;
 
/**********************************************/

rate_info_nonempty_list:
     rate_info COMMA rate_info_nonempty_list { $1 :: $3 } 
	|  rate_info { [$1] }
;

/**********************************************/

rate_info:
	NAME APOSTROPHE OP_EQ rational { $1, $4 }
;

/**********************************************/

transitions:
	transition transitions { $1 :: $2 }
	| { [] }
;

/**********************************************/

transition:
	CT_WHEN convex_predicate update_synchronization CT_GOTO NAME SEMICOLON
	{
		let update_list, sync = $3 in
			$2, update_list, sync, $5
	}
;

/**********************************************/

/* A l'origine de 3 conflits ("2 shift/reduce conflicts, 1 reduce/reduce conflict.") donc petit changement */
update_synchronization:
	| { [], NoSync }
	| updates { $1, NoSync }
	| syn_label { [], (Sync $1) }
	| updates syn_label { $1, (Sync $2) }
	| syn_label updates { $2, (Sync $1) }
;

/**********************************************/

updates:
	CT_DO LBRACE update_list RBRACE { $3 }
;

/**********************************************/

update_list:
	update_nonempty_list { $1 }
	| { [] }
;

/**********************************************/

update_nonempty_list:
	update COMMA update_nonempty_list { $1 :: $3}
	| update { [$1] }
;

/**********************************************/

update:
	NAME APOSTROPHE OP_EQ linear_expression { ($1, $4) }
;

/**********************************************/

syn_label:
	CT_SYNC NAME { $2 }
;


/***********************************************
  RATIONALS, LINEAR TERMS, LINEAR CONSTRAINTS AND CONVEX PREDICATES
***********************************************/

convex_predicate:
	linear_constraint AMPERSAND convex_predicate { $1 :: $3 }
	| linear_constraint { [$1] }
;

linear_constraint:
	linear_expression relop linear_expression { Linear_constraint ($1, $2, $3) }
	| CT_TRUE { True_constraint }
	| CT_FALSE { False_constraint }
;

relop:
	  OP_L { OP_L }
	| OP_LEQ { OP_LEQ }
	| OP_EQ { OP_EQ }
	| OP_GEQ { OP_GEQ }
	| OP_G { OP_G }
;

linear_expression:
	linear_term { Linear_term $1 }
	| linear_expression OP_PLUS linear_term { Linear_plus_expression ($1, $3) }
	| linear_expression OP_MINUS linear_term { Linear_minus_expression ($1, $3) } /* linear_term a la deuxieme place */
;

linear_term:
	rational { Constant $1 }
	| rational NAME { Variable ($1, $2) }
	| rational OP_MUL NAME { Variable ($1, $3) }
	| NAME { Variable (NumConst.one, $1) }
// 	| LPAREN linear_expression RPAREN { $2 }
	| LPAREN linear_term RPAREN { $2 }
;

rational:
	integer { $1 }
	| integer OP_DIV pos_integer { (NumConst.div $1 $3) }
;

integer:
	pos_integer { $1 }
	| OP_MINUS pos_integer { NumConst.neg $2 }
;

pos_integer:
	INT { NumConst.numconst_of_int $1 }
;


/***********************************************
  ANALYSIS COMMANDS
***********************************************/

commands:
	| init_declaration init_definition bad_declaration bad_definition reach_command { ($2, $4) }
;


init_declaration:
	| CT_VAR CT_INIT COLON CT_REGION SEMICOLON { }
	| { }
;

reach_command:
	/* print (reach forward from init endreach); */
	| CT_PRINT LPAREN CT_REACH CT_FORWARD CT_FROM CT_INIT CT_ENDREACH RPAREN SEMICOLON { }
	| { }
;


init_definition:
	CT_INIT OP_ASSIGN region_expression SEMICOLON { $3 }
;

bad_declaration:
	| CT_VAR CT_BAD COLON CT_REGION SEMICOLON { }
	| { }
;

bad_definition:
	| CT_BAD OP_ASSIGN loc_expression SEMICOLON { $3 }
	| { [] } 
;

loc_expression:
	| loc_predicate { [ $1 ] }
	| loc_expression AMPERSAND loc_expression { $1 @ $3 }
;

region_expression:
	| state_predicate { [ $1 ] }
	| LPAREN region_expression RPAREN { $2 }
	| region_expression AMPERSAND region_expression { $1 @ $3 }
;

loc_predicate:
	CT_LOC LSQBRA NAME RSQBRA OP_EQ NAME { Loc_assignment ($3, $6) }
;

state_predicate:
	| loc_predicate { $1 } 
	| linear_constraint { Linear_predicate $1 }
;

