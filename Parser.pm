
##################################################################
# Copyright (C) 2000 Greg London   All Rights Reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.
##################################################################


##################################################################
# this module defines a grammar for the VHDL language
# that can be used by Parse::RecDescent
##################################################################


##################################################################
# NOTE: THIS MODULE IS CURRENTLY IN THE DEBUGGING STAGE
# ALL CODE IS SUBJECT TO CHANGE WITHOUT NOTICE.
# I'm still getting the wrinkles out of the grammar.
# If you have any fixes, patches, or questions,
# please send them to me at 
#   greg42@bellatlantic.net
##################################################################
# Ideally, when it is all ironed out, 
# you'll be able to use this module to create fairly intelligent
# scripts to run on your VHDL code.
##################################################################

##################################################################
package Hardware::Vhdl::Parser;
use Parse::RecDescent;
@ISA = ( 'Parse::RecDescent' );
##################################################################
use vars qw ( $VERSION );
$VERSION = '0.06';
##################################################################

$::RD_AUTOSTUB = undef;

$::RD_ERRORS = 1;
#$::RD_WARN = 1;
#$::RD_HINT = 1;
$::RD_TRACE = undef;


use Data::Dumper;

##################################################################
sub new
##################################################################
{
 my ($pkg) = @_;

 # get the vhdl grammar defined in this file
 my $vhdl_grammar = $pkg->grammar();

 # create a parser object, use SUPER:: to find the method via @ISA
 my $r_hash = $pkg->SUPER::new  ($vhdl_grammar);

 # bless it as a vhdl_parser object
 bless $r_hash, $pkg;
 return $r_hash;
} 



##################################################################
sub grammar
##################################################################
{





























# note, q{  statement should be on line 100, 
# to make it easier to find referenced line numbers

return  q{

#START_OF_GRAMMAR

	eofile : /^\Z/

	####################################################
	####################################################

	design_file :  
		design_unit(s) eofile { $return = $item[1] }

	design_unit : 
		context_clause(s?) library_unit  
		| <error> 

	context_clause :
		  library_clause 
		| use_clause 

	library_unit : 
		  entity_declaration 
		| architecture_body 
		| package_declaration 
		| package_body 
		| configuration_declaration 

	library_clause : 
		'library' library_name_list ';'  

	library_name_list : 
		one_or_more_identifiers_separated_by_commas

	use_clause : 
		'use'  selected_name_list  ';' 

	selected_name_list :
		one_or_more_selected_names_separated_by_commas

	####################################################
	####################################################

	entity_declaration : 
		'entity' entity_name 'is' 
		optional_generic_declaration_section(?)
		optional_port_declaration_section(?)
		entity_declaritive_item(?)
		begin_entity_section(?)
		'end' ( 'entity' )(?) identifier(?) ';'  
		 

	begin_entity_section :
		'begin'
		( concurrent_assertion_statement |
		passive_concurrent_procedure_call_statement |
		passive_process_statement ) 

	passive_concurrent_procedure_call_statement :
		concurrent_procedure_call_statement

	passive_process_statement :
		process_statement

	entity_declaritive_item : 
		  subprogram_declaration 
		| subprogram_body 
		| type_declaration 
		| subtype_declaration 
		| constant_declaration 
		| signal_declaration 
		| shared_variable_declaration 
		| file_declaration 
		| alias_declaration 
		| attribute_declaration 
		| attribute_specification 
		| disconnection_specification 
		| use_clause 
		| group_template_declaration 
		| group_declaration 

	architecture_body :
		'architecture' identifier 'of' entity_name 'is'
			block_declarative_item(s?)
		'begin'
			concurrent_statement(s?)
		'end' ( 'architecture' )(?) identifier(?) ';' 
| <error>

	configuration_declaration :
		'configuration' identifier 'of' entity_name 'is'
			(   use_clause  
			  | attribute_specification
			  | group_declaration )
			block_configuration
		'end' ( 'configuration' )(?) identifier(?) ';' 

	block_configuration : 
		'for' architecture_name
		use_clause(?)
		'end' 'for' ';'

	package_declaration :
		'package' identifier 'is'
			package_declarative_item(s?)
		'end' ( 'package' )(?) identifier(?) ';'  
		
	package_declarative_item :
		  subprogram_declaration 
		| type_declaration 
		| subtype_declaration 
		| constant_declaration 
		| signal_declaration 
		| shared_variable_declaration 
		| file_declaration 
		| alias_declaration 
		| component_declaration 
		| attribute_declaration 
		| attribute_specification 
		| disconnection_specification 
		| use_clause 
		| group_template_declaration 
		| group_declaration 

	package_body :
		'package' 'body' identifier 'is'
			package_body_declarative_item(s?)
		'end' ( 'package' 'body' )(?) identifier(?) ';' 

	package_body_declarative_item :
		  subprogram_body 
		| subprogram_declaration 
		| type_declaration 
		| subtype_declaration 
		| constant_declaration 
		| shared_variable_declaration 
		| file_declaration 
		| alias_declaration 
		| use_clause 
		| group_template_declaration 
		| group_declaration 

	####################################################
	####################################################

	subprogram_declaration : subprogram_specification ';' 

	subprogram_specification :
		( procedure_specification | function_specification ) 

	procedure_specification :
		'procedure' ( identifier | operator_symbol )
		optional_parameter_section(?) 

	function_specification :
		pure_or_impure(?) 
		'function' 
		( identifier | operator_symbol ) 
		optional_parameter_section(?) 
		'return'
		type_mark 

	pure_or_impure :
		( 'pure' | 'impure' )

	subprogram_body :
		subprogram_specification 'is'
			subprogram_declarative_part(?)
		'begin'
			sequential_statement(s?)
		'end' ( 'function' | 'procedure' )(?)
		( identifier | operator_symbol )(?) ';' 

	subprogram_declarative_part :
		  subprogram_declaration 
		| subprogram_body 
		| type_declaration 
		| subtype_declaration 
		| file_declaration 
		| alias_declaration 
		| attribute_declaration 
		| attribute_specification 
		| use_clause 
		| group_template_declaration 
		| group_declaration  

	type_declaration :
		'type' identifier ( 'is' type_definition )(?) ';' 

	type_definition : 
		  enumeration_type_definition
		| integer_type_definition 
		| floating_type_definition 
		| physical_type_definition 
		| array_type_definition 
		| record_type_definition
		| access_type_definition 
		| file_type_definition 

	constant_declaration :
		'constant' one_or_more_identifiers_separated_by_commas ':'
		subtype_indication 
		default_value(?) ';' 

	signal_declaration :
		'signal' one_or_more_identifiers_separated_by_commas ':'
		subtype_indication  ( 'register' | 'bus' )(?)
		default_value(?) ';' 

	shared_variable_declaration : 'shared' variable_declaration
	variable_declaration :
		'variable' one_or_more_identifiers_separated_by_commas ':'
		subtype_indication
		default_value(?) ';' 

	file_declaration :
		'file' one_or_more_identifiers_separated_by_commas ':'
		subtype_indication
		open_file_expression_is_string_expression_option(?) ';' 
		

	open_file_expression_is_string_expression_option :
		open_file_expression_option(?) 'is' string_expression 

	open_file_expression_option :
		'open' file_open_kind_expression 

	alias_declaration :
		'alias' ( identifier | character_literal | operator_symbol ) 
		( ':' subtype_indication )(?)
		'is' name signature(?) ';' 

	component_declaration :
		'component' component_name ('is')(?)
		optional_generic_declaration_section(?)
		optional_port_declaration_section(?)
		'end' 'component' component_name(?) ';'  

	attribute_declaration :
		'attribute' identifier ':' type_mark ';'

	attribute_specification :
		'attribute' identifier 'of' entity_name_list ':' 
		entity_class 'is' expression_rule ';'

	entity_name_list :
		( list_of_id_or_char_or_op_with_optional_signature |
		'others' |
		'all' )

	list_of_id_or_char_or_op_with_optional_signature :
		<leftop: id_or_char_or_op_with_optional_signature /(,)/
		id_or_char_or_op_with_optional_signature>(?)

	id_or_char_or_op_with_optional_signature :
		( identifier | character_literal | operator_symbol )
		signature(?)

	entity_class :
 		(
		  'entity'	| 'architecture' | 'configuration' | 
		  'procedure'   | 'function'	 | 'package' 	   | 
		  'type'	| 'subtype'	 | 'constant'	   | 
		  'signal'	| 'variable' 	 | 'component'	   | 
		  'label' 	| 'literal'	 | 'units'	   |
		  'group'	| 'file'	
		)

	configuration_specification :
		'for' component_specification binding_indication ';'

	component_specification :
		( one_or_more_instantiation_labels_separated_by_commas |
		 'others' | 'all' ) ':' component_name



	binding_indication : 
		optional_use_entity_or_configuration_or_open(?)
		optional_generic_map_section(?)
		optional_port_map_section(?)
	';'

	optional_use_entity_or_configuration_or_open :
		'use' 
		(
			'entity' entity_name architecture_identifier(?) |
			'configuration' configuration_name |
			'open'
		)

	disconnection_specification :
		'disconnect' 
		(
			one_or_more_signals_separated_by_commas |
			'others' |
			'all'
		)
		':' type_mark
		'after' time_expression ';'

	group_template_declaration :
		'group' identifier 'is' 
		'(' list_of_entity_class_with_option ')' ';'

	list_of_entity_class_with_option :
		<leftop: entity_class_with_option /(,)/
		 entity_class_with_option>(?)

	entity_class_with_option :
		entity_class

	group_declaration :
		'group' identifier ':' group_template_name 
		'(' one_or_more_name_char_items_with_comma ')' ';'

	one_or_more_name_char_items_with_comma :
		<leftop: name_or_char_literal /(,)/ name_or_char_literal>(?)

	name_or_char_literal :
		( name | character_literal )

	use_clause : 'use' one_or_more_selected_names_separated_by_commas ';'

	one_or_more_selected_names_separated_by_commas :
		<leftop: selected_name /(,)/ selected_name>(?)

	####################################################
	####################################################
	enumeration_type_definition :
		'(' one_or_more_id_or_char_separated_by_commas ')'

	one_or_more_id_or_char_separated_by_commas :
		<leftop: identifier_or_character_literal /(,)/
		 identifier_or_character_literal>(?)

	identifier_or_character_literal :
		identifier | character_literal

	simple_expression_to_downto_simple_expression :
		simple_expression ( 'to' | 'downto' ) simple_expression 

	integer_type_definition :
		'range' 
		( range_attribute_name |
		 simple_expression_to_downto_simple_expression )
 
	floating_type_definition :
		'range' 
		( range_attribute_name |
		 simple_expression_to_downto_simple_expression )
 
	physical_type_definition :
		'range' 
		( range_attribute_name |
		 simple_expression_to_downto_simple_expression )
		'units' identifier ';'
		identifier_is_physical_literal(?)
		'end' 'units' identifier(?)

	identifier_is_physical_literal :
		identifier '=' physical_literal

	array_type_definition :
		'array' '('
		( array_type_mark_definition | array_discrete_range_definition )
		')' 'of'
		element_subtype_indication

	array_type_mark_definition :
		one_or_more_type_mark_ranges_separated_by_commas 

	one_or_more_type_mark_ranges_separated_by_commas :
		<leftop: type_mark_range_box /(,)/ type_mark_range_box>(?)

	type_mark_range_box :
		type_mark 'range' '<>'

	array_discrete_range_definition :
		one_or_more_discrete_ranges_separated_by_commas

	record_type_definition :
		'record' 
			record_element_definition(s) 
		'end' 'record' identifier(?)

	record_element_definition :
		one_or_more_identifiers_separated_by_commas ':'
		subtype_indication ';'

	access_type_definition :
		'access' subtype_indication

	file_type_definition :
		'file' 'of' type_mark

	subtype_declaration :
		'subtype' identifier 'is' subtype_indication ';'

	subtype_indication :
		# resolution_function_name 
		type_mark
		optional_range_or_simple_or_discrete(?)  

	optional_range_or_simple_or_discrete :
		  range_range_attribute_name_or_simple_downto_expression
		| '(' discrete_range ')'

	range_range_attribute_name_or_simple_downto_expression :
		'range' 
			( range_attribute_name |
			  simple_expression_to_downto_simple_expression ) 		
		
	discrete_range :
		  simple_expression_to_downto_simple_expression 
		| range_attribute_name 
		| discrete_subtype_indication

	discrete_subtype_indication : 
		subtype_indication

	type_mark :
		type_name

	####################################################
	####################################################
	concurrent_statement :
		  component_instantiation_statement 
		| block_statement 
		| process_statement 
		| concurrent_procedure_call_statement 
		| concurrent_assertion_statement 
		| concurrent_signal_assignment_statement 
		| generate_statement 

	block_statement :
		block_label ':'
		'block'
			optional_guard_expression(?)
			('is')(?)
			optional_generic_declaration_section(?)
			optional_generic_map_section(?)
			optional_port_declaration_section(?)
			optional_port_map_section(?)
			block_declarative_item(s?)
		'begin'
			concurrent_statement(s?)
		'end' 'block' block_label(?) ';'

	optional_guard_expression :
		'(' guard_expression ')'

	block_declarative_item :
		  subprogram_declaration 
		| subprogram_body	
		| type_declaration 
		| subtype_declaration 
		| constant_declaration 
		| signal_declaration 
		| shared_variable_declaration 
		| file_declaration 
		| alias_declaration 
		| component_declaration 
		| attribute_declaration 
		| attribute_specification 
		| configuration_specification 
		| disconnection_specification 
		| use_clause 
		| group_template_declaration 
		| group_declaration

	process_statement :
		( process_label ':' )(?)
		( 'postponed' )(?)
		'process'
		optional_sensitivity_list(?)
		('is')(?)
			process_declarative_item(s?)
		'begin'
			sequential_statement(s?)
		'end' ('postponed')(?) 'process' (process_label)(?) ';'

	optional_sensitivity_list :
		'(' one_or_more_signals_separated_by_commas ')'

	process_declarative_item :
		  subprogram_declaration 
		| subprogram_body 
		| type_declaration 
		| subtype_declaration 
		| constant_declaration 
		| variable_declaration 
		| file_declaration 
		| alias_declaration 
		| attribute_declaration 
		| attribute_specification 
		| use_clause 
		| group_template_declaration 
		| group_declaration 

	concurrent_procedure_call_statement :
		( label ':')(?)
		('postponed')(?)
		procedure_name
		optional_parameter_section(?)
		';'

	concurrent_assertion_statement :
		( label ':')(?)
		('postponed')(?)
		'assert' boolean_expression 
		('report' expression_rule)(?)
		('severity' expression_rule)(?) ';'

	concurrent_signal_assignment_statement :
		( label ':')(?) ('postponed')(?) 
		( selected_signal_assignment | conditional_signal_assignment )

	conditional_signal_assignment :
		( name | aggregate ) 
		'<=' 
		('guarded')(?) 
		delay_mechanism(?)
		waveform_rule 
		when_boolean_expression_else_waveform_rule(s?)
		';' 

	when_boolean_expression_else_waveform_rule :
		'when' 
		boolean_expression 
		'else'
		waveform_rule

	selected_signal_assignment :
		'with' expression_rule 'select'
		( name | aggregate ) 
		'<='
		('guarded')(?) 
		(delay_mechanism)(?)
		one_or_more_waveform_when_choices_comma_separated
		';'

	one_or_more_waveform_when_choices_comma_separated :
		<leftop: waveform_when_choices /(,)/ waveform_when_choices>(?)

	waveform_when_choices :
		waveform_rule 'when' choices_separated_by_pipe 

	component_instantiation_statement :
		instantiation_label ':'
		(
			component_instance_component_name |
			component_instance_entity_name    |
			component_instance_configuration_name 
		)
		optional_generic_map_section(?)
		optional_port_map_section(?)
	';'
	| <error>

	component_instance_component_name :
		('component')(?) component_name  

	component_instance_entity_name :
		'entity' entity_name optional_architecture_identifier(?)  

	optional_architecture_identifier :
		 '(' architecture_identifier ')'  

	component_instance_configuration_name :
		 'configuration' configuration_name 

	generate_statement :
		generate_label ':'
		( for_identifier_in_range | if_boolean_expression )
		'generate'
		generate_block_declarative_item_and_begin(?)
		concurrent_statement(s?)
		'end' 'generate' generate_label(?) ';'

	for_identifier_in_range :
		'for' identifier 'in' discrete_range

	if_boolean_expression :
		'if' boolean_expression

	generate_block_declarative_item_and_begin :
		block_declarative_item(s?)
		'begin'

	####################################################
	####################################################
	sequential_statement :
		  wait_statement 
		| assertion_statement
		| report_statement 
		| signal_assignment_statement 
		| variable_assignment_statement 
		| procedure_call_statement 
		| if_statement 
		| case_statement 
		| loop_statement 
		| next_statement 
		| exit_statement 
		| return_statement 
		| null_statement

	wait_statement :
		( label ':')(?)
		'wait'
		on_list_of_signal(?)
		( 'until' boolean_expression )(?)
		( 'for' time_expression )(?)
		';'

	on_list_of_signal :
 		'on' one_or_more_signals_separated_by_commas

	assertion_statement :
		( label ':' )(?)
		'assert' boolean_expression
		( 'report' expression_rule )(?)
		( 'severity' expression_rule )(?)
		';'

	report_statement :
		( label ':' )(?)
		'report' expression_rule 
		( 'severity' expression_rule )(?)
		';'

	signal_assignment_statement :
		( label ':' )(?)
		( name | aggregate ) 
		'<='
		delay_mechanism(?)
		waveform_rule
		';'

	delay_mechanism :
		( 'transport' | inertial_with_optional_reject_time )

	inertial_with_optional_reject_time :
		( 'reject' time_expression )(?) 'inertial'

	waveform_rule :
		  'unaffected' 
		| one_or_more_waveform_items_separated_by_commas

	one_or_more_waveform_items_separated_by_commas :
		<leftop: waveform_item /(,)/ waveform_item>(?) 

	waveform_item :
		  null_with_optional_after_time_expression 
		| value_expression_with_optional_time_expression

	null_with_optional_after_time_expression :
		'null' optional_after_time_expression(?) 

	value_expression_with_optional_time_expression :
		value_expression 
		optional_after_time_expression(?)

	optional_after_time_expression :
		'after' 
		time_expression 

	variable_assignment_statement :
		( label ':')(?)
		( name | aggregate ) ':=' expression_rule ';'

	procedure_call_statement :
		( label ':')(?)
		procedure_name optional_parameter_section(?) ';'

	if_statement :
		( if_label ':')(?)
		'if' boolean_expression 'then'
			sequential_statement(s)
		optional_elsif_section(s?)
		optional_else_section(?)
		'end' 'if' if_label(?) ';'
| <error>

	optional_elsif_section :
		'elsif' boolean_expression 'then'
			sequential_statement(s)

	optional_else_section :
		'else' 
			sequential_statement(s)

	case_statement :
		( case_label ':' )(?)
		'case' expression_rule 'is'
			when_choices_sequential_statement(s)
		'end' 'case' case_label(?) ';'

	when_choices_sequential_statement :
		'when' choices_separated_by_pipe '=>' sequential_statement(s)

	loop_statement :
		( loop_label ':')(?)
		( 'while' boolean_expression |
		  'for' identifier 'in' discrete_range )
		'loop'
			sequential_statement(s)
		'end' 'loop' loop_label(?) ';'

	next_statement :
		( label ':' )(?) 'next' loop_label(?) 
		( 'when' boolean_expression )(?) ';'

	exit_statement :
		( label ':' )(?) 'exit' loop_label(?) 
		( 'when' boolean_expression )(?) ';'

	return_statement :
		( label ':' )(?) 'return' expression_rule(?) ';'

	null_statement :
		( label ':' )(?) 'null' ';'


	####################################################
	# E.7 Interfaces and Associations
	####################################################
	interface_list :
		<leftop: interface_item /(;)/ interface_item>(?)

	interface_item :
		  constant_interface 
		| signal_interface 
		| variable_interface 
		| file_interface

	constant_interface :
		('constant')(?) one_or_more_identifiers_separated_by_commas ':' 
		('in')(?) subtype_indication default_value(?) ';'


	signal_interface :
		('signal')(?) one_or_more_identifiers_separated_by_commas ':' 
		mode(?) subtype_indication ('bus')(?) 
		default_value(?) ';'

	variable_interface :
		('variable')(?) one_or_more_identifiers_separated_by_commas ':' 
		mode(?) subtype_indication default_value(?) ';'

	file_interface :
		'file' one_or_more_identifiers_separated_by_commas ':'
		 subtype_indication ';'

	mode : 
		 'in' | 'out' | 'inout' | 'buffer' | 'linkage' 

	association_list :
		<leftop: actual_part_with_optional_formal_part /(,)/
		 actual_part_with_optional_formal_part>(?)

	actual_part_with_optional_formal_part :
		( formal_part '=>' ) actual_part

	formal_part :
		  generic_name 
		| port_name 
		| parameter_name 
		| function_name generic_port_parameter_selection 
		| type_mark generic_port_parameter_selection 

	generic_port_parameter_selection :
		'(' ( generic_name | port_name | parameter_name ) ')'

	actual_part :
		  expression_rule 
		| variable_name 
		| 'open' 
		| function_name_signal_name_or_variable_name_selection 
		| type_mark_signal_name_or_variable_name_selection


	function_name_signal_name_or_variable_name_selection :
		function_name
		signal_name_or_variable_name_selection

	type_mark_signal_name_or_variable_name_selection :
		type_mark
		signal_name_or_variable_name_selection

	signal_name_or_variable_name_selection :
		'(' ( signal_name | variable_name ) ')'


	####################################################
	# E.8 Expressions
	####################################################
	boolean_expression :
		expression_rule

	static_expression :
		expression_rule


	expression_rule : 
		relation logic_relation(s?)

	logic_relation : 
		( 'and' | 'nand' | 'or' | 'nor' | 'xor' | 'xnor' ) relation

	relation :
		shift_expression relation_shift_expression(?)

	relation_shift_expression :
		( '=' | '/=' | '<' | '<=' | '>' | '>=' ) shift_expression 

	shift_expression :
		simple_expression shift_simple_expression(?)

	shift_simple_expression :
		 ( 'sll' | 'srl' | 'sla' | 'sra' | 'rol' | 'ror' )
		 simple_expression 

	simple_expression :
		sign(?) term optional_term(s?) 

	sign :
		'+' | '-' 
	
	optional_term :
		add_or_concat_operator term 

	add_or_concat_operator :
		  '+' 
		| '-' 
		| '&'

	term :
		factor
		optional_factor(s?) 

	optional_factor :
		multiply_operator factor 

	multiply_operator :
		  '*' 
		| '/' 
		| 'mod' 
		| 'rem'

	factor :
		  'abs' primary 
		| 'not' primary 
		| primary_exp_primary 

	primary_exp_primary :
		primary exponent_primary(?) 

	exponent_primary :
		'**' primary 

	primary : 
		  name 
		| aggregate
		| primary_literal
		| function_call 
		| qualified_expression 
		| type_conversion 
		| allocator 
		| '(' expression_rule ')' 

	primary_literal : super_sub_literal

	super_sub_literal : literal

	type_conversion :
		type_mark '(' expression_rule ')'

	allocator :
		'new' subtype_indication |
		'new' qualified_expression 

	function_call : 
		function_name optional_parameter_section(?)

	qualified_expression :
		type_mark_tick_expression | type_mark_tick_aggregate

	type_mark_tick_expression :
		type_mark "'" '(' expression_rule ')'

	type_mark_tick_aggregate :
		type_mark "'" aggregate

	# left recursion here.
	# name = ( name | functionname) ... | (name | functioncall) 
	# need to clean up "name" rule declaration
	name :
		  attribute_name 
		| operator_symbol 
		| selected_name 
		| identifier '(' one_or_more_expressions_separated_by_commas ')'
		| identifier '(' discrete_range ')'
		| identifier

	selected_name :
		identifier  '.' 
		( identifier  '.' )(?)
		( identifier | character_literal | operator_symbol | 'all' )

	operator_symbol :
		'"' graphic_character '"'

	attribute_name :
		identifier "'" identifier 

	signature :
		'[' one_or_more_type_marks_separated_by_commas(?) ')' 
		( 'return' type_mark )(?)

	literal :
		  character_literal
		| string_literal 
		| bit_string_literal 
		| identifier
		| decimal_literal 
		| based_literal 
		| decimal_or_based_unit_name 
		| 'null'

	decimal_or_based_unit_name :
		( decimal_literal | based_literal ) unit_name 

	decimal_literal : 
		integer
		optional_fractional_part(?)
		optional_sci_notation(?)

	optional_fractional_part :
		  '.' integer

	optional_sci_notation :
		 'E' sign(?) integer

	based_literal :
		integer '#' based_integer optional_based_fraction_part(?) 
		'#' optional_based_sci_notation(?)

	optional_based_fraction_part :
		'.' based_integer 

	optional_based_sci_notation :
		'E' sign(?) integer

	integer :
		/\d+/

	based_integer :
		one_or_more_digit_letters_possibly_separated_by_underscores

	character_literal :
		"'" graphic_character "'" 

	string_literal :
		'"' graphic_character(s) '"'

	bit_string_literal :
		( 'B' | 'O' | 'X' ) '"' based_integer '"'

	aggregate :
		'(' one_or_more_choice_expressions_separated_by_commas ')' 

	one_or_more_choice_expressions_separated_by_commas : 
		<leftop: optional_choice_arrow_with_expression
		 /(,)/ optional_choice_arrow_with_expression>(?)

	optional_choice_arrow_with_expression :
		optional_choice_arrow(?)
		expression_rule 

	optional_choice_arrow :
		choices_separated_by_pipe '=>'

	choices_separated_by_pipe :
		<leftop: one_of_several_choices pipe one_of_several_choices>(?)
| <error>

	pipe : 
		'|'

	one_of_several_choices :
		'others' | simple_expression | discrete_range | identifier  
| <error>

	case_label : identifier
	loop_label : identifier
	if_label : identifier
	label : identifier

	identifier : /[A-Za-z][A-Za-z_0-9]*/ 

	graphic_character :
		/[A-Za-z0-9\-~`!@#$%^&*()_+={};:',.<>|]/
	####################################################
	# misc
	####################################################
	one_or_more_identifiers_separated_by_commas : 
		<leftop: identifier /(,)/ identifier>(?)

	entity_name : identifier 

	####
	# generics
	####
	optional_generic_declaration_section : 
		'generic' generic_interface_list(?) ';'


	generic_interface_list : 
		'('
	 one_or_more_generic_interface_list_entries_separated_by_semicolons 
		')' 

	one_or_more_generic_interface_list_entries_separated_by_semicolons : 
		<leftop: generic_interface_list_entry /(;)/
		 generic_interface_list_entry>(?) 

	generic_interface_list_entry : 
		one_or_more_identifiers_separated_by_commas ':'
		subtype_indication 
		default_value(?) 

	default_value : 
		':=' static_expression

	optional_generic_map_section : 
		'generic' 'map' optional_generic_association_list(?)   

	optional_generic_association_list :
		'(' generic_association_list ')' 

	generic_association_list : 
		association_list 


	####
	# ports
	####
	optional_port_declaration_section : 
		'port' port_interface_list(?) ';'

	port_interface_list : 
		'('
	 one_or_more_port_interface_list_entries_separated_by_semicolons
		 ')' 

	one_or_more_port_interface_list_entries_separated_by_semicolons : 
		<leftop: port_interface_list_entry /(;)/
		 port_interface_list_entry>(?)  

	port_interface_list_entry : 
		one_or_more_ports_separated_by_commas ':'
		mode
		subtype_indication 
		default_value(?)  

	one_or_more_ports_separated_by_commas :
		<leftop: port_name /(,)/ port_name>(?)



	optional_port_map_section : 
		'port' 'map' optional_port_association_list(?)   

	optional_port_association_list :
		'(' port_association_list ')' 

	port_association_list : 
		association_list 

	####
	# parameters to procedure/function call
	####
	optional_parameter_section : 
		'(' parameter_association_list ')' 

	parameter_association_list :
		association_list 

	####
	# instance labels
	####
	one_or_more_instantiation_labels_separated_by_commas :
		<leftop: instantiation_label /(,)/ instantiation_label>(?)

	instantiation_label : identifier 

	####
	# signals
	####
	one_or_more_signals_separated_by_commas :
		<leftop: signal_name /(,)/ signal_name>(?)

	signal_name : identifier

	####
	# discrete range
	####
	one_or_more_discrete_ranges_separated_by_commas :
		<leftop: discrete_range /(,)/ discrete_range>(?)

	####
	# expressions
	####
	one_or_more_expressions_separated_by_commas :
		<leftop: expression_rule /(,)/ expression_rule>(?)

	####
	# type_marks
	####
	one_or_more_type_marks_separated_by_commas :
		<leftop: type_mark /(,)/ type_mark>(?)


	####
	# digits
	####
	one_or_more_digits_possibly_separated_by_underscores :
		<leftop: digit /(_)/ digit>(?)

	digit :
		/[0-9]/

	####
	# digit_letter
	####
	one_or_more_digit_letters_possibly_separated_by_underscores :
		<leftop: digit_letter /(_)/ digit_letter>(?)

	digit_letter :
		/[A-Za-z0-9]/



	########################################################
	# oddball rules, need to confirm correctness
	########################################################
	procedure_name : identifier
	function_name : identifier 
	component_name : identifier 
	architecture_name : identifier
	configuration_name : identifier 
	architecture_identifier : identifier
	variable_name : identifier
	generic_name : identifier
	port_name : identifier
	parameter_name : identifier
	process_label : identifier
	group_template_name : identifier
	block_label : identifier
	type_name : identifier
	subtype_name : identifier
	generate_label : identifier
	resolution_function_name : identifier 
	unit_name : identifier
	physical_literal : identifier

	range_attribute_name : attribute_name

	guard_expression : expression_rule
	value_expression : expression_rule 
	string_expression : expression_rule
	time_expression : expression_rule time_units
	file_open_kind_expression : expression_rule

	static_expression : identifier

	element_subtype_indication : subtype_indication

	time_units : 
		'fs' | 'ps' | 'ns' | 'us' | 'ms' | 'sec' | 'min' | 'hr'

#END_OF_GRAMMAR

	};   # end of return statement


} #end of sub grammar



1;  # end of module


