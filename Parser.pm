##################################################################
# Copyright (C) 2000 Greg London   All Rights Reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.
##################################################################


##################################################################
# this module defines a grammar for the VHDL language
# that can be used by Parse::RecDescent
##################################################################

package Hardware::Vhdl::Parser;

use Parse::RecDescent;

use strict;
use vars qw($VERSION @ISA);


@ISA = ( 'Parse::RecDescent' );

$VERSION = '0.07';
#########################################################################


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

####################################################
# define reserved words. case insensitive
####################################################
reserved_word_abs : 
	/abs/i

reserved_word_access : 
	/access/i

reserved_word_after : 
	/after/i

reserved_word_alias : 
	/alias/i

reserved_word_all : 
	/all/i

reserved_word_and : 
	/and/i

reserved_word_architecture : 
	/architecture/i

reserved_word_array : 
	/array/i

reserved_word_assert : 
	/assert/i

reserved_word_attribute : 
	/attribute/i

reserved_word_begin : 
	/begin/i

reserved_word_block : 
	/block/i

reserved_word_body : 
	/body/i

reserved_word_buffer : 
	/buffer/i

reserved_word_bus : 
	/bus/i

reserved_word_case : 
	/case/i

reserved_word_component : 
	/component/i

reserved_word_configuration : 
	/configuration/i

reserved_word_constant : 
	/constant/i

reserved_word_disconnect : 
	/disconnect/i

reserved_word_downto : 
	/downto/i

reserved_word_else : 
	/else/i

reserved_word_elsif : 
	/elsif/i

reserved_word_end : 
	/end/i

reserved_word_entity : 
	/entity/i

reserved_word_exit : 
	/exit/i

reserved_word_file : 
	/file/i

reserved_word_for : 
	/for/i

reserved_word_function : 
	/function/i

reserved_word_generate : 
	/generate/i

reserved_word_generic : 
	/generic/i

reserved_word_group : 
	/group/i

reserved_word_guarded : 
	/guarded/i

reserved_word_if : 
	/if/i

reserved_word_impure : 
	/impure/i

reserved_word_in : 
	/in/i

reserved_word_inertial : 
	/inertial/i

reserved_word_inout : 
	/inout/i

reserved_word_is : 
	/is/i

reserved_word_label : 
	/label/i

reserved_word_library : 
	/library/i

reserved_word_linkage : 
	/linkage/i

reserved_word_literal : 
	/literal/i

reserved_word_loop : 
	/loop/i

reserved_word_map : 
	/map/i

reserved_word_mod : 
	/mod/i

reserved_word_nand : 
	/nand/i

reserved_word_new : 
	/new/i

reserved_word_next : 
	/next/i

reserved_word_nor : 
	/nor/i

reserved_word_not : 
	/not/i

reserved_word_null : 
	/null/i

reserved_word_of : 
	/of/i

reserved_word_on : 
	/on/i

reserved_word_open : 
	/open/i

reserved_word_or : 
	/or/i

reserved_word_others : 
	/others/i

reserved_word_out : 
	/out/i

reserved_word_package : 
	/package/i

reserved_word_port : 
	/port/i

reserved_word_postponed : 
	/postponed/i

reserved_word_procedure : 
	/procedure/i

reserved_word_process : 
	/process/i

reserved_word_pure : 
	/pure/i

reserved_word_range : 
	/range/i

reserved_word_record : 
	/record/i

reserved_word_register : 
	/register/i

reserved_word_reject : 
	/reject/i

reserved_word_rem : 
	/rem/i

reserved_word_report : 
	/report/i

reserved_word_return : 
	/return/i

reserved_word_rol : 
	/rol/i

reserved_word_ror : 
	/ror/i

reserved_word_select : 
	/select/i

reserved_word_severity : 
	/severity/i

reserved_word_signal : 
	/signal/i

reserved_word_shared : 
	/shared/i

reserved_word_sla : 
	/sla/i

reserved_word_sll : 
	/sll/i

reserved_word_sra : 
	/sra/i

reserved_word_srl : 
	/srl/i

reserved_word_subtype : 
	/subtype/i

reserved_word_then : 
	/then/i

reserved_word_to : 
	/to/i

reserved_word_transport : 
	/transport/i

reserved_word_type : 
	/type/i

reserved_word_unaffected : 
	/unaffected/i

reserved_word_units : 
	/units/i

reserved_word_until : 
	/until/i

reserved_word_use : 
	/use/i

reserved_word_variable : 
	/variable/i

reserved_word_wait : 
	/wait/i

reserved_word_when : 
	/when/i

reserved_word_while : 
	/while/i

reserved_word_with : 
	/with/i

reserved_word_xnor : 
	/xnor/i

reserved_word_xor : 
	/xor/i

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
	reserved_word_library library_name_list ';'  

library_name_list : 
	one_or_more_identifiers_separated_by_commas

use_clause : 
	reserved_word_use  selected_name_list  ';' 

selected_name_list :
	one_or_more_selected_names_separated_by_commas

####################################################
####################################################

entity_declaration : 
	reserved_word_entity entity_name reserved_word_is 
	optional_generic_declaration_section(?)
	optional_port_declaration_section(?)
	entity_declaritive_item(?)
	begin_entity_section(?)
	reserved_word_end ( reserved_word_entity )(?) identifier(?) ';'  
	 

begin_entity_section :
	reserved_word_begin
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
	reserved_word_architecture identifier reserved_word_of entity_name reserved_word_is
		block_declarative_item(s?)
	reserved_word_begin
		concurrent_statement(s?)
	reserved_word_end ( reserved_word_architecture )(?) identifier(?) ';' 

configuration_declaration :
	reserved_word_configuration identifier reserved_word_of entity_name reserved_word_is
		(   use_clause  
		  | attribute_specification
		  | group_declaration )
		block_configuration
	reserved_word_end ( reserved_word_configuration )(?) identifier(?) ';' 

block_configuration : 
	reserved_word_for architecture_name
	use_clause(?)
	reserved_word_end reserved_word_for ';'

package_declaration :
	reserved_word_package identifier reserved_word_is
		package_declarative_item(s?)
	reserved_word_end ( reserved_word_package )(?) identifier(?) ';'  
	
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
	reserved_word_package reserved_word_body identifier reserved_word_is
		package_body_declarative_item(s?)
	reserved_word_end ( reserved_word_package reserved_word_body )(?) identifier(?) ';' 

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
	reserved_word_procedure ( identifier | operator_symbol )
	optional_parameter_section(?) 

function_specification :
	pure_or_impure(?) 
	reserved_word_function 
	( identifier | operator_symbol ) 
	optional_parameter_section(?) 
	reserved_word_return
	type_mark 

pure_or_impure :
	( reserved_word_pure | reserved_word_impure )

subprogram_body :
	subprogram_specification reserved_word_is
		subprogram_declarative_part(?)
	reserved_word_begin
		sequential_statement(s?)
	reserved_word_end ( reserved_word_function | reserved_word_procedure )(?)
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
	reserved_word_type identifier ( reserved_word_is type_definition )(?) ';' 

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
	reserved_word_constant one_or_more_identifiers_separated_by_commas ':'
	subtype_indication 
	default_value(?) ';' 

signal_declaration :
	reserved_word_signal one_or_more_identifiers_separated_by_commas ':'
	subtype_indication  ( reserved_word_register | reserved_word_bus )(?)
	default_value(?) ';' 

shared_variable_declaration : reserved_word_shared variable_declaration
variable_declaration :
	reserved_word_variable one_or_more_identifiers_separated_by_commas ':'
	subtype_indication
	default_value(?) ';' 

file_declaration :
	reserved_word_file one_or_more_identifiers_separated_by_commas ':'
	subtype_indication
	open_file_expression_is_string_expression_option(?) ';' 
	

open_file_expression_is_string_expression_option :
	open_file_expression_option(?) reserved_word_is string_expression 

open_file_expression_option :
	reserved_word_open file_open_kind_expression 

alias_declaration :
	reserved_word_alias ( identifier | character_literal | operator_symbol ) 
	( ':' subtype_indication )(?)
	reserved_word_is name signature(?) ';' 

component_declaration :
	reserved_word_component component_name (reserved_word_is)(?)
	optional_generic_declaration_section(?)
	optional_port_declaration_section(?)
	reserved_word_end reserved_word_component component_name(?) ';'  

attribute_declaration :
	reserved_word_attribute identifier ':' type_mark ';'

attribute_specification :
	reserved_word_attribute identifier reserved_word_of entity_name_list ':' 
	entity_class reserved_word_is expression_rule ';'

entity_name_list :
	( list_of_id_or_char_or_op_with_optional_signature |
	reserved_word_others |
	reserved_word_all )

list_of_id_or_char_or_op_with_optional_signature :
	<leftop: id_or_char_or_op_with_optional_signature /(,)/
	id_or_char_or_op_with_optional_signature>(?)

id_or_char_or_op_with_optional_signature :
	( identifier | character_literal | operator_symbol )
	signature(?)

entity_class :
 		(
	  reserved_word_entity	| reserved_word_architecture | reserved_word_configuration | 
	  reserved_word_procedure   | reserved_word_function	 | reserved_word_package 	   | 
	  reserved_word_type	| reserved_word_subtype	 | reserved_word_constant	   | 
	  reserved_word_signal	| reserved_word_variable 	 | reserved_word_component	   | 
	  reserved_word_label 	| reserved_word_literal	 | reserved_word_units	   |
	  reserved_word_group	| reserved_word_file	
	)

configuration_specification :
	reserved_word_for component_specification binding_indication ';'

component_specification :
	( one_or_more_instantiation_labels_separated_by_commas |
	 reserved_word_others | reserved_word_all ) ':' component_name



binding_indication : 
	optional_use_entity_or_configuration_or_open(?)
	optional_generic_map_section(?)
	optional_port_map_section(?)
';'

optional_use_entity_or_configuration_or_open :
	reserved_word_use 
	(
		reserved_word_entity entity_name architecture_identifier(?) |
		reserved_word_configuration configuration_name |
		reserved_word_open
	)

disconnection_specification :
	reserved_word_disconnect 
	(
		one_or_more_signals_separated_by_commas |
		reserved_word_others |
		reserved_word_all
	)
	':' type_mark
	reserved_word_after time_expression ';'

group_template_declaration :
	reserved_word_group identifier reserved_word_is 
	'(' list_of_entity_class_with_option ')' ';'

list_of_entity_class_with_option :
	<leftop: entity_class_with_option /(,)/
	 entity_class_with_option>(?)

entity_class_with_option :
	entity_class

group_declaration :
	reserved_word_group identifier ':' group_template_name 
	'(' one_or_more_name_char_items_with_comma ')' ';'

one_or_more_name_char_items_with_comma :
	<leftop: name_or_char_literal /(,)/ name_or_char_literal>(?)

name_or_char_literal :
	( name | character_literal )

use_clause : reserved_word_use one_or_more_selected_names_separated_by_commas ';'

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
	simple_expression ( reserved_word_to | reserved_word_downto ) simple_expression 

integer_type_definition :
	reserved_word_range 
	( range_attribute_name |
	 simple_expression_to_downto_simple_expression )
 
floating_type_definition :
	reserved_word_range 
	( range_attribute_name |
	 simple_expression_to_downto_simple_expression )
 
physical_type_definition :
	reserved_word_range 
	( range_attribute_name |
	 simple_expression_to_downto_simple_expression )
	reserved_word_units identifier ';'
	identifier_is_physical_literal(?)
	reserved_word_end reserved_word_units identifier(?)

identifier_is_physical_literal :
	identifier '=' physical_literal

array_type_definition :
	reserved_word_array '('
	( array_type_mark_definition | array_discrete_range_definition )
	')' reserved_word_of
	element_subtype_indication

array_type_mark_definition :
	one_or_more_type_mark_ranges_separated_by_commas 

one_or_more_type_mark_ranges_separated_by_commas :
	<leftop: type_mark_range_box /(,)/ type_mark_range_box>(?)

type_mark_range_box :
	type_mark reserved_word_range '<>'

array_discrete_range_definition :
	one_or_more_discrete_ranges_separated_by_commas

record_type_definition :
	reserved_word_record 
		record_element_definition(s) 
	reserved_word_end reserved_word_record identifier(?)

record_element_definition :
	one_or_more_identifiers_separated_by_commas ':'
	subtype_indication ';'

access_type_definition :
	reserved_word_access subtype_indication

file_type_definition :
	reserved_word_file reserved_word_of type_mark

subtype_declaration :
	reserved_word_subtype identifier reserved_word_is subtype_indication ';'

subtype_indication :
	# resolution_function_name 
	type_mark
	optional_range_or_simple_or_discrete(?)  

optional_range_or_simple_or_discrete :
	  range_range_attribute_name_or_simple_downto_expression
	| '(' discrete_range ')'

range_range_attribute_name_or_simple_downto_expression :
	reserved_word_range 
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
	reserved_word_block
		optional_guard_expression(?)
		(reserved_word_is)(?)
		optional_generic_declaration_section(?)
		optional_generic_map_section(?)
		optional_port_declaration_section(?)
		optional_port_map_section(?)
		block_declarative_item(s?)
	reserved_word_begin
		concurrent_statement(s?)
	reserved_word_end reserved_word_block block_label(?) ';'

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
	( reserved_word_postponed )(?)
	reserved_word_process
	optional_sensitivity_list(?)
	(reserved_word_is)(?)
		process_declarative_item(s?)
	reserved_word_begin
		sequential_statement(s?)
	reserved_word_end (reserved_word_postponed)(?) reserved_word_process (process_label)(?) ';'

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
	(reserved_word_postponed)(?)
	procedure_name
	optional_parameter_section(?)
	';'

concurrent_assertion_statement :
	( label ':')(?)
	(reserved_word_postponed)(?)
	reserved_word_assert boolean_expression 
	(reserved_word_report expression_rule)(?)
	(reserved_word_severity expression_rule)(?) ';'

concurrent_signal_assignment_statement :
	( label ':')(?) (reserved_word_postponed)(?) 
	( selected_signal_assignment | conditional_signal_assignment )

conditional_signal_assignment :
	( name | aggregate ) 
	'<=' 
	(reserved_word_guarded)(?) 
	delay_mechanism(?)
	waveform_rule 
	when_boolean_expression_else_waveform_rule(s?)
	';' 

when_boolean_expression_else_waveform_rule :
	reserved_word_when 
	boolean_expression 
	reserved_word_else
	waveform_rule

selected_signal_assignment :
	reserved_word_with expression_rule reserved_word_select
	( name | aggregate ) 
	'<='
	(reserved_word_guarded)(?) 
	(delay_mechanism)(?)
	one_or_more_waveform_when_choices_comma_separated
	';'

one_or_more_waveform_when_choices_comma_separated :
	<leftop: waveform_when_choices /(,)/ waveform_when_choices>(?)

waveform_when_choices :
	waveform_rule reserved_word_when choices_separated_by_pipe 

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

component_instance_component_name :
	(reserved_word_component)(?) component_name  

component_instance_entity_name :
	reserved_word_entity entity_name optional_architecture_identifier(?)  

optional_architecture_identifier :
	 '(' architecture_identifier ')'  

component_instance_configuration_name :
	 reserved_word_configuration configuration_name 

generate_statement :
	generate_label ':'
	( for_identifier_in_range | if_boolean_expression )
	reserved_word_generate
	generate_block_declarative_item_and_begin(?)
	concurrent_statement(s?)
	reserved_word_end reserved_word_generate generate_label(?) ';'

for_identifier_in_range :
	reserved_word_for identifier reserved_word_in discrete_range

if_boolean_expression :
	reserved_word_if boolean_expression

generate_block_declarative_item_and_begin :
	block_declarative_item(s?)
	reserved_word_begin

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
	reserved_word_wait
	on_list_of_signal(?)
	( reserved_word_until boolean_expression )(?)
	( reserved_word_for time_expression )(?)
	';'

on_list_of_signal :
 		reserved_word_on one_or_more_signals_separated_by_commas

assertion_statement :
	( label ':' )(?)
	reserved_word_assert boolean_expression
	( reserved_word_report expression_rule )(?)
	( reserved_word_severity expression_rule )(?)
	';'

report_statement :
	( label ':' )(?)
	reserved_word_report expression_rule 
	( reserved_word_severity expression_rule )(?)
	';'

signal_assignment_statement :
	( label ':' )(?)
	( name | aggregate ) 
	'<='
	delay_mechanism(?)
	waveform_rule
	';'

delay_mechanism :
	( reserved_word_transport | inertial_with_optional_reject_time )

inertial_with_optional_reject_time :
	( reserved_word_reject time_expression )(?) reserved_word_inertial

waveform_rule :
	  reserved_word_unaffected 
	| one_or_more_waveform_items_separated_by_commas

one_or_more_waveform_items_separated_by_commas :
	<leftop: waveform_item /(,)/ waveform_item>(?) 

waveform_item :
	  null_with_optional_after_time_expression 
	| value_expression_with_optional_time_expression

null_with_optional_after_time_expression :
	reserved_word_null optional_after_time_expression(?) 

value_expression_with_optional_time_expression :
	value_expression 
	optional_after_time_expression(?)

optional_after_time_expression :
	reserved_word_after 
	time_expression 

variable_assignment_statement :
	( label ':')(?)
	( name | aggregate ) ':=' expression_rule ';'

procedure_call_statement :
	( label ':')(?)
	procedure_name optional_parameter_section(?) ';'

if_statement :
	( if_label ':')(?)
	reserved_word_if boolean_expression reserved_word_then
		sequential_statement(s)
	optional_elsif_section(s?)
	optional_else_section(?)
	reserved_word_end reserved_word_if if_label(?) ';'

optional_elsif_section :
	reserved_word_elsif boolean_expression reserved_word_then
		sequential_statement(s)

optional_else_section :
	reserved_word_else 
		sequential_statement(s)

case_statement :
	( case_label ':' )(?)
	reserved_word_case expression_rule reserved_word_is
		when_choices_sequential_statement(s)
	reserved_word_end reserved_word_case case_label(?) ';'

when_choices_sequential_statement :
	reserved_word_when choices_separated_by_pipe '=>' sequential_statement(s)

loop_statement :
	( loop_label ':')(?)
	( reserved_word_while boolean_expression |
	  reserved_word_for identifier reserved_word_in discrete_range )
	reserved_word_loop
		sequential_statement(s)
	reserved_word_end reserved_word_loop loop_label(?) ';'

next_statement :
	( label ':' )(?) reserved_word_next loop_label(?) 
	( reserved_word_when boolean_expression )(?) ';'

exit_statement :
	( label ':' )(?) reserved_word_exit loop_label(?) 
	( reserved_word_when boolean_expression )(?) ';'

return_statement :
	( label ':' )(?) reserved_word_return expression_rule(?) ';'

null_statement :
	( label ':' )(?) reserved_word_null ';'


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
	(reserved_word_constant)(?) one_or_more_identifiers_separated_by_commas ':' 
	(reserved_word_in)(?) subtype_indication default_value(?) ';'


signal_interface :
	(reserved_word_signal)(?) one_or_more_identifiers_separated_by_commas ':' 
	mode(?) subtype_indication (reserved_word_bus)(?) 
	default_value(?) ';'

variable_interface :
	(reserved_word_variable)(?) one_or_more_identifiers_separated_by_commas ':' 
	mode(?) subtype_indication default_value(?) ';'

file_interface :
	reserved_word_file one_or_more_identifiers_separated_by_commas ':'
	 subtype_indication ';'

mode : 
	 reserved_word_in | reserved_word_out | reserved_word_inout | reserved_word_buffer | reserved_word_linkage 

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
	| reserved_word_open 
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
	( reserved_word_and | reserved_word_nand | reserved_word_or | reserved_word_nor | reserved_word_xor | reserved_word_xnor ) relation

relation :
	shift_expression relation_shift_expression(?)

relation_shift_expression :
	( '=' | '/=' | '<' | '<=' | '>' | '>=' ) shift_expression 

shift_expression :
	simple_expression shift_simple_expression(?)

shift_simple_expression :
	 ( reserved_word_sll | reserved_word_srl | reserved_word_sla | reserved_word_sra | reserved_word_rol | reserved_word_ror )
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
	| reserved_word_mod 
	| reserved_word_rem

factor :
	  reserved_word_abs primary 
	| reserved_word_not primary 
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
	reserved_word_new subtype_indication |
	reserved_word_new qualified_expression 

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
	( identifier | character_literal | operator_symbol | reserved_word_all )

operator_symbol :
	'"' graphic_character '"'

attribute_name :
	identifier "'" identifier 

signature :
	'[' one_or_more_type_marks_separated_by_commas(?) ')' 
	( reserved_word_return type_mark )(?)

literal :
	  character_literal
	| string_literal 
	| bit_string_literal 
	| identifier
	| decimal_literal 
	| based_literal 
	| decimal_or_based_unit_name 
	| reserved_word_null

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

pipe : 
	'|'

one_of_several_choices :
	reserved_word_others | simple_expression | discrete_range | identifier  

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
	reserved_word_generic generic_interface_list(?) ';'


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
	reserved_word_generic reserved_word_map optional_generic_association_list(?)   

optional_generic_association_list :
	'(' generic_association_list ')' 

generic_association_list : 
	association_list 


####
# ports
####
optional_port_declaration_section : 
	reserved_word_port port_interface_list(?) ';'

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
	reserved_word_port reserved_word_map optional_port_association_list(?)   

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

#########################################################################
sub decomment_given_text
#########################################################################
{
 my ($obj,$text)=@_;

 my $filtered_text='';

 my $state = 'code';

 my ( $string_prior_to_comment, $string_after_comment);
 my ( $string_prior_to_quote, $string_after_quote);
 my ( $comment_string, $string_after_comment_string);
 my ( $quoted_string, $string_after_quoted_string);

 my $index_to_comment=0;
 my $index_to_quote =0;

 while (1)
  {
  if ($state eq 'code')
	{

	unless ( ($text =~ /--/) or ($text =~ /\"/) )
		{ 
		$filtered_text .= $text ;
		last;
		}


	# look for comment or quoted string
	( $string_prior_to_comment, $string_after_comment)
		= split( /--/ , $text, 2 );

	( $string_prior_to_quote, $string_after_quote)
		= split( /\"/ , $text, 2 );

	$index_to_comment = length($string_prior_to_comment);
	$index_to_quote   = length($string_prior_to_quote  );


	if($index_to_quote < $index_to_comment)
		{
		$state = 'quote'; 
		$filtered_text .= $string_prior_to_quote;
		$text =  $string_after_quote;
		$filtered_text .= '"' ;
		}
	else
		{ 
		$state = 'comment';
		$filtered_text .= $string_prior_to_comment;
		$text = '--' . $string_after_comment;
		}
	}

  elsif ($state eq 'comment')
	{
	# strip out everything from here to the next \n charater
	( $comment_string, $string_after_comment_string)
		= split( /\n/ , $text, 2  );

	$text = "\n" . $string_after_comment_string;

	$state = 'code';
	}

  elsif ($state eq 'quote')
	{
	# get the text until the next quote mark and keep it as a string
	( $quoted_string, $string_after_quoted_string)
		= split( /"/ , $text, 2  );

	$filtered_text .= $quoted_string . '"' ;
	$text =  $string_after_quoted_string;

	$state = 'code';
	}
  }

 ###################
 # make everything lower case, VHDL identifiers are case insensitive
 ###################
 ### $filtered_text = lc($filtered_text);  
  ## well, maybe this isn't such a good solution after all.

 return $filtered_text;

}


#########################################################################
sub Filename
#########################################################################
{
 my $obj = shift;

 while(@_)
	{
	my $filename = shift;
 	my $text = $obj->filename_to_text($filename);
 	$text = $obj->decomment_given_text($text);
 	$obj->design_file($text);
	}
}

#########################################################################
sub filename_to_text
#########################################################################
{
 my ($obj,$filename)=@_;
 open (FILE, $filename) or die "Cannot open $filename for read\n";
 my $text;
 while(<FILE>)
  {
  $text .= $_;
  }
 return $text;
}


#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
1;
__END__
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################

=head1 NAME

Hardware::Vhdl::Parser - Perl extension for parsing VHDL code

=head1 SYNOPSIS

  use Hardware::Vhdl::Parser;
  $parser = new Hardware::Vhdl::Parser;

  my @file = <>;
  my $file = join('',@file);
  $parser->design_file($file);

=head1 DESCRIPTION

##################################################################
# Copyright (C) 2000 Greg London   All Rights Reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.
##################################################################

This module defines the complete grammar needed to parse any VHDL code.
By overloading this grammar, it is possible to easily create perl scripts
which run through VHDL code and perform specific functions.

For example, a Hierarchy.pm uses Hardware::Vhdl::Parser to overload the
grammar rule for component instantiations. This single modification
will print out all instance names that occur in the module being parsed.
This might be useful for creating an automatic build script, or a graphical
hierarchical browser of a VHDL design.

=head1 AUTHOR

Greg London  greg42@bellatlantic.net

=head1 SEE ALSO

perl(1).
Parse::RecDescent

=cut

