
##################################################################
# Copyright (C) 2000 Greg London   All Rights Reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.
##################################################################

##################################################################
package Hardware::Vhdl::Hierarchy;
use Hardware::Vhdl::Parser;
@ISA = ( 'Hardware::Vhdl::Parser' );
##################################################################
use vars qw ( $VERSION );
$VERSION = '0.01';
##################################################################

##################################################################
sub new
##################################################################
{
 my ($pkg) = @_;

 # make Hardware::Vhdl::Parser object, use SUPER:: to find the method via @ISA
 my $r_hash = $pkg->SUPER::new;

 $r_hash -> Replace ( q( 
component_instantiation_statement :
	instantiation_label 
	':'
	(
		reserved_word_entity_and_entity_name_arch_name_in_parens |
		reserved_word_configuration_and_configuration_name |
		reserved_word_component_and_component_name 
	)
	generic_map_section(?)
	port_map_section(?)
	';'

		{ print "INSTANCENAME $item{instantiation_label} \n"; }
 ));


 # bless it as a vhdl_hierarchy object
 bless $r_hash, $pkg;
 return $r_hash;
} 

