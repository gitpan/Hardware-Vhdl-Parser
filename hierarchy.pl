#! /bin/perl -sw

####################################################
# Copyright (C) 2000 Greg London
# All Rights Reserved.
####################################################

use Hardware::Vhdl::Hierarchy;
$parse = new Hardware::Vhdl::Hierarchy;

# $::RD_TRACE = 1;
$::RD_WARN = undef;
$::RD_HINT = undef;

my @file = <>;
my $file = join('',@file);
$parse->design_file($file);



