#!/usr/bin/perl -w

# Basic test for JSAN::Index

use strict;
use lib ();
use UNIVERSAL 'isa';
use File::Spec::Functions ':ALL';
BEGIN {
	$| = 1;
	unless ( $ENV{HARNESS_ACTIVE} ) {
		require FindBin;
		chdir ($FindBin::Bin = $FindBin::Bin); # Avoid a warning
		lib->import( catdir( updir(), 'lib') );
	}
}

use Test::More tests => 5;

use File::Remove ();
use JSAN::Transport;
use JSAN::Index;

# Create and/or clear the test directory
my $testdir = catdir( curdir(), '05_extract' );
File::Remove::remove \1, $testdir if -e $testdir;
ok( ! -e $testdir, "Test directory '$testdir' does not exist" );
ok( mkdir($testdir), "Create test directory '$testdir'" );
END {
	File::Remove::remove \1, $testdir if -e $testdir;
}





#####################################################################
# Extract a known release into the test directory

# Find a known library
my $swap = JSAN::Index::Library->retrieve( name => 'Display.Swap' );
isa_ok( $swap, 'JSAN::Index::Library' );

# Attempt to extract it to the testdir
ok( $swap->extract_libs( to => $testdir ),
	'->extract_libs returns ok for Display.Swap' );
my $testfile = catfile( $testdir, 'Display', 'Swap.js' );
ok( -f $testfile, "->extract_libs created expected file '$testfile'" );

exit(0);
