#!/usr/bin/perl -w

# Top level testing for JSAN::Client itself

use strict;
use lib ();
use File::Spec::Functions ':ALL';
BEGIN {
	$| = 1;
	unless ( $ENV{HARNESS_ACTIVE} ) {
		require FindBin;
		$FindBin::Bin = $FindBin::Bin; # Avoid a warning
		chdir catdir( $FindBin::Bin, updir() );
		lib->import(
			catdir('blib', 'lib'),
			catdir('blib', 'arch'),
			'lib',
			);
	}
}

use JSAN::URI ();
use Test::More tests => 4;




#####################################################################
# Create an object for a mirror

my $mirror = JSAN::URI->new( 'http://master.openjsan.org/' );
isa_ok( $mirror, 'JSAN::URI' );

my $config = $mirror->_config;
my $master = $mirror->_master;
isa_ok( $config, 'Config::Tiny' );
isa_ok( $master, 'Config::Tiny' );

ok( $mirror->valid, "Mirror $mirror is valid" );

