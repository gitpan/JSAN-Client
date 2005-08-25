package JSAN::Client;

=pod

=head1 NAME

JSAN::Client - JavaScript Archive Network Client Object

=head1 SYNOPSIS

  # Create the client object
  my $client = JSAN::Client->new(
  	prefix  => '/usr/local/js',
  	verbose => 1,
  	);

  # Install by library name
  $client->install_library( 'Display.Swap' );
  
  # Install by distribution name
  $client->install_distribution( 'DOM.Trigger' );

=head1 DESCRIPTION

The C<JSAN::Client> API intended to provide the highest-possible level
abstraction for the process of installing JSAN distributions.

However please not this class is still intended for developers, and
won't do things like auto-detect your C<JSAN_PREFIX> and so on.

For more DWIM type functionality aimed at users, see the L<JSAN::Shell>
class, or even more preferably the L<jsan> installer application.

=head1 METHODS

=cut

use strict;
use JSAN::Transport;
use JSAN::Index;

use vars qw{$VERSION};
BEGIN {
	$VERSION = '0.07';
}





#####################################################################
# Constructor and Accessors

=pod

=head2 new param => 'value', ...

The C<new> constructor takes a set of key/value params and creates a new
JSAN installation client, who's purpose is to install JSAN distributions
to a specific location on the local disk.

Please note that although you can create multiple C<JSAN::Client> objects
for multiple install paths, all clients will share the common L<JSAN::Index>
and L<JSAN::Transport> layers, which means both that distribution packages
will be cached across all the clients, and that they B<must> install from
the same remote JSAN repository.

The constructor takes the following parameters

=over 4

=item prefix

The C<prefix> params should be the location on the local disk that the JSAN
libraries contained in the distribution should be installed to.

=item verbose

The C<verbose> (current not implemented) enables verbose mode for the client.

In verbose mode, the client will write a number of procedural and diagnostic
messages to C<STDOUT> as it processes the installation requests.

=back

Returns a new C<JSAN::Client> object, or dies on error.

=cut

sub new {
	my $class  = ref $_[0] ? ref shift : shift;
	if ( scalar(@_) % 2 ) {
		Carp::croak("Odd number of params passed to JSAN::Client::new");
	}
	my %params = @_;

	# Create the basic object
	my $self = bless {
		prefix  =>    $params{prefix},
		verbose => !! $params{verbose},
		}, $class;

	# Check the prefix
	unless ( $self->prefix ) {
		Carp::croak("No prefix provided to JSAN::Client::new");
	}
	unless ( -d $self->prefix and -w $self->prefix ) {
		Carp::croak("Prefix provided to JSAN::Client::new is not a writable directory");
	}

	$self;			
}

=pod

=head2 prefix

The C<prefix> accessor returns the installation prefix path for the client.

=cut

sub prefix { $_[0]->{prefix} }

=pod

=head2 verbose

The C<verbose> accessor returns the boolean flag indicating whether the
client is running in verbose mode.

=cut

sub verbose { $_[0]->{verbose} }




#####################################################################
# JSAN::Client Methods

=pod

=head2 install_library $name

The C<install_library> method takes the name of a JSAN library and
installs the most recent release of the distribution that the library is
contained in as indicated in the JSAN index.

Any dependencies required for the library will also be installed as needed.

Please note the difference between a JSAN "library" and a JSAN
"distribution". There are often many libraries contained in a single
distribution.

Returns true if the library was installed, false if the library is already
up to date and did not need to be installed, or dies on error.

=cut

sub install_library {
	my $self    = shift;

	# Does the library exist?
	my $library = JSAN::Index::Library->retrieve( name => $_[0] )
		or Carp::croak("The JSAN library '$_[0]' does not exist");

	$self->_install_release( $library->release, $_[0] );
}

=pod

=head2 install_distribution $name

The C<install_distribution> method takes the name of a JSAN distribution and
installs the most recent release of that distribution.

Any dependencies required for the distribution will also be installed as
needed.

Please note the difference between a JSAN "library" and a JSAN
"distribution". There are often many libraries contained in a single
distribution.

Returns true if the distribution was installed, false if the distribution
is already up to date and did not need to be installed, or dies on error.

=cut

sub install_distribution {
	my $self    = shift;

	# Does the distribution exist?
	my $distribution = JSAN::Index::Distribution->retrieve( name => $_[0] )
		or Carp::croak("The JSAN distribution '$_[0]' does not exist");

	$self->_install_release( $distribution->latest_release, $_[0] );
}

# Takes a JSAN::Index::Release object, and installs it
sub _install_release {
	my ($self, $requested, $name) = @_;

	# Find the full schedule
	my $dependency = JSAN::Index->install_dependency;
	my $schedule   = $dependency->schedule( $requested )
		or Carp::croak("Error while finding dependencies for '$name'");
	my @releases = map {
		JSAN::Index::Release->retrieve( source => $_ )
			or Carp::croak("Failed to get an object for '$_'")
		} @$schedule;

	# Following debian's lead, download all the releases first
	foreach my $release ( @releases ) {
		$release->mirror;
	}

	# Install each of the releases
	foreach my $release ( @releases ) {
		$self->_extract_release( $release );
	}

	1;
}

# Takes a single JSAN::Index::Release object, and extracts its libs to the prefix dir
sub _extract_release {
	my ($self, $release) = @_;
	$release->extract_libs( to => $self->prefix );
}

1;

=pod

=head1 TO DO

- Add the testing dependency algorithm variant

- Add support for JSON META.yml files

=head1 SUPPORT

Bugs should be reported via the CPAN bug tracker at

L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=JSAN-Client>

For other issues, contact the author.

=head1 AUTHOR

Adam Kennedy E<lt>cpan@ali.asE<gt>, L<http://ali.as/>

=head1 COPYRIGHT

Copyright 2005 Adam Kennedy. All rights reserved.

This program is free software; you can redistribute
it and/or modify it under the same terms as Perl itself.

The full text of the license can be found in the
LICENSE file included with this module.

=cut
