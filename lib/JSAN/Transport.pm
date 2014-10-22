package JSAN::Transport;

=pod

=head1 NAME

JSAN::Transport - JavaScript Archive Network Transport and Resources

=head1 SYNOPSIS

  # Create a client
  JSAN::Transport->init(
  	verbose       => 1,
  	mirror_remote => 'http://openjsan.org/',
  	mirror_local  => '~/.jsan',
  	);
  
  # Retrieve a file
  JSAN::Transport->file_get( 'index.sqlite' );
  
  # Get an index database connection
  JSAN::Transport->index_dbh;
 
=head1 DESCRIPTION

C<JSAN::Transport> provides the primary programatic interface for creating
and manipulating a JSAN client transport and resource manager.

It controls connection to JSAN, retrieval and mirroring of files, and
database connectivity to the JSAN index.

=head1 METHODS

=cut

use strict;
use Carp                  ();
use File::Spec            ();
use File::Path            ();
use File::HomeDir         ();
use File::Basename        ();
use HTML::Location        ();
use LWP::Simple           ();
use DBI                   ();

use vars qw{$VERSION};
BEGIN {
	$VERSION = '0.11';

	# Optional prefork.pm support
	eval "use prefork 'Class::DBI';";
}

# The path to the index
my $SQLITE_INDEX = 'index.sqlite';





#####################################################################
# Constructor

=pod

=head2 init param => $value, ...

The C<init> method initializes the JSAN client adapter. It takes a set of
parameters and initializes the C<JSAN::Transport> class.

Once you have initialized C<JSAN::Transport>, any further attempts to do
so will result in an exception being thrown.

The C<init> method is also aliased to C<import>, so that you can do
something like the following.

  # Initialize JSAN::Transport at use-time
  use JSAN::Transport verbose       => 1,
                      mirror_remote => 'http://openjsan.org',
                      mirror_local  => '~/.jsan';

=over 4

=item mirror_remote

The C<mirror_remoter> param specifies the JSAN mirror to be retrieve packages
and other files from. If you do not specify a location, a default value of
C<http://openjsan.org> will be used.

=item mirror_local

A JSAN client downloads and caches various files from the repository. This
primarily means the JSAN packages themselves, but also includes things like
the index and other control files. This effectively creates a partial local
mirror of the repository, although it will also include some working files
not found on the server.

The C<mirror_local> param specifies the path for the local mirror. If the
path provided does not exist, the constructor will attempt to create it. If
creation fails the constructor will throw an exception.

=item verbose

Mainly used for console interfaces for user feedback, the C<verbose> flag
causes the client to print various information to C<STDOUT> as it does
various tasks.

=back

Returns true, or will throw an exception (i.e. die) on error.

=cut

my $SINGLETON = undef;

sub init {
	Carp::croak("CPAN::Transport already initialized") if $SINGLETON;
	my $class  = shift;
	my $params = { @_ };

	# Create the empty object
	my $self = bless {
		verbose => !! $params->{verbose},
		mirror  => undef,
		}, $class;

	# Apply defaults
	my $uri  = $params->{mirror_remote} || 'http://openjsan.org';
	my $path = $params->{mirror_local}  || File::Spec->catdir(
		File::HomeDir::home(), '.jsan'
		);

	# Strip superfluous trailing slashes
	$path =~ s/\/+$//;
	$uri  =~ s/\/+$//;

	# Create the mirror_local path if needed
	-e $path or File::Path::mkpath($path);
	-d $path or Carp::croak("mirror_local: Path '$path' is not a directory");
	-w $path or Carp::croak("mirror_local: No write permissions to path '$path'");

	# Create the location object
	$self->{mirror} = HTML::Location->new( $path => $uri )
		or Carp::croak("Unexpected error creating HTML::Location object");

	$SINGLETON = $self;
	1;
}

sub import { shift->init(@_) }

sub _self {
	return shift if ref $_[0];
	$SINGLETON or Carp::croak("JSAN::Transport not intialized");
}

=pod

The C<mirror_location> accessor returns the L<HTML::Location> of the
L<URI> to local path map.

=cut

sub mirror_location { shift->_self->{mirror} }

=pod

The C<mirror_remote> accessor returns the location of the remote mirror
configured when the object was created.

=cut

sub mirror_remote { shift->_self->{mirror}->uri }

=pod

The C<mirror_local> accessor returns the location of the local mirror
configured when the object was created.

=cut

sub mirror_local { shift->_self->{mirror}->path }

=pod

The C<verbose> accessor returns the boolean flag on whether the object
is running in verbose mode.

=cut

sub verbose { shift->_self->{verbose} }

1;





#####################################################################
# JSAN::Transport Methods

=pod

=head2 file_location path/to/file.txt

The C<file_location> method takes the path of a file within the
repository, and returns a L<HTML::Location> object representing
it's location on both the server, and on the local filesystem.

Paths should B<always> be provided in unix/web format, not the
local filesystem's format.

Returns a L<HTML::Location> or throws an exception if passed a
bad path.

=cut

sub file_location {
	my $self = shift->_self;
	my $path = $self->_path(shift);

	# Strip any leading slash
	$path =~ s/^\///;

	# Split into parts and find the location for it.
	my @parts    = split /\//, $path;
	my $mirror   = $self->mirror_location;
	$mirror->catfile( @parts );
}

=pod

=head2 file_get path/to/file.txt

The C<file_get> method takes the path of a file within the
repository, and fetches it from the remote repository, storing
it at the appropriate local path.

As all C<file_> operations, paths should B<always> be provided
in unix/web format, not the local filesystem's format.

Returns the L<HTML::Location> for the file if retrieved successfully,
false (C<''>) if the file did not exist in the repository, or C<undef>
on error.

=cut

sub file_get {
	my $self     = shift->_self;
	my $location = $self->file_location(shift);

	# Check local dir exists
	my $dir = File::Basename::dirname($location->path);
	-d $dir or File::Path::mkpath($dir);

	# Fetch the file from the server
	my $rc = LWP::Simple::getstore( $location->uri, $location->path );
	if ( LWP::Simple::is_success($rc) ) {
		return $location;
	} elsif ( $rc == LWP::Simple::RC_NOT_FOUND ) {
		return '';
	} else {
		Carp::croak("$rc error retriving file " . $location->uri);
	}
}

=pod

=head2 file_mirror path/to/file.txt

The C<file_mirror> method takes the path of a file within the
repository, and mirrors it from the remote repository, storing
it at the appropriate local path.

Using this method if preferable for items like indexs for which
want to ensure you have the current version, but do not want to
freshly download each time.

As all C<file_> operations, paths should B<always> be provided
in unix/web format, not the local filesystem's format.

Returns the L<HTML::Location> for the file if mirrored successfully,
false (C<''>) if the file did not exist in the repository, or
C<undef> on error.

=cut

sub file_mirror {
	my $self     = shift->_self;
	my $path     = $self->_path(shift);
	my $location = $self->file_location($path);

	# If any only if a path is "stable" and the file already exists,
	# it is guarenteed not to change, and we don't have to do the
	# mirroring operation.
	if ( $self->_path_stable($path) and -f $location->path ) {
		return $location;
	}

	# Check local dir exists
	my $dir = File::Basename::dirname($location->path);
	-d $dir or File::Path::mkpath($dir);

	# Fetch the file from the server
	my $rc = LWP::Simple::mirror( $location->uri, $location->path );
	if ( LWP::Simple::is_success($rc) ) {
		return $location;
	} elsif ( $rc == LWP::Simple::RC_NOT_MODIFIED ) {
		return $location;
	} elsif ( $rc == LWP::Simple::RC_NOT_FOUND ) {
		return '';
	} else {
		Carp::croak("HTTP error $rc while syncing file " . $location->uri);
	}
}

=pod

=head2 index_file

The C<index_file> method checks that the SQLite index is up to date, and
returns the path to it on the filesystem.

=cut

sub index_file {
	shift->_self->_index_synced->path;
}

=pod

=head2 index_dsn

The C<index_dsn> method checks the SQLite index is up to date, and
return the L<DBI> dsn for the index database.

=cut

sub index_dsn {
	my $self = shift->_self;
	my $file = $self->index_file;
	"dbi:SQLite:dbname=$file";
}

=pod

=head2 index_dbh

The C<index_dbh> method check the SQLite index is up to date, and
return a database connection to the index database.

=cut

sub index_dbh {
	my $self = shift->_self;
	my $DSN  = $self->index_dsn;

	# Unless we use Class::DBI's attributes, the whole thing comes
	# tumbling horribly down around us. Yes, this completely sucks.
	require Class::DBI;
	my %attr = Class::DBI->_default_attributes;

	DBI->connect( $DSN, '', '', \%attr )
		or Carp::croak("Database error connecting to JSAN index at $DSN");
}





#####################################################################
# Support Methods

# Validate a JSAN file path
sub _path {
	my $self = shift->_self;
	my $path = shift or Carp::croak("No JSAN file path provided");

	# Strip any leading slash
	$path =~ s(^\/)();

	$path;
}

# Is a path considered "stable" (does not change over time)
sub _path_stable {
	my $self = shift->_self;
	my $path = $self->_path(shift);

	# Paths under the "dist" path are stable
	if ( $path =~ m{^dist/} ) {
		return 1;
	}

	'';
}

# Returns the location of the SQLite index, syncronising it if needed
sub _index_synced {
	my $self = shift->_self;
	if ( $self->{index_synced} ) {
		return $self->file_location($SQLITE_INDEX);
	}
	my $location = $self->file_mirror($SQLITE_INDEX);
	$self->{index_synced}++;
	$location;
}

=pod

=head1 TO DO

- Add verbose support

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
