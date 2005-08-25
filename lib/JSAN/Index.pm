package JSAN::Index;

=pod

=head1 NAME

JSAN::Index - JavaScript Archive Network Index SQLite/CDBI Interface

=head1 DESCRIPTION

JSAN is the JavaScript Archive Network, a CPAN implementation for Javascript.

You can find the JSAN at L<http://openjsan.org>.

As well as a flat text file index like CPAN, the JSAN index is also
distributed as a SQLite database.

C<JSAN::Index> is a L<Class::DBI> wrapper built around the JSAN SQLite index.

It allow you to easily do all sorts of nifty things with the index in a
simple and straight forward way.

=head1 STATUS

This is currently only for the use of JSAN developers, as the index is still
going through some amount of flux. It should not be used by outsiders until
the index has stabilised a little and we assign a non-devel version

=head1 JSAN::Index

The C<JSAN::Index> class itself currently contains a limited number of
shortcut and utility methods.

=cut

use strict;

use vars qw{$VERSION};
BEGIN {
	$VERSION = '0.08';
}





#####################################################################
# Top-level Methods

=pod

=head2 dependency param => $value

The C<dependency> method creates and returns an dependency resolution 
object that is used by L<JSAN::Client> to schedule which releases to
install.

If the optional parameter 'build' is true, creates a build-time
dependency resolve, which will additionally install releases only needed
for testing.

Returns an L<Algorithm::Dependency> object.

=cut

sub dependency {
	my $class  = shift;
	my %params = @_;
	$params{type} = 'install';
	JSAN::Index::Release::_Dependency->new( %params );
}





#####################################################################
# CDBI-Initialisation

package JSAN::Index::CDBI;

=pod

=head1 JSAN::Index::CDBI

The C<JSAN::Index::CDBI> class acts as a base class for the index and provides
the integration with L<Class::DBI> and L<JSAN::Transport>.

It has no user-servicable parts at this time.

=cut

use JSAN::Transport ();
use base 'Class::DBI';

use vars qw{$VERSION};
BEGIN {
	$VERSION = '0.08';
}

my $dbh;

sub db_Main {
	$dbh or
	$dbh = JSAN::Transport->index_dbh;
}





#####################################################################
# Authors

package JSAN::Index::Author;

=pod

=head1 JSAN::Index::Author

This class provides objects for authors in the JSAN index.

In addition to the general methods provided by L<Class::DBI>, it has the
following methods

=head2 login

The C<login> accessor returns the JSAN author code/login for the author.

=head2 name

The C<name> accessor returns the full name of the author.

=head2 doc

The C<doc> accessor returns the root-relative documentation path for the
author on any L<http://openjsan.org/> mirror.

=head2 email

The C<email> accessor returns the public email address for the author.

=head2 url

The C<url> acessor returns the uri for the authors homepage as a string.

=head2 releases

The C<releases> method finds and retrieves all of the releases for an author.

Returns a list of L<\JSAN::Index::Release> objects.

=cut

use vars qw{$VERSION @ISA};
BEGIN {
	$VERSION = '0.08';
	@ISA     = 'JSAN::Index::CDBI';
}

JSAN::Index::Author->table('author');
JSAN::Index::Author->columns( Essential =>
	'login',    # AUTHOR login id        - 'adamk'
	'name',     # Full Name              - 'Adam Kennedy'
	            #                          'Han Kwai Teow'
	'doc',      # openjsan.org doc path  - '/doc/a/au/adamk
	'email',    # Public email address   - 'jsan@ali.as'
	'url',      # Personal website       - 'http://ali.as/'
	);
JSAN::Index::Author->columns(
	Primary => 'login',
	);
JSAN::Index::Author->has_many(
	releases  => 'JSAN::Index::Release',
	);




#####################################################################
# Extractable Things

package JSAN::Index::Extractable;

=pod

=head1 JSAN::Index::Extractable

JSAN::Index::Extractable provides a common base class for the various
things that can be identified by a tarball and extracted to the local
filesystem (or elsewhere).

For each of the methods, when called on a C<JSAN::Index::Release> it
extracts that release, when called on a C<JSAN::Index::Distribution>
extracts from the most recent release, and when call on a
C<JSAN::Index::Library> extracts the release that the library is
contained in (according to the indexer).

=cut

use vars qw{$VERSION @ISA};
BEGIN {
	$VERSION = '0.08';
	@ISA     = 'JSAN::Index::CDBI';
}

=pod

=head2 extract_libs to => $path

The C<extract_libs> method will extract the libraries for a release
(i.e. the contents of the C<lib> directory> to the local filesystem.

Returns the number of files extracted, or dies on error.

=cut

sub extract_libs {
	my $self = shift;
	$self->extract_resource('lib', @_);
}

=pod

=head2 extract_tests to => $path

The C<extract_tests> method will extract the test scripts for a release
(i.e. the contents of the C<tests> directory> to the local filesystem.

Returns the number of files extracted, or dies on error.

=cut

sub extract_tests {
	my $self = shift;
	$self->extract_resource('tests', @_);
}

=pod

=head2 extract_resource to => 'path';

All JSAN tarballs contain a number of standard "resource"
directories. The most common of these is the 'lib' resource.

It takes named parameters to control its behaviour.

=over 4

=item to

The C<to> parameter specifies the destination for the files to be
extracted to. When passed as a single string, this is taken to be a
directory on the local host.

No other destination options other than the local filesystem are
available at this time, but more destination options are expected at
a later date.

=back

=cut

sub extract_resource {
	my $class = ref $_[0] || $_[0];
	Carp::croak("$class does not implement method 'extract_resource'");
}





#####################################################################
# Indexed Libraries

package JSAN::Index::Library;

=pod

=head1 JSAN::Index::Library

This class provides objects for the various libraries in the JSAN.

In addition to the general methods provided by L<Class::DBI>, it has the
following methods

=head2 name

The C<name> accessor returns the name (possibly including the use of
pseudo-namespaces) of the library. e.g. "Test.Simple.Container.Browser"

=head2 release

The C<release> method returns the L<\JSAN::Index::Release> object for the
release that the library is defined in.

=head2 version

The C<version> accessor returns the version of the library.

=head2 doc

The C<doc> accessor returns the root-relative location of the documentation
for this library on the L<http://openjsan.org/> website.

=cut

use vars qw{$VERSION @ISA};
BEGIN {
	$VERSION = '0.08';
	@ISA     = 'JSAN::Index::Extractable';
}

JSAN::Index::Library->table('library');
JSAN::Index::Library->columns( Essential =>
	'name',         # Library namespace          - 'Display.Swap'
	'release',      # Release containing library - '/dist/ADAMK/Display.Swap-0.01.tar.gz'
	'version',      # Library version            - '0.01' or '~'
	'doc',          # Doc path
	);
JSAN::Index::Library->has_a(
	release => 'JSAN::Index::Release',
	);

=pod

=head2 distribution

The C<distribution> method is a shortcut for
C<$library-E<gt>release-E<gt>distribution> and returns the
L<\JSAN::Index::Distribution> for the distribution that this library
is of.

=cut

sub distribution {
	shift()->release->distribution;
}

sub extract_resource {
	shift()->release->extract_resource(@_);
}





#####################################################################
# Distributions

package JSAN::Index::Distribution;

=pod

=head1 JSAN::Index::Distribution

This class provides objects for named distributions in the JSAN index.

In addition to the general methods provided by L<Class::DBI>, it has the
following methods

=head2 name

The C<name> accessor returns the name of the distribution.

=head2 doc

The C<doc> accessor returns the root-relative location of the documentation
for this distribution on the L<http://openjsan.org/> website.

=head2 releases

The C<releases> method finds and retrieves all of the releases of the
distribution.

Returns a list of L<\JSAN::Index::Release> objects.

=cut

use vars qw{$VERSION @ISA};
BEGIN {
	$VERSION = '0.08';
	@ISA     = 'JSAN::Index::Extractable';
}

JSAN::Index::Distribution->table('distribution');
JSAN::Index::Distribution->columns( Essential =>
	'name',    # Name in META.yml       - 'Display-Swap'
	'doc',     # openjsan.org doc path  - '/doc/a/ad/adamk/Display/Swap'
	);
JSAN::Index::Distribution->columns(
	Primary  => 'name',
	);
JSAN::Index::Distribution->has_many(
	releases => 'JSAN::Index::Release',
	);

# Returns the latest release of the distribution
sub latest_release {
	my $self = shift;
	my @releases = $self->releases( { order_by => 'version desc' } )
		or Carp::croak("No releases found for distribution "
			. $self->name );
	$releases[0];
}

sub extract_resource {
	my $self    = shift;
	my $release = $self->latest_release;
	$release->extract_resource(@_);
}





#####################################################################
# Releases

package JSAN::Index::Release;

=pod

=head1 JSAN::Index::Release

This class provides objects for a single release of a distribution by an author

In addition to the general methods provided by L<Class::DBI>, it has the
following methods

=head2 id

The C<id> accessor returns the unique identifier for the release (an integer)

=head2 source

The C<source> access returns the root-relative path within a JSAN mirror
that the package can be found at.

=head2 distribution

The C<distribution> method returns the L<\JSAN::Index::Distribution> for
the distribution that this release is of.

=head2 author

The C<author> method returns the L<\JSAN::Index::Author> for the JSAN author
that uploaded the release.

=head2 version

The C<version> accessor returns the version of the release.

=head2 created

The C<created> accessor returns the time that the release was received and
first indexed by the JSAN upload server.

Returns an integer in unix epoch time.

=head2 doc

The C<doc> accessor returns the root-relative location of the documentation
for this release on the L<http://openjsan.org/> website.

=head2 meta

The C<meta> accessor returns the actual content of the C<META.yml> file that
came with the distribution.

=head2 checksum

The C<checksum> accessor returns the MD5 checksum for the release tarball.

=head2 latest

The C<latest> accessor returns a boolean flag indicating if the release is
the most recent release of the distribution.

=cut

use File::Spec       ();
use File::Spec::Unix ();
use File::Path       ();
eval "use prefork 'YAML'";
eval "use prefork 'Archive::Tar'";
eval "use prefork 'Archive::Zip'";

# Make the tar code read saner below
use constant COMPRESSED => 1;

use vars qw{$VERSION @ISA};
BEGIN {
	$VERSION = '0.08';
	@ISA     = 'JSAN::Index::Extractable';
}

JSAN::Index::Release->table('release');
JSAN::Index::Release->columns( Essential =>
	'id',           # Unique identifier
	'source',       # File path within JSAN   - '/dist/ADAMK/Display.Swap-0.01.tar.gz'
	'distribution', # Distribution            - 'Display.Swap'
	'author',       # The uploading author    - 'ADAMK'
	'version',      # Release version         - '0.01'
	'created',      # Unix time first indexed - '1120886566'
	'doc',          # openjsan.org doc path   - '/doc/ADAMK/Display/Swap/0.01'
	'meta',         # contents of the META.yml file
	'checksum',     # MD5 checksum            - '996156f963ad58c1b55621390e5c2066'
	'latest',       # Most recent release?    - 1
	);
JSAN::Index::Release->columns(
	Primary      => 'id',
	);
JSAN::Index::Release->has_a(
	distribution => 'JSAN::Index::Distribution',
	);
JSAN::Index::Release->has_a(
	author       => 'JSAN::Index::Author',
	);

=pod

=head2 requires

The C<requires> method finds the set of run-time library dependencies for
this release, as identified in the META.yml data contained in the index.

Returns a reference to a HASH where the key is the name of a library as
a string, and the value is the version for the dependency (or zero if the
dependency is not for a specific version).

=cut

sub requires {
	my $self = shift;

	# Get the raw dependency hash
	my $meta = $self->meta_data;
	unless ( UNIVERSAL::isa($meta, 'HASH') ) {
		# If it has no META.yml at all, we assume that it
		# has no dependencies.
		return ();
	}
	my $requires = $meta->{requires} or return {};
	if ( UNIVERSAL::isa($requires, 'HASH') ) {
		# To be safe (mainly in case it's a dependency object of
		# some sort) make sure it's a plain hash before returning.
		my %hash = %$requires;
		return \%hash;
	}

	# It could be an array of Requires objects
	if ( UNIVERSAL::isa($requires, 'ARRAY') ) {
		my %hash = ();
		foreach my $dep ( @$requires ) {
			unless ( UNIVERSAL::isa($dep, 'Module::META::Requires') ) {
				Carp::croak("Unknown dependency structure in META.yml for "
					. $self->source);
			}
			$hash{ $dep->{name} } = $dep->{version};
		}
		return \%hash;
	}

	Carp::croak("Unknown 'requires' dependency structure in META.yml for "
		. $self->source);
}

=pod

=head2 build_requires

The C<build_requires> method finds the set of build-time library
dependencies for this release, as identified in the META.yml data contained
in the index.

Returns a reference to a HASH where the key is the name of a library as
a string, and the value is the version for the dependency (or zero if the
dependency is not for a specific version).

=cut

sub build_requires {
	my $self = shift;

	# Get the raw dependency hash
	my $meta = $self->meta_data;
	unless ( UNIVERSAL::isa($meta, 'HASH') ) {
		# If it has no META.yml at all, we assume that it
		# has no dependencies.
		return ();
	}
	my $requires = $meta->{build_requires} or return {};
	if ( UNIVERSAL::isa($requires, 'HASH') ) {
		# To be safe (mainly in case it's a dependency object of
		# some sort) make sure it's a plain hash before returning.
		my %hash = %$requires;
		return \%hash;
	}

	# It could be an array of Requires objects
	if ( UNIVERSAL::isa($requires, 'ARRAY') ) {
		my %hash = ();
		foreach my $dep ( @$requires ) {
			unless ( UNIVERSAL::isa($dep, 'Module::META::Requires') ) {
				Carp::croak("Unknown dependency structure in META.yml for "
					. $self->source);
			}
			$hash{ $dep->{name} } = $dep->{version};
		}
		return \%hash;
	}

	Carp::croak("Unknown 'build_requires' dependency structure in META.yml for "
		. $self->source);
}

=pod

=head2 requires_libraries

The C<requires_libraries> method returns a list of the C<JSAN::Index::Library>
dependencies as identified by the META.yml file for the release.

=cut

sub requires_libraries {
	my $self     = shift;
	my $requires = $self->requires;

	# Find the library object for each key
	my @libraries = ();
	foreach my $name ( sort keys %$requires ) {
		my $library = JSAN::Index::Library->retrieve( name => $name );
		push @libraries, $library if $library;
	}

	@libraries;
}

=pod

=head2 build_requires_libraries

The C<build_requires_libraries> method returns a list of the build-time
C<JSAN::Index::Library> dependencies as identified by the META.yml file
for the release.

=cut

sub build_requires_libraries {
	my $self     = shift;
	my $requires = $self->build_requires;

	# Find the library object for each key
	my @libraries = ();
	foreach my $name ( sort keys %$requires ) {
		my $library = JSAN::Index::Library->retrieve( name => $name );
		push @libraries, $library if $library;
	}

	@libraries;
}

=pod

=head2 requires_releases

The C<requires_releases> method returns a list of the C<JSAN::Index::Release>
dependencies based on the dependencies specified in the META.yml file for the
release.

=cut

sub requires_releases {
	my $self      = shift;
	my @libraries =	$self->requires_libraries;

	# Derive a list of releases
	my @releases = map { $_->release } @libraries;
	return @releases;
}

=pod

=head2 build_requires_releases

The C<requires_releases> method returns a list of the C<JSAN::Index::Release>
build-time depedencies based on those specified in the META.yml file for the
release.

=cut

sub build_requires_releases {
	my $self      = shift;
	my @libraries =	$self->build_requires_libraries;

	# Derive a list of releases
	my @releases = map { $_->release } @libraries;
	return @releases;
}

=pod

=head2 meta_data

The C<meta_data> method loads and deserialises the META.yml content
contained in the index (and returned by the C<meta> method above).

=cut

sub meta_data {
	my $self    = shift;
	require YAML;
	my @structs = YAML::Load($self->meta);
	unless ( defined $structs[0] ) {
		Carp::croak("Failed to load META.yml struct for "
			. $self->source );
	}
	$structs[0];
}

=pod

=head2 mirror

The C<mirror> method fetches the tarball from your JSAN currently configured
JSAN mirror as determined by L<JSAN::Transport> (if not already cached).

Returns a file path to the tarball on the local machine, or may emit an
exception thrown by the underlying L<JSAN::Transport> functions.

=cut

sub mirror {
	my $self     = shift;
	my $location = JSAN::Transport->file_mirror($self->source);
	$location->path;
}

=pod

=head2 file_mirrored

The C<file_mirrored> method checks to see if the release tarball has previously
been downloaded to the local mirror.

Returns true if the file exists in the local mirror, or false if not.

=cut

sub file_mirrored {
	!! -f $_[0]->file_path;
}

=pod

=head2 file_path

The C<file_path> method returns the location on the local disk where
the release tarball should be, if mirrored.

=cut

sub file_path {
	JSAN::Transport->file_location($_[0]->source)->path;
}

=pod

=head2 archive

The C<archive> method returns the release as an in-memory archive.
Depending on the type, this should be either a L<Archive::Tar> or an
L<Archive::Zip> object.

=cut

sub archive {
	# Cache result of the real method
	$_[0]->{archive} or
	$_[0]->{archive} = $_[0]->_archive;
}

sub _archive {
	my $self = shift;

	# Load tarballs
	if ( $self->source =~ /\.tar\.gz/ ) {
		require Archive::Tar;
		my $tar  = Archive::Tar->new;
		my $path = $self->mirror;
		$tar->read($path, COMPRESSED)
			or Carp::croak("Failed to open tarball '$path'");
		return $tar;
	}

	# Load zip files
	if ( $self->source =~ /\.zip$/ ) {
		require Archive::Zip;
		my $zip  = Archive::Zip->new;
		my $path = $self->mirror;
		unless ( $zip->read($path) == Archive::Zip::AZ_OK() ) {
			Carp::croak("Failed to open zip file '$path'");
		}
		return $zip;
	}

	# We don't support anything else
	Carp::croak('Failed to load unsupported archive type '
		. $self->source);
}

sub extract_libs {
	my $self = shift;
	$self->extract_resource('lib', @_ );
}

sub extract_resource {
	my $self     = shift;
	my $resource = shift
		or Carp::croak("No resource name provided to _extract_resource");
	my %params   = @_;

	# Check the extraction destination
	$params{to} ||= File::Spec->curdir;
	unless ( -d $params{to} ) {
		Carp::croak("Extraction directory '$params{to}' does not exist");
	}
	unless ( -w $params{to} ) {
		Carp::croak("No permissions to write to extraction directory '$params{to}'");
	}

	# Split on archive type
	if ( $self->archive->isa('Archive::Tar') ) {
		return $self->_extract_resource_from_tar($resource, @_);
	}
	if ( $self->archive->isa('Archive::Zip') ) {
		return $self->_extract_resource_from_zip($resource, @_);
	}
	Carp::croak("Unsupported archive type " . ref($self->archive));
}

sub _extract_resource_from_tar {
	my ($self, $resource, %params) = @_;
	my $tar   = $self->archive;
	my @files = $tar->get_files;

	# Determine which files to extract, and to where
	my $extracted_files = 0;
	foreach my $item ( @files ) {
		next unless $item->is_file;

		# Split into parts and remove the top level dir
		my ($vol, $dir, $file)
			= File::Spec::Unix->splitpath($item->name);
		my @dirs = File::Spec::Unix->splitdir($dir);
		shift @dirs;

		# Is this file in the resource directory
		my $res = shift(@dirs) or next;
		next unless $res eq $resource;

		# These are STILL relative, but we'll deal with that later.
		my $write_dir = File::Spec->catfile($params{to}, @dirs);

		# Write the file
		$self->_write( $write_dir, $file, $item->get_content );
		$extracted_files++;
	}

	# Return the number of files, or error if none
	return $extracted_files if $extracted_files;
	my $path = $self->source;
	Carp::croak("Tarball '$path' does not contain resource '$resource'");
}

sub _extract_resource_from_zip {
	Carp::croak("Zip support not yet completed");
}

# Utilty method
sub _write {
	my ($self, $dir, $file, $content) = @_;

	# Localise newlines in the files
	$content =~ s/(\015{1,2}\012|\015|\012)/\n/g;

	# Create the save directory if needed
	File::Path::mkpath( $dir, 0, 0755 ) unless -d $dir;

	# Save it
	my $path = File::Spec->catfile( $dir, $file );
	open( LIBRARY, '>', $path ) 
		or Carp::croak( "Failed to open '$path' for writing: $!" );
	print LIBRARY $content
		or Carp::croak( "Failed to write to '$path'" );
	close LIBRARY
		or Carp::croak( "Failed to close '$path' after writing" );

	1;
}





#####################################################################
# Algorithm::Dependency::Ordered for the ::Release data

package JSAN::Index::Release::_Dependency;

use base 'Algorithm::Dependency::Ordered';

use vars qw{$VERSION};
BEGIN {
	$VERSION = '0.08';
}

sub new {
	my $class  = ref $_[0] ? ref shift : shift;
	my %params = @_;

	# Apply defaults
	$params{source} ||= JSAN::Index::Release::_Source->new( %params );

	# Hand off to superclass constructor
	my $self = $class->SUPER::new( %params )
		or Carp::croak("Failed to create JSAN::Index::Release::_Dependency object");

	# Save the type for later
	$self->{build} = !! $params{build};

	$self;
}

sub build { $_[0]->{build} }

sub schedule {
	my $self     = shift;
	my @schedule = @_;

	# Convert things in the schedule from index objects to
	# release source strings as needed
	my @cleaned = ();
	foreach my $item ( @schedule ) {
		if ( defined $item and ! ref $item and $item =~ /^(?:\w+)(?:\.\w+)*$/ ) {
			$item = JSAN::Index::Library->retrieve( name => $item );
		}
		if ( UNIVERSAL::isa($item, 'JSAN::Index::Library') ) {
			$item = $item->release;
		}
		if ( UNIVERSAL::isa($item, 'JSAN::Index::Release') ) {
			$item = $item->source;
		}
		push @cleaned, $item;
	}

	$self->SUPER::schedule(@cleaned);
}





#####################################################################
# Algorithm::Dependency::Source for the ::Release data

package JSAN::Index::Release::_Source;

use Algorithm::Dependency::Item ();
use base 'Algorithm::Dependency::Source';

use vars qw{$VERSION};
BEGIN {
	$VERSION = '0.08';
}

sub new {
	my $class  = ref $_[0] ? ref shift : shift;
	my %params = @_;

	# Create the basic object
	my $self = $class->SUPER::new();

	# Set the methods to use
	$self->{requires_releases} = 1;
	if ( $params{build} ) {
		$self->{build_requires_releases} = 1;
	}

	$self;
}

sub _load_item_list {
	my $self = shift;

	### FIXME: This is crudely effective, but a little innefficient.
	###        Later, we should be able to determine which subset of
	###        these can never be called, and leave them out of the list.

	# Get every single release in the index
	my @releases = JSAN::Index::Release->retrieve_all;

	# Wrap the releases in the Adapter objects
	my @items  = ();
	foreach my $release ( @releases ) {
		my $id      = $release->source;

		# Get the list of dependencies
		my @depends = ();
		if ( $self->{requires_releases} ) {
			push @depends, $release->requires_releases;
		}
		if ( $self->{build_requires_releases} ) {
			push @depends, $release->build_requires_releases;
		}

		# Convert to a distinct source list
		my %seen = ();
		@depends = grep { ! $seen{$_} } map { $_->source } @depends;

		# Add the dependency
		my $item = Algorithm::Dependency::Item->new( $id => @depends )
			or die "Failed to create Algorithm::Dependency::Item";
		push @items, $item;
	}

	\@items;
}

1;

=pod

=head1 TO DO

- Add the testing dependency algorithm variant

- Add support for JSON META.yml files

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
