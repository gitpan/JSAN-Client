package JSAN::Index::Library;

use 5.008005;
use strict;
use warnings;
use Carp ();

our $VERSION = '0.29';

sub distribution {
    JSAN::Index::Distribution->retrieve(
        name => $_[0]->{distribution},
    );
}

sub release {
    JSAN::Index::Release->retrieve(
        id => $_[0]->{release},
    );
}

sub retrieve {
    my $class  = shift;
    my %params = @_;
    my $sql    = join " and ", map { "$_ = ?" } keys(%params); 
    my @result = $class->select( "where $sql", values(%params) );
    if ( @result == 1 ) {
        return $result[0];
    }
    if ( @result > 1 ) {
        Carp::croak("Found more than one author record");
    } else {
        return undef;
    }
}

sub extract_libs {
    my $self = shift;
    $self->extract_resource('lib', @_);
}

sub extract_tests {
    my $self = shift;
    $self->extract_resource('tests', @_);
}

sub extract_resource {
    shift->release->extract_resource(@_);
}

sub search {
    my $class  = shift;
    my %params = @_;
    my $sql    = join " and ", map { "$_ = ?" } keys(%params); 
    my @result = $class->select( "where $sql", values(%params) );
    return @result
}

sub search_like {
    my $class  = shift;
    my %params = @_;
    my $sql    = join " and ", map { "$_ like ?" } keys(%params); 
    my @result = $class->select( "where $sql", values(%params) );
    return @result
}





######################################################################
# Generated by ORLite 1.25 (Unused parts are commented out)

#sub base { 'JSAN::Index' }
#
#sub table { 'library' }

sub select {
    my $class = shift;
    my $sql   = 'select "name", "distribution", "release", "version", "doc" from library ';
       $sql  .= shift if @_;
    my $rows  = JSAN::Index->selectall_arrayref( $sql, { Slice => {} }, @_ );
    bless( $_, 'JSAN::Index::Library' ) foreach @$rows;
    wantarray ? @$rows : $rows;
}

#sub count {
#    my $class = shift;
#    my $sql   = 'select count(*) from library ';
#       $sql  .= shift if @_;
#    JSAN::Index->selectrow_array( $sql, {}, @_ );
#}
#
#sub iterate {
#    my $class = shift;
#    my $call  = pop;
#    my $sql   = 'select "name", "distribution", "release", "version", "doc" from library ';
#       $sql  .= shift if @_;
#    my $sth   = JSAN::Index->prepare( $sql );
#    $sth->execute( @_ );
#    while ( $_ = $sth->fetchrow_hashref ) {
#        bless( $_, 'JSAN::Index::Library' );
#        $call->() or last;
#    }
#    $sth->finish;
#}

sub name {
    $_[0]->{name};
}

#sub distribution {
#    $_[0]->{distribution};
#}
#
#sub release {
#    $_[0]->{release};
#}

sub version {
    $_[0]->{version};
}

sub doc {
    $_[0]->{doc};
}

1;

__END__

=pod

=head1 NAME

JSAN::Index::Library - A JavaScript Archive Network (JSAN) Software Library

=head1 DESCRIPTION

This class provides objects for the various libraries (software components)
in the JSAN.

=head1 METHODS

In addition to the general methods provided by L<ORLite>, it has the
following methods

=head2 name

The C<name> accessor returns the name (possibly including the use of
pseudo-namespaces) of the library. e.g. "Test.Simple.Container.Browser"

=head2 release

The C<release> method returns the L<JSAN::Index::Release> object for the
release that the library is defined in.

=head2 version

The C<version> accessor returns the version of the library.

=head2 doc

The C<doc> accessor returns the root-relative location of the documentation
for this library on the L<http://openjsan.org/> website.

=head2 distribution

The C<distribution> method is a shortcut for
C<$library-E<gt>release-E<gt>distribution> and returns the
L<JSAN::Index::Distribution> for the distribution that this library
is of.

=head2 extract_libs to => $path

The C<extract_libs> method will extract the libraries for a release
(i.e. the contents of the C<lib> directory> to the local filesystem.

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

Returns the number of files extracted, or dies on error.

=head2 extract_tests to => $path

The C<extract_tests> method will extract the test scripts for a release
(i.e. the contents of the C<tests> directory> to the local filesystem.

Returns the number of files extracted, or dies on error.

=head1 SUPPORT

Bugs should be reported via the CPAN bug tracker at

L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=JSAN-Client>

For other issues, contact the author.

=head1 AUTHOR

Adam Kennedy E<lt>adamk@cpan.orgE<gt>

=head1 SEE ALSO

L<JSAN::Index>, L<JSAN::Shell>, L<http://openjsan.org>

=head1 COPYRIGHT

Copyright 2005 - 2007 Adam Kennedy.

This program is free software; you can redistribute
it and/or modify it under the same terms as Perl itself.

The full text of the license can be found in the
LICENSE file included with this module.

=cut


=pod

=head1 NAME

JSAN::Index::Library - JSAN::Index class for the library table

=head1 SYNOPSIS

  TO BE COMPLETED

=head1 DESCRIPTION

TO BE COMPLETED

=head1 METHODS

=head2 select

  # Get all objects in list context
  my @list = JSAN::Index::Library->select;
  
  # Get a subset of objects in scalar context
  my $array_ref = JSAN::Index::Library->select(
      'where name > ? order by name',
      1000,
  );

The C<select> method executes a typical SQL C<SELECT> query on the
library table.

It takes an optional argument of a SQL phrase to be added after the
C<FROM library> section of the query, followed by variables
to be bound to the placeholders in the SQL phrase. Any SQL that is
compatible with SQLite can be used in the parameter.

Returns a list of B<JSAN::Index::Library> objects when called in list context, or a
reference to an C<ARRAY> of B<JSAN::Index::Library> objects when called in scalar
 context.

Throws an exception on error, typically directly from the L<DBI> layer.

=head2 count

  # How many objects are in the table
  my $rows = JSAN::Index::Library->count;
  
  # How many objects 
  my $small = JSAN::Index::Library->count(
      'where name > ?',
      1000,
  );

The C<count> method executes a C<SELECT COUNT(*)> query on the
library table.

It takes an optional argument of a SQL phrase to be added after the
C<FROM library> section of the query, followed by variables
to be bound to the placeholders in the SQL phrase. Any SQL that is
compatible with SQLite can be used in the parameter.

Returns the number of objects that match the condition.

Throws an exception on error, typically directly from the L<DBI> layer.

=head1 ACCESSORS

=head2 name

  if ( $object->name ) {
      print "Object has been inserted\n";
  } else {
      print "Object has not been inserted\n";
  }

Returns true, or throws an exception on error.


REMAINING ACCESSORS TO BE COMPLETED

=head1 SQL

The library table was originally created with the
following SQL command.

  CREATE TABLE library (
      name varchar (
          100
      )
      NOT NULL,
      distribution varchar (
          100
      )
      NOT NULL,
      release int (
          11
      )
      NOT NULL,
      version varchar (
          100
      )
  ,
      doc varchar (
          100
      )
      NOT NULL,
      PRIMARY KEY (
          name
      )
  )


=head1 SUPPORT

JSAN::Index::Library is part of the L<JSAN::Index> API.

See the documentation for L<JSAN::Index> for more information.

=head1 COPYRIGHT

Copyright 2009 - 2010 Adam Kennedy.

This program is free software; you can redistribute
it and/or modify it under the same terms as Perl itself.

The full text of the license can be found in the
LICENSE file included with this module.

=cut
