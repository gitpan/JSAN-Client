package JSAN::Index::Author;

use 5.006;
use strict;
use warnings;
use JSAN::Index::Release ();

our $VERSION = '0.27';

sub releases {
    JSAN::Index::Release->select( 'where author = ?', $_[0]->login );
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
#sub table { 'author' }

sub select {
    my $class = shift;
    my $sql   = 'select "login", "name", "doc", "email", "url" from author ';
       $sql  .= shift if @_;
    my $rows  = JSAN::Index->selectall_arrayref( $sql, { Slice => {} }, @_ );
    bless( $_, 'JSAN::Index::Author' ) foreach @$rows;
    wantarray ? @$rows : $rows;
}

sub count {
    my $class = shift;
    my $sql   = 'select count(*) from author ';
       $sql  .= shift if @_;
    JSAN::Index->selectrow_array( $sql, {}, @_ );
}

sub iterate {
    my $class = shift;
    my $call  = pop;
    my $sql   = 'select "login", "name", "doc", "email", "url" from author ';
       $sql  .= shift if @_;
    my $sth   = JSAN::Index->prepare( $sql );
    $sth->execute( @_ );
    while ( $_ = $sth->fetchrow_hashref ) {
        bless( $_, 'JSAN::Index::Author' );
        $call->() or last;
    }
    $sth->finish;
}

sub login {
    $_[0]->{login};
}

sub name {
    $_[0]->{name};
}

sub doc {
    $_[0]->{doc};
}

sub email {
    $_[0]->{email};
}

sub url {
    $_[0]->{url};
}

1;

__END__;

=pod

=head1 NAME

JSAN::Index::Author - A JavaScript Archive Network (JSAN) Author

=head1 DESCRIPTION

This class provides objects that represent authors in the L<JSAN::Index>.

=head1 METHODS

In addition to the general methods provided by L<ORLite>, this class has
the following additional methods.

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

Returns a list of L<JSAN::Index::Release> objects.

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

JSAN::Index::Author - JSAN::Index class for the author table

=head1 SYNOPSIS

  TO BE COMPLETED

=head1 DESCRIPTION

TO BE COMPLETED

=head1 METHODS

=head2 select

  # Get all objects in list context
  my @list = JSAN::Index::Author->select;
  
  # Get a subset of objects in scalar context
  my $array_ref = JSAN::Index::Author->select(
      'where login > ? order by login',
      1000,
  );

The C<select> method executes a typical SQL C<SELECT> query on the
author table.

It takes an optional argument of a SQL phrase to be added after the
C<FROM author> section of the query, followed by variables
to be bound to the placeholders in the SQL phrase. Any SQL that is
compatible with SQLite can be used in the parameter.

Returns a list of B<JSAN::Index::Author> objects when called in list context, or a
reference to an C<ARRAY> of B<JSAN::Index::Author> objects when called in scalar
 context.

Throws an exception on error, typically directly from the L<DBI> layer.

=head2 count

  # How many objects are in the table
  my $rows = JSAN::Index::Author->count;
  
  # How many objects 
  my $small = JSAN::Index::Author->count(
      'where login > ?',
      1000,
  );

The C<count> method executes a C<SELECT COUNT(*)> query on the
author table.

It takes an optional argument of a SQL phrase to be added after the
C<FROM author> section of the query, followed by variables
to be bound to the placeholders in the SQL phrase. Any SQL that is
compatible with SQLite can be used in the parameter.

Returns the number of objects that match the condition.

Throws an exception on error, typically directly from the L<DBI> layer.

=head1 ACCESSORS

=head2 login

  if ( $object->login ) {
      print "Object has been inserted\n";
  } else {
      print "Object has not been inserted\n";
  }

Returns true, or throws an exception on error.


REMAINING ACCESSORS TO BE COMPLETED

=head1 SQL

The author table was originally created with the
following SQL command.

  CREATE TABLE author (
      login varchar (
          100
      )
      NOT NULL,
      name varchar (
          100
      )
      NOT NULL,
      doc varchar (
          100
      )
      NOT NULL,
      email varchar (
          100
      )
      NOT NULL,
      url varchar (
          100
      )
  ,
      PRIMARY KEY (
          login
      )
  )


=head1 SUPPORT

JSAN::Index::Author is part of the L<JSAN::Index> API.

See the documentation for L<JSAN::Index> for more information.

=head1 COPYRIGHT

Copyright 2009 - 2010 Adam Kennedy.

This program is free software; you can redistribute
it and/or modify it under the same terms as Perl itself.

The full text of the license can be found in the
LICENSE file included with this module.

