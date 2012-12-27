
package ArkTengXslate;
use strict;
use warnings;
use base 'Module::Setup::Flavor';
1;

=head1

ArkTengXslate - pack from ark-teng-xslate

=head1 SYNOPSIS

  ArkTengXslate-setup --init --flavor-class=+ArkTengXslate new_flavor

=cut

__DATA__

---
file: .gitignore
template: |
  cover_db
  META.yml
  MYMETA*
  Makefile
  blib
  inc
  pm_to_blib
  MANIFEST
  Makefile.old
  nytprof.out
  MANIFEST.bak
  *.sw[po]
  .#*
  ~*
  config_local.pl
  tmp/
---
file: .shipit
template: "steps = FindVersion, ChangeVersion, CheckChangeLog, DistTest, Commit, Tag, MakeDist, UploadCPAN\n"
---
file: Changes
template: |
  Revision history for Perl extension [% module %]

  0.01    [% localtime %]
          - original version
---
file: config.pl
template: |
  +{
      default_view => 'Xslate',

      database => {
          master => [
              'dbi:mysql:host=127.0.0.1;port=3306;database=dbname', 'root', '', {
                  on_connect_do     => [ 'SET NAMES utf8' ],
                  mysql_enable_utf8 => 1,
              },
          ],
          slave => [
              'dbi:mysql:host=127.0.0.1;port=3306;database=dbname', 'root', '', {
                  on_connect_do     => [ 'SET NAMES utf8' ],
                  mysql_enable_utf8 => 1,
              },
          ],
      },
  }
---
file: config_local.pl
template: "+{}\n"
---
file: dev.psgi
template: |
  use Plack::Builder;
  use Plack::Middleware::Static;
  use lib 'lib';
  use [% module %];

  my $app = [% module %]->new;
  $app->setup;

  builder {
      enable 'Plack::Middleware::Static',
          path => qr{^/(js/|css/|swf/|images?/|imgs?/|static/|[^/]+\.[^/]+$)},
          root => $app->path_to('root')->stringify;
      $app->handler;
  };
---
file: Makefile.PL
template: |
  use inc::Module::Install;

  name '[% module %]';
  all_from 'lib/[% module %].pm';

  requires 'Path::AttrRouter';
  requires 'Ark';

  requires 'Class::Method::Modifiers';
  requires 'DateTime';
  requires 'FindBin::libs';
  requires 'SQL::Translator';
  requires 'String::CamelCase';
  requires 'Teng';
  requires 'Text::Xslate';

  tests 't/*.t';
  author_tests 'xt';

  auto_set_repository;
  auto_include;

  WriteAll;
---
file: MANIFEST.SKIP
template: |
  \bRCS\b
  \bCVS\b
  ^MANIFEST\.
  ^Makefile$
  ~$
  ^#
  \.old$
  ^blib/
  ^pm_to_blib
  ^MakeMaker-\d
  \.gz$
  \.cvsignore
  ^t/9\d_.*\.t
  ^t/perlcritic
  ^tools/
  \.svn/
  ^[^/]+\.yaml$
  ^[^/]+\.pl$
  ^\.shipit$
  ^\.git/
  \.sw[po]$
---
file: prod.psgi
template: |
  use lib 'lib';

  use Plack::Builder;
  use [% module %];
  use [% module %]::Models;

  my $app = [% module %]->new;
  $app->setup;

  # preload models
  my $models = [% module %]::Models->instance;
  $models->load_all;

  builder {
      $app->handler;
  };
---
file: README
template: |
  This is Perl module [% module %].

  INSTALLATION

  [% module %] installation is straightforward. If your CPAN shell is set up,
  you should just be able to do

      % cpan [% module %]

  Download it, unpack it, then build it as per the usual:

      % perl Makefile.PL
      % make && make test

  Then install it:

      % make install

  DOCUMENTATION

  [% module %] documentation is available as in POD. So you can do:

      % perldoc [% module %]

  to read the documentation online with your favorite pager.

  [% config.author %]
---
dir: inc/.author
---
file: inc/Module/Install.pm
template: |
  #line 1
  package Module::Install;

  # For any maintainers:
  # The load order for Module::Install is a bit magic.
  # It goes something like this...
  #
  # IF ( host has Module::Install installed, creating author mode ) {
  #     1. Makefile.PL calls "use inc::Module::Install"
  #     2. $INC{inc/Module/Install.pm} set to installed version of inc::Module::Install
  #     3. The installed version of inc::Module::Install loads
  #     4. inc::Module::Install calls "require Module::Install"
  #     5. The ./inc/ version of Module::Install loads
  # } ELSE {
  #     1. Makefile.PL calls "use inc::Module::Install"
  #     2. $INC{inc/Module/Install.pm} set to ./inc/ version of Module::Install
  #     3. The ./inc/ version of Module::Install loads
  # }

  use 5.005;
  use strict 'vars';
  use Cwd        ();
  use File::Find ();
  use File::Path ();

  use vars qw{$VERSION $MAIN};
  BEGIN {
    # All Module::Install core packages now require synchronised versions.
    # This will be used to ensure we don't accidentally load old or
    # different versions of modules.
    # This is not enforced yet, but will be some time in the next few
    # releases once we can make sure it won't clash with custom
    # Module::Install extensions.
    $VERSION = '1.06';

    # Storage for the pseudo-singleton
    $MAIN    = undef;

    *inc::Module::Install::VERSION = *VERSION;
    @inc::Module::Install::ISA     = __PACKAGE__;

  }

  sub import {
    my $class = shift;
    my $self  = $class->new(@_);
    my $who   = $self->_caller;

    #-------------------------------------------------------------
    # all of the following checks should be included in import(),
    # to allow "eval 'require Module::Install; 1' to test
    # installation of Module::Install. (RT #51267)
    #-------------------------------------------------------------

    # Whether or not inc::Module::Install is actually loaded, the
    # $INC{inc/Module/Install.pm} is what will still get set as long as
    # the caller loaded module this in the documented manner.
    # If not set, the caller may NOT have loaded the bundled version, and thus
    # they may not have a MI version that works with the Makefile.PL. This would
    # result in false errors or unexpected behaviour. And we don't want that.
    my $file = join( '/', 'inc', split /::/, __PACKAGE__ ) . '.pm';
    unless ( $INC{$file} ) { die <<"END_DIE" }

  Please invoke ${\__PACKAGE__} with:

    use inc::${\__PACKAGE__};

  not:

    use ${\__PACKAGE__};

  END_DIE

    # This reportedly fixes a rare Win32 UTC file time issue, but
    # as this is a non-cross-platform XS module not in the core,
    # we shouldn't really depend on it. See RT #24194 for detail.
    # (Also, this module only supports Perl 5.6 and above).
    eval "use Win32::UTCFileTime" if $^O eq 'MSWin32' && $] >= 5.006;

    # If the script that is loading Module::Install is from the future,
    # then make will detect this and cause it to re-run over and over
    # again. This is bad. Rather than taking action to touch it (which
    # is unreliable on some platforms and requires write permissions)
    # for now we should catch this and refuse to run.
    if ( -f $0 ) {
        my $s = (stat($0))[9];

        # If the modification time is only slightly in the future,
        # sleep briefly to remove the problem.
        my $a = $s - time;
        if ( $a > 0 and $a < 5 ) { sleep 5 }

        # Too far in the future, throw an error.
        my $t = time;
        if ( $s > $t ) { die <<"END_DIE" }

  Your installer $0 has a modification time in the future ($s > $t).

  This is known to create infinite loops in make.

  Please correct this, then run $0 again.

  END_DIE
    }


    # Build.PL was formerly supported, but no longer is due to excessive
    # difficulty in implementing every single feature twice.
    if ( $0 =~ /Build.PL$/i ) { die <<"END_DIE" }

  Module::Install no longer supports Build.PL.

  It was impossible to maintain duel backends, and has been deprecated.

  Please remove all Build.PL files and only use the Makefile.PL installer.

  END_DIE

    #-------------------------------------------------------------

    # To save some more typing in Module::Install installers, every...
    # use inc::Module::Install
    # ...also acts as an implicit use strict.
    $^H |= strict::bits(qw(refs subs vars));

    #-------------------------------------------------------------

    unless ( -f $self->{file} ) {
        foreach my $key (keys %INC) {
            delete $INC{$key} if $key =~ /Module\/Install/;
        }

        local $^W;
        require "$self->{path}/$self->{dispatch}.pm";
        File::Path::mkpath("$self->{prefix}/$self->{author}");
        $self->{admin} = "$self->{name}::$self->{dispatch}"->new( _top => $self );
        $self->{admin}->init;
        @_ = ($class, _self => $self);
        goto &{"$self->{name}::import"};
    }

    local $^W;
    *{"${who}::AUTOLOAD"} = $self->autoload;
    $self->preload;

    # Unregister loader and worker packages so subdirs can use them again
    delete $INC{'inc/Module/Install.pm'};
    delete $INC{'Module/Install.pm'};

    # Save to the singleton
    $MAIN = $self;

    return 1;
  }

  sub autoload {
    my $self = shift;
    my $who  = $self->_caller;
    my $cwd  = Cwd::cwd();
    my $sym  = "${who}::AUTOLOAD";
    $sym->{$cwd} = sub {
        my $pwd = Cwd::cwd();
        if ( my $code = $sym->{$pwd} ) {
            # Delegate back to parent dirs
            goto &$code unless $cwd eq $pwd;
        }
        unless ($$sym =~ s/([^:]+)$//) {
            # XXX: it looks like we can't retrieve the missing function
            # via $$sym (usually $main::AUTOLOAD) in this case.
            # I'm still wondering if we should slurp Makefile.PL to
            # get some context or not ...
            my ($package, $file, $line) = caller;
            die <<"EOT";
  Unknown function is found at $file line $line.
  Execution of $file aborted due to runtime errors.

  If you're a contributor to a project, you may need to install
  some Module::Install extensions from CPAN (or other repository).
  If you're a user of a module, please contact the author.
  EOT
        }
        my $method = $1;
        if ( uc($method) eq $method ) {
            # Do nothing
            return;
        } elsif ( $method =~ /^_/ and $self->can($method) ) {
            # Dispatch to the root M:I class
            return $self->$method(@_);
        }

        # Dispatch to the appropriate plugin
        unshift @_, ( $self, $1 );
        goto &{$self->can('call')};
    };
  }

  sub preload {
    my $self = shift;
    unless ( $self->{extensions} ) {
        $self->load_extensions(
            "$self->{prefix}/$self->{path}", $self
        );
    }

    my @exts = @{$self->{extensions}};
    unless ( @exts ) {
        @exts = $self->{admin}->load_all_extensions;
    }

    my %seen;
    foreach my $obj ( @exts ) {
        while (my ($method, $glob) = each %{ref($obj) . '::'}) {
            next unless $obj->can($method);
            next if $method =~ /^_/;
            next if $method eq uc($method);
            $seen{$method}++;
        }
    }

    my $who = $self->_caller;
    foreach my $name ( sort keys %seen ) {
        local $^W;
        *{"${who}::$name"} = sub {
            ${"${who}::AUTOLOAD"} = "${who}::$name";
            goto &{"${who}::AUTOLOAD"};
        };
    }
  }

  sub new {
    my ($class, %args) = @_;

    delete $INC{'FindBin.pm'};
    {
        # to suppress the redefine warning
        local $SIG{__WARN__} = sub {};
        require FindBin;
    }

    # ignore the prefix on extension modules built from top level.
    my $base_path = Cwd::abs_path($FindBin::Bin);
    unless ( Cwd::abs_path(Cwd::cwd()) eq $base_path ) {
        delete $args{prefix};
    }
    return $args{_self} if $args{_self};

    $args{dispatch} ||= 'Admin';
    $args{prefix}   ||= 'inc';
    $args{author}   ||= ($^O eq 'VMS' ? '_author' : '.author');
    $args{bundle}   ||= 'inc/BUNDLES';
    $args{base}     ||= $base_path;
    $class =~ s/^\Q$args{prefix}\E:://;
    $args{name}     ||= $class;
    $args{version}  ||= $class->VERSION;
    unless ( $args{path} ) {
        $args{path}  = $args{name};
        $args{path}  =~ s!::!/!g;
    }
    $args{file}     ||= "$args{base}/$args{prefix}/$args{path}.pm";
    $args{wrote}      = 0;

    bless( \%args, $class );
  }

  sub call {
    my ($self, $method) = @_;
    my $obj = $self->load($method) or return;
          splice(@_, 0, 2, $obj);
    goto &{$obj->can($method)};
  }

  sub load {
    my ($self, $method) = @_;

    $self->load_extensions(
        "$self->{prefix}/$self->{path}", $self
    ) unless $self->{extensions};

    foreach my $obj (@{$self->{extensions}}) {
        return $obj if $obj->can($method);
    }

    my $admin = $self->{admin} or die <<"END_DIE";
  The '$method' method does not exist in the '$self->{prefix}' path!
  Please remove the '$self->{prefix}' directory and run $0 again to load it.
  END_DIE

    my $obj = $admin->load($method, 1);
    push @{$self->{extensions}}, $obj;

    $obj;
  }

  sub load_extensions {
    my ($self, $path, $top) = @_;

    my $should_reload = 0;
    unless ( grep { ! ref $_ and lc $_ eq lc $self->{prefix} } @INC ) {
        unshift @INC, $self->{prefix};
        $should_reload = 1;
    }

    foreach my $rv ( $self->find_extensions($path) ) {
        my ($file, $pkg) = @{$rv};
        next if $self->{pathnames}{$pkg};

        local $@;
        my $new = eval { local $^W; require $file; $pkg->can('new') };
        unless ( $new ) {
            warn $@ if $@;
            next;
        }
        $self->{pathnames}{$pkg} =
            $should_reload ? delete $INC{$file} : $INC{$file};
        push @{$self->{extensions}}, &{$new}($pkg, _top => $top );
    }

    $self->{extensions} ||= [];
  }

  sub find_extensions {
    my ($self, $path) = @_;

    my @found;
    File::Find::find( sub {
        my $file = $File::Find::name;
        return unless $file =~ m!^\Q$path\E/(.+)\.pm\Z!is;
        my $subpath = $1;
        return if lc($subpath) eq lc($self->{dispatch});

        $file = "$self->{path}/$subpath.pm";
        my $pkg = "$self->{name}::$subpath";
        $pkg =~ s!/!::!g;

        # If we have a mixed-case package name, assume case has been preserved
        # correctly.  Otherwise, root through the file to locate the case-preserved
        # version of the package name.
        if ( $subpath eq lc($subpath) || $subpath eq uc($subpath) ) {
            my $content = Module::Install::_read($subpath . '.pm');
            my $in_pod  = 0;
            foreach ( split //, $content ) {
                $in_pod = 1 if /^=\w/;
                $in_pod = 0 if /^=cut/;
                next if ($in_pod || /^=cut/);  # skip pod text
                next if /^\s*#/;               # and comments
                if ( m/^\s*package\s+($pkg)\s*;/i ) {
                    $pkg = $1;
                    last;
                }
            }
        }

        push @found, [ $file, $pkg ];
    }, $path ) if -d $path;

    @found;
  }





  #####################################################################
  # Common Utility Functions

  sub _caller {
    my $depth = 0;
    my $call  = caller($depth);
    while ( $call eq __PACKAGE__ ) {
        $depth++;
        $call = caller($depth);
    }
    return $call;
  }

  # Done in evals to avoid confusing Perl::MinimumVersion
  eval( $] >= 5.006 ? <<'END_NEW' : <<'END_OLD' ); die $@ if $@;
  sub _read {
    local *FH;
    open( FH, '<', $_[0] ) or die "open($_[0]): $!";
    my $string = do { local $/; <FH> };
    close FH or die "close($_[0]): $!";
    return $string;
  }
  END_NEW
  sub _read {
    local *FH;
    open( FH, "< $_[0]"  ) or die "open($_[0]): $!";
    my $string = do { local $/; <FH> };
    close FH or die "close($_[0]): $!";
    return $string;
  }
  END_OLD

  sub _readperl {
    my $string = Module::Install::_read($_[0]);
    $string =~ s/(?:\015{1,2}\012|\015|\012)/\n/sg;
    $string =~ s/(\n)\n*__(?:DATA|END)__\b.*\z/$1/s;
    $string =~ s/\n\n=\w+.+?\n\n=cut\b.+?\n+/\n\n/sg;
    return $string;
  }

  sub _readpod {
    my $string = Module::Install::_read($_[0]);
    $string =~ s/(?:\015{1,2}\012|\015|\012)/\n/sg;
    return $string if $_[0] =~ /\.pod\z/;
    $string =~ s/(^|\n=cut\b.+?\n+)[^=\s].+?\n(\n=\w+|\z)/$1$2/sg;
    $string =~ s/\n*=pod\b[^\n]*\n+/\n\n/sg;
    $string =~ s/\n*=cut\b[^\n]*\n+/\n\n/sg;
    $string =~ s/^\n+//s;
    return $string;
  }

  # Done in evals to avoid confusing Perl::MinimumVersion
  eval( $] >= 5.006 ? <<'END_NEW' : <<'END_OLD' ); die $@ if $@;
  sub _write {
    local *FH;
    open( FH, '>', $_[0] ) or die "open($_[0]): $!";
    foreach ( 1 .. $#_ ) {
        print FH $_[$_] or die "print($_[0]): $!";
    }
    close FH or die "close($_[0]): $!";
  }
  END_NEW
  sub _write {
    local *FH;
    open( FH, "> $_[0]"  ) or die "open($_[0]): $!";
    foreach ( 1 .. $#_ ) {
        print FH $_[$_] or die "print($_[0]): $!";
    }
    close FH or die "close($_[0]): $!";
  }
  END_OLD

  # _version is for processing module versions (eg, 1.03_05) not
  # Perl versions (eg, 5.8.1).
  sub _version ($) {
    my $s = shift || 0;
    my $d =()= $s =~ /(\.)/g;
    if ( $d >= 2 ) {
        # Normalise multipart versions
        $s =~ s/(\.)(\d{1,3})/sprintf("$1%03d",$2)/eg;
    }
    $s =~ s/^(\d+)\.?//;
    my $l = $1 || 0;
    my @v = map {
        $_ . '0' x (3 - length $_)
    } $s =~ /(\d{1,3})\D?/g;
    $l = $l . '.' . join '', @v if @v;
    return $l + 0;
  }

  sub _cmp ($$) {
    _version($_[1]) <=> _version($_[2]);
  }

  # Cloned from Params::Util::_CLASS
  sub _CLASS ($) {
    (
        defined $_[0]
        and
        ! ref $_[0]
        and
        $_[0] =~ m/^[^\W\d]\w*(?:::\w+)*\z/s
    ) ? $_[0] : undef;
  }

  1;

  # Copyright 2008 - 2012 Adam Kennedy.
---
file: lib/____var-module_path-var____.pm
template: |
  package [% module %];
  use Ark;

  use_model '[% module %]::Models';
  our $VERSION = '0.01';

  __PACKAGE__->meta->make_immutable;

  __END__

  =head1 NAME

  [% module %] -

  =head1 SYNOPSIS

  use [% module %];

  =head1 DESCRIPTION

  [% module %] is

  =head1 AUTHOR

  [% config.author %] E<lt>[% config.email %]E<gt>

  =head1 SEE ALSO

  =head1 LICENSE

  This library is free software; you can redistribute it and/or modify
  it under the same terms as Perl itself.

  =cut
---
file: lib/____var-module_path-var____/Controller.pm
template: |
  package [% module %]::Controller;
  use Ark 'Controller';
  use [% module %]::Models;

  # default 404 handler
  sub default :Path :Args {
      my ($self, $c) = @_;

      $c->res->status(404);
      $c->res->body('404 Not Found');
  }

  sub index :Path :Args(0) {
      my ($self, $c) = @_;
      $c->stash->{title} = 'Ark Default Index by Xslate';
  }

  sub end :Private {
      my ($self, $c) = @_;

      unless ($c->res->body || ($c->res->status == 302)) {
          $c->forward($c->view('Xslate'));
      }

      unless ($c->res->content_type()) {
          $c->res->header('content-type' => 'text/html')
      }
  }

  __PACKAGE__->meta->make_immutable;
---
file: lib/____var-module_path-var____/DB.pm
template: |
  package [% module %]::DB;

  use strict;
  use warnings;
  use utf8;

  use parent 'Teng';
  __PACKAGE__->load_plugin('Pager::MySQLFoundRows');
  __PACKAGE__->load_plugin('Count');
  __PACKAGE__->load_plugin('FindOrCreate');

  1;
---
file: lib/____var-module_path-var____/Models.pm
template: |
  package [% module %]::Models;
  use strict;
  use warnings;
  use Ark::Models '-base';
  use Module::Find;
  use Teng::Schema::Loader;
  use String::CamelCase qw/decamelize/;

  use constant PROJECT_NAME => (split /::/, __PACKAGE__)[0];

  register db => sub {
      my $self = shift;

      my $cls = PROJECT_NAME."::DB";
      $self->ensure_class_loaded($cls);

      my $conf = $self->get('conf')->{database}{master}
          or die 'Require database config';

      my $dbh = DBI->connect(@{$conf})
          or die 'cannot connect database';

      my $loader = Teng::Schema::Loader->load(
          dbh       => $dbh,
          namespace => PROJECT_NAME.'::DB'
      );

      $cls->new(
          dbh           => $dbh,
          schema        => $loader->schema,
          slave_methods => [qw/count search_with_pager/],
      );
  };

  autoloader qr/^DB::/ => sub {
      my ($self, $name) = @_;

      my @modules = Module::Find::findallmod(PROJECT_NAME.'::DB::Table');
      my %module_by_name = map {
          (split /::/)[-1] => $_;
      } @modules;

      for my $source (keys %module_by_name) {
          register "DB::${source}" => sub {
              my $class = PROJECT_NAME."::DB::Table::${source}";
              shift->ensure_class_loaded($class);
              $class->new({ table_name => decamelize($source) });
          };
      }
  };

  1;
---
file: lib/____var-module_path-var____/Test.pm
template: |
  package [% module %]::Test;

  use strict;
  use warnings;
  use utf8;

  use Ark 'Test';

  use Class::Method::Modifiers;
  use File::Basename qw/basename/;
  use Path::Class;
  use Teng::Schema::Loader;
  use Test::More;
  use Test::mysqld;
  use Try::Tiny;
  use URI;

  use [% module %]::DB;
  use [% module %]::Models;
  use [% module %]::Test::mysqld;

  sub import {
      my ($class, $app_class, %options) = @_;
      $app_class ||= '[% module %]';

      return unless $app_class;

      Any::Moose::load_class($app_class) unless Any::Moose::is_class_loaded($app_class);

      @_ = ($class, $app_class, %options);

      my $test_db;
      my $caller = caller;
      {
          no strict 'refs';
          no warnings 'redefine';

          *{ $caller .'::template_file' } = sub {
              Ark::Test::context()->stash->{__view_xslate_template}
                    || Ark::Test::context()->request->action->reverse;
          };

          *{ $caller .'::load_fixture' }  = sub {
              my $filename = shift;

              my ($pkg, $file, $line) = caller;
              my ($basename) = split (/\./, basename($file));

              my $dir = dir(models('home')->subdir($file))
                  ->parent->subdir($basename);

              my $datafile = $dir->file($filename);

              do $datafile or die $!;
          };

          *{ $caller . '::setup_testing' } = sub {
              my $app    = $app_class->new;
              my $config = $app->config;

              $config->{database}->{enable_replicated} = 0;

              &{ $caller . '::setup_database' }();

              models('db')->txn_begin;
          };

          *{ $caller . '::database_rollback' } = sub {
              models('db')->txn_rollback;
          };

          *{ $caller . '::test' } = sub (&) {
              my $code = shift;
              &{ $caller.'::setup_testing' };
              $code->();
              &{ $caller.'::database_rollback' };
              &{ $caller.'::done_testing' };
          }
          ;

          *{ $caller . '::setup_database' } = sub {
              my $dsn = $ENV{LAPSEARCH_TEST_DSN} || do {
                  $test_db = [% module %]::Test::mysqld->setup;
                  $test_db->dsn;
              };

              my $conf = [$dsn, 'root', '', {
                  on_connect_do  => ['SET NAMES utf8'],
                  mysql_enable_utf8 => 1,
                  ignore_version => 1,
              }];

              my $dbh = DBI->connect($dsn) or die $!;
              open(my $SQL_FILE,
                   "<", models('home')->subdir(qw/sql/)."/create.sql")
                  or die $!;

              my $sqls = do { local $/; <$SQL_FILE>; };
              close($SQL_FILE);

              for my $s (split(/;/, $sqls)) {
                  next if !$s || $s =~ /^\s+$/;

                  $dbh->do($s);
              }

              models->unregister('db');

              my $loader = Teng::Schema::Loader->load(
                  dbh       => $dbh,
                  namespace => '[% module %]::DB',
              );

              my $schema = $loader->schema;

              for my $table(keys %{$schema->tables}) {
                  my $primary_keys = $schema->tables->{$table}->primary_keys;
                  if (grep /^id$/, @$primary_keys) {
                      $schema->tables->{$table}->primary_keys(['id']);
                  }
              }

              models->register(
                  db => sub {
                      [% module %]::DB->new(
                          dbh       => $dbh,
                          schema    => $schema,
                          slave_dbh => $dbh,
                      );
                  }
              );
          };

          *{ $caller . '::create_uri' } = sub {
              my ($path, $params) = @_;

              my $domain = models('conf')->{domain};

              my $uri = URI->new("https://${domain}${path}");
              $uri->query_form($params->{query} || {});

              return $uri;
          };
      }

      goto $class->can('SUPER::import');
  }

  __PACKAGE__->meta->make_immutable;
---
dir: lib/____var-module_path-var____/Controller
---
file: lib/____var-module_path-var____/DB/RowBase.pm
template: |
  package Nakamap::DB::Row::App;
  use Mouse;
  use MouseX::Foreign qw(Teng::Row);

  __PACKAGE__->meta->make_immutable;
---
file: lib/____var-module_path-var____/DB/TableBase.pm
template: |
  package [% module %]::DB::TableBase;
  use Mouse;
  use [% module %]::Models qw/_models/;

  has table_name => (
      is  => 'ro',
      isa => 'Str',
  );

  sub single {
      my ($self, $cond, $options) = @_;

      $cond    ||= {};
      $options ||= {};

      return _models('db')->single($self->table_name, $cond, $options);
  }

  sub search {
      my ($self, $cond, $options) = @_;

      $cond    ||= {};
      $options ||= {};

      return _models('db')->search($self->table_name, $cond, $options);
  }

  sub insert {
      my ($self, $params) = @_;

      return _models('db')->insert($self->table_name, $params);
  }

  sub delete {
      my ($self, $params) = @_;

      return _models('db')->delete($self->table_name, $params);
  }

  __PACKAGE__->meta->make_immutable;
---
dir: lib/____var-module_path-var____/DB/Row
---
dir: lib/____var-module_path-var____/DB/Table
---
file: lib/____var-module_path-var____/Test/mysqld.pm
template: |
  package [% module %]::Test::mysqld;

  use strict;
  use warnings;
  use utf8;

  use Test::mysqld;
  use JSON;
  use DBI;

  our $SKIP_DROP_DB_MAP = {
      information_schema => 1,
      mysql              => 1,
      test               => 1,
  };

  my $tempfile = File::Spec->catfile(File::Spec->tmpdir, 'test_mysqld.json');

  sub setup {
      my ($class, %config) = @_;

      my $mysqld;

      if ( -e $tempfile ) {
          open my $fh, '<', $tempfile or die $!;
          my $obj = decode_json(join '', <$fh>);
          $mysqld = bless $obj, 'Test::mysqld';
      }
      elsif ( my $json = $ENV{'TEST_MYSQLD'} ) {
          my $obj = decode_json($json);
          $mysqld = bless $obj, 'Test::mysqld';
      }
      else {
          $mysqld = Test::mysqld->new(my_cnf => {
              'skip-networking' => '',
              %config,
          }) or die $Test::mysqld::errstr;
      }

      return $mysqld;
  }

  sub cleanup {
      my ($class, $mysqld) = @_;
      my $dbh = DBI->connect($mysqld->dsn, '', '', {
          AutoCommit => 1,
          RaiseError => 1,
      });

      my $rs = $dbh->selectall_hashref('SHOW DATABASES', 'Database');
      for my $dbname (keys %$rs) {
          next if $SKIP_DROP_DB_MAP->{$dbname};
          $dbh->do("DROP DATABASE ${dbname}");
      }
  }

  1;
---
file: lib/____var-module_path-var____/View/Xslate.pm
template: |
  package [% module %]::View::Xslate;
  use Ark 'View::Xslate';

  use Encode 'encode_utf8';
  use [% module %]::Models;

  has '+path' => (
      default => sub {
          [ models('home')->subdir('root')->stringify() ];
      },
  );

  has '+options' => (
      default => sub {
          +{
              cache_dir => models('home')->subdir(qw/tmp xslate_cache/)->stringify(),
          };
      },
  );

  __PACKAGE__->meta->make_immutable;
---
file: root/index.tx
template: |
  <!DOCTYPE html>
  <html lang="ja">

  <head>
  <meta charset="utf-8">
  <title></title>
  </head>

  <body>

  <h1><: $title :></h1>

  </body>
  </html>
---
file: script/dev/mysqld_runner.pl
template: |
  #!/usr/bin/env perl
  use strict;
  use warnings;
  use FindBin::libs;
  use File::Spec;
  use JSON;
  use [% module %]::Test::mysqld;

  my $tempfile = File::Spec->catfile(File::Spec->tmpdir, 'test_mysqld.json');

  $SIG{'INT'} = *purge;
  END { purge(); }

  print "Starting mysqld...";
  my $mysqld = [% module %]::Test::mysqld->setup(
      log                           => '/tmp/query.log',
      slow_query_log                => 1,
      slow_query_log_file           => '/tmp/slow_query.log',
      log_output                    => 'FILE',
      log_queries_not_using_indexes => 1,
  );
  my $log = File::Spec->catfile($mysqld->{'base_dir'}, qw/tmp mysqld.log/);
  printf " started at %s\n", $mysqld->{'my_cnf'}{'socket'};
  print "log file: $log\n";

  {
      my $json = encode_json({ %$mysqld });
      open my $fh, '>', $tempfile or die $!;
      $fh->print($json);
      $fh->close;
  }

  sleep 3 while -e $tempfile;

  sub purge {
      unlink $tempfile;
      print "Shutting down mysqld...\n";
      exit;
  }
---
file: sql/create.sql
template: |
  --
  -- Created by SQL::Translator::Producer::MySQL
  -- Created on [% localtime %]
  --
  SET foreign_key_checks=0;

  SET foreign_key_checks=1;
---
file: t/00_compile.t
template: |
  use strict;
  use Test::More tests => 1;

  BEGIN { use_ok '[% module %]' }
---
dir: tmp
---
file: xt/01_podspell.t
template: |
  use Test::More;
  eval q{ use Test::Spelling };
  plan skip_all => "Test::Spelling is not installed." if $@;
  add_stopwords(map { split /[\s\:\-]/ } <DATA>);
  $ENV{LANG} = 'C';
  all_pod_files_spelling_ok('lib');
  __DATA__
  [% config.author %]
  [% config.email %]
  [% module %]
---
file: xt/02_perlcritic.t
template: |
  use strict;
  use Test::More;
  eval {
      require Test::Perl::Critic;
      Test::Perl::Critic->import( -profile => 'xt/perlcriticrc');
  };
  plan skip_all => "Test::Perl::Critic is not installed." if $@;
  all_critic_ok('lib');
---
file: xt/03_pod.t
template: |
  use Test::More;
  eval "use Test::Pod 1.00";
  plan skip_all => "Test::Pod 1.00 required for testing POD" if $@;
  all_pod_files_ok();
---
file: xt/perlcriticrc
template: |
  [TestingAndDebugging::ProhibitNoStrict]
  allow=refs
---
config:
  author: NAGATA Hiroaki
  class: Module::Setup::Flavor::ArkTengXslate
  email: nagata {at} handlena.me
  plugins:
    - Config::Basic
    - Template
    - Test::Makefile
    - Additional
