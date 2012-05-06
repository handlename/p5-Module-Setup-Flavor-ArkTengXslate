package Module::Setup::Flavor::ArkTengXslate;
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

      my $slave_conf = $self->get('conf')->{database}{slave}
          or die 'Require database config';

      my $slave_dbh = DBI->connect(@{$slave_conf})
          or die 'cannot connect database';

      my $loader = Teng::Schema::Loader->load(
          dbh       => $dbh,
          namespace => PROJECT_NAME.'::DB'
      );

      $cls->new(
          dbh           => $dbh,
          slave_dbh     => $slave_dbh,
          schema        => $loader,
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
dir: lib/____var-module_path-var____/Controller
---
file: lib/____var-module_path-var____/DB/RowBase.pm
template: |
  package Nakamap::DB::Row::App;
  use Mouse;
  use MouseX::Foreign qw(Teng::Row);

  __PACKAGE__->meta->make_immutable;
---
file: lib/____var-module_path-var____/DB/Schema.pm
template: |
  package [% module %]::DB::Schema;
  use Teng::Schema::Declare;
  table {
  };

  1;
  # generated by script/dev/make_teng_schema.pl
---
file: lib/____var-module_path-var____/DB/TableBase.pm
template: |
  package [% module %]::DB::TableBase;
  use Mouse;
  use [% module %]::Models;

  has table_name => (
      is  => 'ro',
      isa => 'Str',
  );

  sub search {
      my ($self, $cond, $options) = @_;

      $cond    ||= {};
      $options ||= {};

      return models('db')->search($self->table_name, $cond, $options);
  }

  __PACKAGE__->meta->make_immutable;
---
dir: lib/____var-module_path-var____/DB/Row
---
dir: lib/____var-module_path-var____/DB/Table
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
file: script/dev/make_teng_schema.pl
template: |
  #!/usr/bin/env perl
  use strict;
  use warnings;
  use DBI;
  use FindBin::libs;
  use [% module %]::Models;
  use Teng::Schema::Dumper;

  my $conf = models('conf')->{database}{master};
  my $dbh  = DBI->connect(@{ $conf }) or die "Cannot connect to DB:: " . $DBI::errstr;

  my $schema = Teng::Schema::Dumper->dump(dbh => $dbh, namespace => '[% module %]::DB');
  $schema   .= "# generated by $0\n";

  my $dest = models('home')->file(qw/lib [% module %] DB Schema.pm/);
  open my $fh, '>', $dest or die "cannot open file '$dest': $!";
  print {$fh} $schema;
  close;
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
  author: Default Name
  class: Module::Setup::Flavor::Default
  email: 'default {at} example.com'
  plugins:
    - Config::Basic
    - Template
    - Test::Makefile
    - Additional
