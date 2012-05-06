use strict;
use warnings;
use Test::More;
use YAML;
use Module::Setup::Flavor::Ark;
use Module::Setup::Flavor::ArkTengXslate;

{
    for my $kls ('ArkTengXslate', 'Ark') {
        my $class = "Module::Setup::Flavor::$kls";
        local $/;
        local $@;
        my $data = eval "package $class; <DATA>"; ## no critic
        ok $data;
        eval {YAML::Load(join '', $data)};
        ok !$@ or note $@;
    }
}

done_testing;
