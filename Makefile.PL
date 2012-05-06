use inc::Module::Install;
name 'Module-Setup-Flavor-ArkTengXslate';
all_from 'lib/Module/Setup/Flavor/ArkTengXslate.pm';

# requires '';

tests 't/*.t';
author_tests 'xt';

test_requires 'Test::More';
auto_set_repository;
auto_include;
WriteAll;
