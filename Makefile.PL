use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'		=> 'TM',
    'VERSION_FROM'	=> 'TM.pm', # finds $VERSION
    'PREREQ_PM'		=> {}, # e.g., Module::Name => 1.1
    'LIBS'		=> [], 
    'DEFINE'		=> '', # e.g., '-DHAVE_SOMETHING'
    'XSPROTOARG'        => '-noprototypes',
    'TYPEMAPS'  => ['perlobject.map' ],
    'INC'		=> '', 
);
