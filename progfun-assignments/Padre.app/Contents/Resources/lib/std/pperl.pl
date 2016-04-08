BEGIN { __x_05ty7nn_::xinit; }
#line 1 "<Packaged Files>/cavaexecscript/pperl.pl"
BEGIN {
    use Cava::Packager;
    $ENV{PERL5DB} = 'BEGIN { require "perl5db.pl" }';
    $ENV{PERL5DB_OPTS} = '';
    #SET THIS TO USE NON-STANDARD CONFIG DIRECTORY FOR TESTING
    #$ENV{PADRE_HOME} = Cava::Packager::GetUserAppDataDir() . '/.PadreSO';
}

#----------------------------------------------------------------------
# Code Stub for module IPC::Run from version : DEFAULT
# This stub will be prepended to scripts when the module is included
#----------------------------------------------------------------------

BEGIN {
    use Cava::Packager;
    if(Cava::Packager::IsWindows() && Cava::Packager::IsPackaged()) {
        my $command = join(' ', @ARGV);
        if($command =~ / -MIPC::Run::Win32Pump -e 1 \d+ \d+ \d+ \d+ \d+ \d+ \d+ /) {
            while( my $arg = shift( @ARGV )) {
                last if($arg eq '1');
            }
            eval{ require IPC::Run::Win32Pump; };
            die $@ if $@;
            exit(0);
        }
        if($command =~ / -MIPC::Run::Win32Pump -e 1/) {
            die 'Incorrect IPC::Run handling in executable';
        }
    }
}

#!/usr/bin/perl

# rudimentary Perl implementation to satisfy minimal usage
# via Padre::Perl;

my $scriptname;
my $evalstring;

while ( my $carg = shift @ARGV ) {
    if($carg !~ /^-/) {
        $scriptname = $carg;
        last;
    }
    if($carg eq '-e') {
        $evalstring = shift @ARGV;
        last;
    }
    if($carg =~ /^-e.+/) { #'" keep duff editors happy
        $evalstring = $carg;
        $evalstring =~ s/^-e//;
        last;
    }
}

if($evalstring) {
    eval qq($evalstring);
    die $@ if $@;
}

if($scriptname) {
    do $scriptname;
    die $@ if $@;
}

1;

