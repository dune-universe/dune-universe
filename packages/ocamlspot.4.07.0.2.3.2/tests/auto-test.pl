#!/usr/bin/perl

sub load_file {
    my $file = $_[0];
    open(INF, $file);
    my $content = "";
    while(<INF>){
	$content = "$content$_";
    }
    close INF;
    return $content;
}

sub check_file_head {
    my $file = $_[0];
    open(INH, $file);
    my $head = <INH>;
    close INH;
    if( $head =~ /\(\*\s*([^\*]+)\s*\*\)/ ){
        my $result = $1;
        $result =~ s/^\s+//;
        $result =~ s/\s+$//;
        return $result;
    }
    return "none";
}

sub check_result {
    my $file = $_[0];
    my $start = $_[1];
    my $end = $_[2];
    my $content = load_file($file);

    my $match = substr ($content, $start, $end - $start);
    # print STDERR "MATCH=/$match/\n";

    # print STDERR "PREFIX: $prefix/\n";
    my $prefix = substr($content, 0, $start);

    # fix the prefix for type >>>(* xxx => *) ... <<<
    if( $match =~ /^\s*\(\*[^\*]+=>\s*\*\)/ ){
	$prefix = "$prefix$&";
    }

    my $test_prefix = "";
    if( $prefix =~ /\(\*\s*([^\*]+)=>\s*\*\)\s*$/ ){
	$test_prefix = $1;
	$test_prefix =~ s/^\s*//;
	$test_prefix =~ s/\s*$//;
	# print "PREFIX $test_prefix found\n";
    }

    my $postfix = substr($content, $end, length($content) - $end - 1);
    my $test_postfix = "";
    if( $postfix =~ /^\s*\(\* <=([^\*]+)\*\)/ ){
	$test_postfix = $1;
	$test_postfix =~ s/^\s*//;
	$test_postfix =~ s/\s*$//;
	# print "POSTFIX $test_postfix found\n";
    }

    if( $test_prefix eq $test_postfix ){ return $test_prefix; }
    else { 
	$message = "pre/post=/$test_prefix/$test_postfix/\n";
	return ""; 
    }
}

my $all_tests = 0;
my $all_succeeds = 0;

sub test {
    my $file = $_[0];

    if( -f $file . "l" ){
	print STDERR $file . "l exists. Skip the test\n";
	return;
    }

    my $content = load_file($file);
    print STDER "$file loaded\n";
    my $pos = 0;
    while( $content =~ /\s*\(\*\s*\?\s*([^\*]+)\*\)/ ){
	$pos += length($`);
	my $test_name = $1;
	my $test_pos = $pos - 1;
	$content = $'; #'
	$pos = $pos + length($&);

	$test_name =~ s/^\s*//;
	$test_name =~ s/\s*$//;

	if( $test_name eq "" ) { next; }

	my $message = "* $test_name: ocamlspot $file:$test_pos\n";

	my $succeed = 0;
	if( -x "ocamlspot" ){
	    $command = "./ocamlspot $file:b$test_pos";
	} elsif( -x "../ocamlspot" )  {
	    $command = "../ocamlspot $file:b$test_pos";
	} else {
	    print "no ocamlspot binary around\n";
	    exit 1;
	}

	print STDERR "$command\n";
	open(IN, "$command |");

	$all_tests++;
	$message = "";
	while(<IN>){

	    s/\r\n/\n/; # Fucking Windows

	    $tested = 0;
            my $result;
            if( /^Spot: <(.*):all>/ ){ # whole file
                $tested = 1;
                $message = "$message$&\n";
                $result = check_file_head($1);
            }
	    if( /^Spot: <(.*):l[0-9]+c[0-9]+b([0-9]+):l[0-9]+c[0-9]+b([0-9]+)>$/ ){
		$tested = 1;
		$message = "$message$&\n";
		$result = check_result($1, $2, $3);
            }
            if( $tested ){
                if( $test_name eq $result ){
                    print STDERR "$file:$test_pos:$test_name:\tOK!\n";
                    $succeed = 1;
		    $all_succeeds ++;
		    last;
                } else {
		    $message = $messge . "$file:$test_pos:$test_name:\tFAILED!\n$message\{ test_name=\"$test_name\"; result=\"$result\" \}\n";
		}
            }
	}
	while(<IN>){} # avoid Broken pipe
	close IN;
	if (!$succeed ){
	    print STDERR $message;
	    if( $test_name =~ /impos/ ){ # This is not a bug. Known impossible.
		print STDERR "$file:$test_pos:$test_name:\tnot found, but a known issue\n";
		$all_succeeds ++;
	    } else {
		print STDERR "$file:$test_pos:$test_name:\tFAILED! NOT FOUND!\n";
	    }
	}
    }
}

for $f (@ARGV) {
    test($f);
}

$ratio = $all_succeeds/$all_tests;
printf "successes/tests = $all_succeeds/$all_tests = %.02f\n", $ratio;
if( $ratio == 1.0  ){
    print "Looks ok\n"; 
    exit 0;
} else {
    print "Some tests failed!\n"; 
    exit 1;
}

  
