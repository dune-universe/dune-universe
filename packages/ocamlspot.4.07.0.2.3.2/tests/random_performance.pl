#!/usr/bin/perl

open(IN, "ls ../*.ml ../*.mli |");
while(<IN>){
    chop;
    my $cmt = $_;
    $cmt =~ s/\.ml/.cmt/;
    if( -f $cmt ){
	@cands = ($_, @cands);
    }
}

sub rand_cand {
    return $cands[int(rand($#cands+1))];
}

sub rand_pos {
    my $cand = $_[0];
    my @st = stat($cand);
    return int(rand($st[7]));
}

for ($i = 0; $i< 100; $i++){
    my $cand = rand_cand();
    my $pos = rand_pos($cand);

    $log = `../ocamlspot.opt $cand:b$pos 2>&1`;
    if( $log =~ /^Error: nothing at .*\nBYE!/ ){
	next;
    }
    print "------\n$cand:b$pos\n";
    print $log;
}

