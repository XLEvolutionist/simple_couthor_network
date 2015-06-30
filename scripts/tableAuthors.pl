#!usr/bin/perl
use strict;
use warnings;

######
# Parses a bibtex file, to tabulate number of papers I have co-authored with each
# co-author
######

# The input file is a bibtex file of all my publication, with each entry looking like this

# @article{
#    author = {Buggs, Richard JA and Renny-Byfield, S. and Chester, M. and Jordan-Thaden, I. E. and Viccini, L. F. and Chamala, Srikar and Leitch, A. R and Schnable, P. S. and Barbazuk, W. B. and Soltis, P. S. and Soltis, D. E.},
#    title = {Next-generation sequencing and genome evolution in allopolyploids},
#    journal = {American Journal of Botany},
#    volume = {99},
#    pages = {372-382},
#    year = {2012},
#    type = {Journal Article}
# }

# declare some useful variables
my %coAuthors;
# open the input file

open ( BIB , "<$ARGV[0]" ) || die "Could not open file:$!\n";

# scan through the file, looking for the author field
while ( <BIB> ) {
	next unless m/author/;
	chomp;
	#remove all of the junk data we don't need
	s/author = {//;
	s/},//;
	#print $_ , "\n";
	# seperate each author (they are seperated by "and"
	my @authors = split / and /;
	# now for each co-author get the first and last name
	foreach ( @authors ) {
		my ( $last, $first ) = split /,/;
		#remove any lowercase characters
		$first =~ s/[a-z]+//;
		$first =~ s/\.//g;
		$first =~ s/\s+//g;
		$last =~ s/\s+//g;
		$first = substr($first,0,1);
		#print $first , "\t" , $last , "\n"; 
		if ( exists $coAuthors{$last}{$first} ) {
			$coAuthors{$last}{$first} = $coAuthors{$last}{$first}+1;
		}
		else {
			$coAuthors{$last}{$first} = $coAuthors{$last}{$first}=1;
		}#else 
	}#foreach
}#while

# now for each author print the number of co-authored articles

for my $last ( keys ( %coAuthors ) ) {
	for my $first ( keys ( %{$coAuthors{$last}} ) ) {
		print "$first\t$last\t$coAuthors{$last}{$first}\n";
	}#for
}#for

exit;