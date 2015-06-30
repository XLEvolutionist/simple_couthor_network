#!usr/bin/perl
use strict;
use warnings;
use List::Pairwise qw(mapp grepp);
use List::Util;

######
# Parses a bibtex file, to tabulate number of papers
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

#
# This scrip actually duplicates samples such that:
#
# authorA authorB 10
# authorB authorA 10
#
# both appear in the output, but not to worry, the next step in R removes this.

###
# declare some useful variables
###
my %coAuthors; #stores a list of all unique author names in your bibtex file
my %coAuthorLists;	#each pub gets it own list of auhtors, useful for later seeing shared authorship
my $paperNum=1;	# an iterator
my %authorSums;	# a nested hash of the form {author1}{author2} that stores the number of shared citations


####
# open the input file
####
open ( BIB , "<$ARGV[0]" ) || die "Could not open file:$!\n";

####
# Do the leg work, and produce the data
####

# scan through the file, looking for the author field
while ( <BIB> ) {
	# we only care about the author info so skip everything else
	next unless m/author/;
	chomp;
	#remove all of the junk data we don't need
	s/author = {//;
	s/},//;
	# seperate each author (they are seperated by "and")
	my @authorslong = split / and /;
	# now for each co-author get the first and last name
	foreach ( @authorslong ) {
		my ( $last, $first ) = split /,/;
		#remove any lowercase characters
		$first =~ s/[a-z]+//;
		$first =~ s/\.//g;
		$first =~ s/\s+//g;
		$last =~ s/\s+//g;
		$first = substr($first,0,1);
		# add each author to the list for the current pub
		push(@{$coAuthorLists{$paperNum}}, "$last,$first");
		# keep a record of unique authors
		$coAuthors{"$last,$first"} = "Y";
	}#foreach
	# iterate for the next pub
	$paperNum++;
}#while

# a list of all co-authors
my @list = keys ( %coAuthors );

# for each author
foreach ( @list ) {
	my $name1 = $_;
	my @list2 = grep ( !/$name1/, @list );
	# and for all the other authors
	foreach ( @list2 ) {
		my $name2=$_;
		# make a list of pubs..
		my @papers = keys(%coAuthorLists);
		#...and for each of those pubs...
		foreach ( @papers ) {
			my @authors = @{$coAuthorLists{$_}};
			# see if they are both in the author list
			if ( grep (/$name1/ , @authors ) and grep (/$name2/  , @authors ) ) {
					$authorSums{$name1}{$name2}++;
			}#if
		}#foreach	
	}#foreach	 
}#foreach

#####
# Print of the info in a useful way
#####

print "name1\tname2\tcitations\n";
foreach my $key ( keys ( %authorSums ) ) {
	foreach my $key2 ( keys ( $authorSums{$key} ) ) {
		print "$key\t$key2\t$authorSums{$key}{$key2}\n";
	}#foreach
}#foreach

exit;