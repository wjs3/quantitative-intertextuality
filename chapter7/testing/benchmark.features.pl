#! /usr/bin/perl

#
# benchmark.features.pl 
#
#   for David Johnson

=head1 NAME

benchmark.features.pl - generate feature data for the lucan-vergil benchmark

=head1 SYNOPSIS

benchmark.features.pl [options] > FILE

=head1 DESCRIPTION

This script reads the benchmark data created by the Tesserae script build-rec.pl and 
stored in $fs_data/bench/rec.cache.  It generates lots of feature data for each parallel,
to be used by Walter and David to model the human scores.  Output is to STDOUT.
 
This requires a configured installation of Tesserae.  In addition, the ancillary Tesserae
scripts 'build-rec.pl' and 'tess-idf.pl' must have been run.
 
=head1 OPTIONS AND ARGUMENTS

=over

=item B<--delim> I<FS>

Separate fields by string I<FS> in output.  Default is "\t".

=item B<--parallel> I<N>

Allow I<N> parallel processes.  Requires Parallel::ForkManager.

=item B<--quiet>

Don't display progress.

=item B<--no-FEAT>

You can turn off individual sets of measurements by prefixing '--no-' to the name of
the feature set.  Use 'perldoc benchmark.features.pl' to see a full list of feature sets 
with their names.

=item B<--help>

Print usage and exit.

=back

=head1 AVAILABLE FEATURE SETS

=over

=item B<benchmark>

This feature set includes the original columns from the input benchmark set, including
the texts of the two phrases in each parallel, their loci, phrase id numbers in Tesserae,
human-assigned scores and whether they were found in the commentaries.

=item B<num-matching>

This feature set includes various methods of tallying the number of matching words in
the parallel.

=item B<freq-match>
 	
This feature set includes several metrics based on the frequencies of matching words in
in the parallel.  It will return 'NA' in every column for parallels that don't have any
specific matching words.

=item B<freq-phrase>
 	
This feature set calculates the same series of metrics as B<freq-match> but uses all the
words in the phrases rather than just those shared between them.  Because of this, it can
return values even for parallels that don't share any words.

=item B<dist> 	

A set of metrics measuring the distance in words across which the intertext is spread.

=item B<edit-dist>

Levenshtein edit distance between the two phrases of the the parallel.
 	
=item B<count-chr-ngram>

This measures the difference in character n-gram counts between the two phrases for 
several values of I<n>.  That is, the counts of all character n-grams occurring in one
phrase are subtracted from the counts for those same n-grams in the other phrase; the
absolute values of these differences for individual n-grams are then added together.

=item B<freq-chr-ngram>

This measures the cosine similarity of the two phrases based upon character n-gram 
probabilities for several values of I<n>.
 	
=item B<semantic>

A first try at measuring semantic similarity of the two phrases.  Uses the English terms
found in dictionary definitions of the Latin words in the phrases as features to describe
the phrases.  Each phrase is a vector whose components are the counts of terms in the 
English definitions of its words.  The two vectors are compared using cosine similarity.

=head1 KNOWN BUGS

=head1 SEE ALSO

=head1 COPYRIGHT

University at Buffalo Public License Version 1.0.
The contents of this file are subject to the University at Buffalo Public License Version 1.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://tesserae.caset.buffalo.edu/license.txt.

Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the specific language governing rights and limitations under the License.

The Original Code is benchmark.features.pl.

The Initial Developer of the Original Code is Research Foundation of State University of New York, on behalf of University at Buffalo.

Portions created by the Initial Developer are Copyright (C) 2007 Research Foundation of State University of New York, on behalf of University at Buffalo. All Rights Reserved.

Contributor(s): Chris Forstall, Walter Scheirer, David Johnson

Alternatively, the contents of this file may be used under the terms of either the GNU General Public License Version 2 (the "GPL"), or the GNU Lesser General Public License Version 2.1 (the "LGPL"), in which case the provisions of the GPL or the LGPL are applicable instead of those above. If you wish to allow use of your version of this file only under the terms of either the GPL or the LGPL, and not to allow others to use your version of this file under the terms of the UBPL, indicate your decision by deleting the provisions above and replace them with the notice and other provisions required by the GPL or the LGPL. If you do not delete the provisions above, a recipient may use your version of this file under the terms of any one of the UBPL, the GPL or the LGPL.

=cut

use strict;
use warnings;

# modules necessary to look for config

use Cwd qw/abs_path/;
use FindBin qw/$Bin/;
use File::Spec::Functions;

# load configuration file

my $tesslib;

BEGIN {
	
	my $dir  = $Bin;
	my $prev = "";
			
	while (-d $dir and $dir ne $prev) {

		my $pointer = catfile($dir, '.tesserae.conf');

		if (-s $pointer) {
		
			open (FH, $pointer) or die "can't open $pointer: $!";
			
			$tesslib = <FH>;
			
			chomp $tesslib;
			
			last;
		}
		
		$dir = abs_path(catdir($dir, '..'));
	}
	
	unless ($tesslib) {
	
		die "can't find .tesserae.conf!";
	}
}

# load Tesserae-specific modules

use lib $tesslib;

use TessSystemVars;
use EasyProgressBar;

# modules to read cmd-line options and print usage

use Getopt::Long;
use Pod::Usage;

# load additional modules necessary for this script

use Storable;
use Data::Dumper;

#
# initialize variables
#

my $parallel = 0;
my $delim    = "\t";
my $quiet    = 0;
my $help     = 0;

# data locations

my %file = (	
	stems         => catfile($fs_data, 'common', 'la.stem.cache'),
	syns          => catfile($fs_data, 'common', 'la.syn.cache'),
	semantic      => catfile($fs_data, 'common', 'whit.cache'),

	freq_stem_ALL => catfile($fs_data, 'common', 'la.stem.freq'),
	freq_word_ALL => catfile($fs_data, 'common', 'la.word.freq'),

	idf_phrase    => catfile($fs_data, 'common', 'la.idf_phrase'),
	idf_text      => catfile($fs_data, 'common', 'la.idf_text'),
	
	rec           => catfile($fs_data, 'bench', 'rec.cache'),
	
	freq_stem_AEN => catfile($fs_data, 'v3', 'la', 'vergil.aeneid', 
									'vergil.aeneid.freq_score_stem'),
	freq_word_AEN => catfile($fs_data, 'v3', 'la', 'vergil.aeneid',
									'vergil.aeneid.freq_score_word'),
	freq_stem_BC  => catfile($fs_data, 'v3', 'la', 'lucan.bellum_civile.part.1', 
									'lucan.bellum_civile.part.1.freq_score_stem'),
	freq_word_BC  => catfile($fs_data, 'v3', 'la', 'lucan.bellum_civile.part.1',
									'lucan.bellum_civile.part.1.freq_score_word'),
);

#
# features to measure
#

my @features = (
	
	# columns from the benchmark csv file
	{
		name   => 'benchmark',
		calc   => \&orig_cols,
		enable => 1
	},
	
	# counts of matching words
	{
		name   => 'num-matching',
 		calc   => \&num_matching,
 		enable => 1
 	},
 	
 	# frequency stats for matching words
 	{
 		name   => 'freq-match',
 		calc   => \&freq_match,
 		enable => 1
 	},
 	
 	# frequency stats for all words
 	{
 		name   => 'freq-phrase',
	 	calc   => \&freq_phrase,
	 	enable => 1
 	},
 	
 	# word-distance measures
 	{
 		name   => 'dist',
	 	calc   => \&dist,
	 	enable => 1
	},
 	
 	# levenshtein edit distance
 	{
 		name   => 'edit-dist',
 		calc   => \&edit_dist,
 		enable => 1
 	},
 	
 	# difference in ngram counts
 	{
 		name   => 'count-chr-ngram',
	 	calc   => \&count_chr_ngram,
	 	enable => 1
	},
 	
 	# difference in ngram probabilities
 	{
 		name   => 'freq-chr-ngram',
	 	calc   => \&freq_chr_ngram,
	 	enable => 1
 	},
 	
 	# semantic similarity
 	{
 		name   => 'semantic',
 		calc   => \&semantic,
 		enable => 1
 	}
);

#
# get user options
#

my %opt_feature;

for my $feat (@features) {

	$opt_feature{$feat->{name} . '!'} = \$feat->{enable};
}

GetOptions(
	%opt_feature,
	'parallel=i' => \$parallel,
	'delim'      => \$delim,
	'quiet'      => \$quiet,
	'help'       => \$help
);

# print usage if the user needs help
	
if ($help) {

	pod2usage(-verbose => 1);
}

#
# load tesserae data
#

# the stem dictionary
my %stem = %{ retrieve($file{stems}) };

# the synonym dictionary
my %syn = %{ retrieve($file{syns}) };

# the semantic tag dictionary
my %semantic = %{ retrieve($file{semantic}) };


# load the two texts from the Tesseare database

my %freq;

for my $text (qw(ALL AEN BC)) {
		
	for my $feature (qw(word stem)) {
	
		$freq{$feature}{$text} = TessSystemVars::stoplist_hash($file{"freq_${feature}_$text"});
	}
}

# inverse document frequencies
my %idf_by_phrase = %{retrieve($file{idf_phrase})};
my %idf_by_text = %{retrieve($file{idf_text})};

# the table of parallels
my @rec = @{ retrieve($file{rec}) };


#
# initialize parallel processing
#

my $pm;

if ($parallel) {

	eval {
		
		require Parallel::ForkManager;
		
		$pm = Parallel::ForkManager->new($parallel);
	};
	
	if ($@) {
	
		print STDERR "Can't intialize parallel process manager: $!\n";
		print STDERR "Continuing with --parallel 0\n";
		$parallel = 0;
	}
}

# draw a progress bar;

my $pr;

$pr = ProgressBar->new(scalar(@rec), $quiet);

#
# main loop
#

for my $rec (@rec) {
		
	$pr->advance;
	
	# fork
	
	if ($parallel) {

		$pm->start and next;
	}

	# perform a tesserae-style search:
	# 
	#  some of the later metrics depend on
	#  which words match.
	
	$$rec{TESSMATCH} = TessSearch(BC => $$rec{BC_PHRASE}, AEN => $$rec{AEN_PHRASE});

	# the row of feature data for this parallel
	
	my @row;

	for my $feat (@features) {
	
		if ($feat->{enable}) {
		
			push @row, $feat->{calc}->($rec);
		}
	}

	print format_row($delim, \@row);
	
	$pm->finish if $parallel;
}

$pm->wait_all_children if $parallel;

#
# feature subroutines
#

# the first columns are the original columns from the old CSV file.
# 
# Walter may be interested in these, particularly the text.
# David probably doesn't want them, but they can be dropped later
# using cut or awk.

sub orig_cols {

	my $rec = shift;
	
	my @cols = ( 
		'BC_PHRASEID',		# phrase id in lucan
		'BC_BOOK',			# book number in lucan
		'BC_LINE',			# line number in lucan
		'BC_PHRASE',		# text of the lucan phrase
		
		'AEN_PHRASEID',		# phrase id in vergil
		'AEN_BOOK',			# book number in vergil
		'AEN_LINE',			# line number in vergil
		'AEN_PHRASE',		# text of the vergil phrase
		
		'SCORE',			# human-assigned score
		'AUTH'				# in commentaries?
	);
	
	# when passed non-rec, return column headings
	
	unless ($rec) {return @cols}
	
	# otherwise, calculate
		
	my %param = (
	
		BC_PHRASEID  => $$rec{BC_PHRASEID},
		BC_BOOK      => $$rec{BC_BOOK},
		BC_LINE      => $$rec{BC_LINE},
		BC_PHRASE    => join(" ", @{$$rec{BC_PHRASE}}),
		
		AEN_PHRASEID => $$rec{AEN_PHRASEID},
		AEN_BOOK     => $$rec{AEN_BOOK},
		AEN_LINE     => $$rec{AEN_LINE},
		AEN_PHRASE   => join(" ", @{$$rec{AEN_PHRASE}}),

		SCORE        => $$rec{SCORE},
		AUTH         => defined $$rec{AUTH} ? 1 : 0
	);

	return @param{@cols};
}

#
# diverse counts of matching words
#

sub num_matching {

	my $rec = shift;
	
	my @cols = (
		'MATCH_WORD_BC',	# number of exact-word matches in lucan
		'MATCH_WORD_AEN',	# number of exact-word matches in vergil
		'MATCH_WORD_BOTH',	# total number of exact-word matches
		
		'MATCH_STEM_BC',	# number of stem-based matches in lucan
		'MATCH_STEM_AEN',	# number of stem-based matches in vergil
		'MATCH_STEM_BOTH',	# number of stem-based matches in both
		
		'MATCH_WORD_UNIQ',	# number of unique forms among words matching by word
		'MATCH_STEM_UNIQ'	# number of unique forms among words matching by stem
			
	);
	
	# when passed non-rec, return column headings
	
	unless ($rec) { return @cols }
	
	# otherwise, calculate
	
	my %match = %{$$rec{TESSMATCH}};
	
	my %param;
	
	for my $featureset qw(WORD STEM) {
		
		# set everything to 0 by default
		
		for my $text qw(BC AEN) {
			
			$param{"MATCH_${featureset}_$text"} = 0;
		}
		
		# now check for matches
		
		if (defined $match{$featureset} && scalar(keys %{$match{$featureset}}) > 0) {
			
			# this iterates over matching features
			
			for my $text qw(BC AEN) {
				
				my %marked = ();
			
				for my $feature (keys %{$match{$featureset}}) {
					
					# this iterates over the word ids indexed under each feature
		
					for my $i (@{$match{$featureset}{$feature}{$text}}) {
						
						$marked{$i} = 1;
					}
				}
				
				$param{"MATCH_${featureset}_$text"} = keys %marked;
			}
		}
		
		$param{"MATCH_${featureset}_BOTH"} = $param{"MATCH_${featureset}_AEN"} + $param{"MATCH_${featureset}_BC"};
	}
	
	# number of unique forms among words matching by stem
	
	$param{MATCH_STEM_UNIQ} = defined $match{STEM} ? scalar(keys %{$match{STEM}}) : 0;

	# number of unique forms among words matching by word

	$param{MATCH_WORD_UNIQ} = defined $match{WORD} ? scalar(keys %{$match{WORD}}) : 0;

	return @param{@cols};
}


#
# some frequency measurements
#
# F_M_ series are frequency metrics based on matching words only
#      if the parallel has no matching words this returns NA
#
# key to subscripts:
#
# _DOC_ uses document specific frequencies
# _COR_ uses corpus-wide frequencies
# _RAT_ uses (document_freq / corpus_freq)
# _DIF_ uses (document_freq - corpus_freq) 
# 
# _AVG_ is the mean frequency
# _MIN_ is the minimum value only; supposed to represent the most interesting word
# _SUM_ is the sum of frequencies
# _INV_ is the sum of (1/f) for each freq f
#
# _BC   uses frequencies in lucan only
# _AEN  uses those in vergil only
# _BOTH uses frequencies from both phrases

sub freq_match {

	# benchmark record

	my ($rec) = shift;
	
	# results of tess mini-search

	my %match = %{$$rec{TESSMATCH}{STEM}};
	
	# column headings for results
	
	my @cols = (
	
		# document-specific frequencies
			
		'F_M_DOC_AVG_BC', 'F_M_DOC_AVG_AEN', 'F_M_DOC_AVG_BOTH',
		'F_M_DOC_MIN_BC', 'F_M_DOC_MIN_AEN', 'F_M_DOC_MIN_BOTH',
		'F_M_DOC_SUM_BC', 'F_M_DOC_SUM_AEN', 'F_M_DOC_SUM_BOTH',
		'F_M_DOC_INV_BC', 'F_M_DOC_INV_AEN', 'F_M_DOC_INV_BOTH',
	
		# corpus-wide frequencies

		'F_M_COR_AVG_BC', 'F_M_COR_AVG_AEN', 'F_M_COR_AVG_BOTH',		
		'F_M_COR_MIN_BC', 'F_M_COR_MIN_AEN', 'F_M_COR_MIN_BOTH',
		'F_M_COR_SUM_BC', 'F_M_COR_SUM_AEN', 'F_M_COR_SUM_BOTH',
		'F_M_COR_INV_BC', 'F_M_COR_INV_AEN', 'F_M_COR_INV_BOTH',
		
		# ratio of text-specific freq to corpus-wide freq
		
		'F_M_RAT_SUM_BC', 'F_M_RAT_SUM_AEN', 'F_M_RAT_SUM_BOTH',
		'F_M_RAT_INV_BC', 'F_M_RAT_INV_AEN', 'F_M_RAT_INV_BOTH',
	
		# difference between text-specific and corpus-wide freqs
	
		'F_M_DIF_SUM_BC', 'F_M_DIF_SUM_AEN', 'F_M_DIF_SUM_BOTH',
		'F_M_DIF_INV_BC', 'F_M_DIF_INV_AEN', 'F_M_DIF_INV_BOTH'
	);
	
	# when passed non-rec, return column headings
	
	unless ($rec) {return \@cols}
	
	# results
		
	my %param;
	
	# don't proceed unless there are matching words
	
	if (scalar(keys %match) == 0) {
		
		return ('NA') x scalar(@cols);
	}
	
	# these hold raw frequencies
	
	my %values_doc;
	my %values_corpus;
	my %values_ratio;
	my %values_diff;
		
	for my $text (qw/BC AEN/) {
		
		for my $feature (keys %match) {
		
			for my $i (@{$match{$feature}{$text}}) {
			
				my $word = $$rec{$text . "_PHRASE"}[$i];

				push @{$values_doc{$text}},    $freq{word}{$text}{$word};
				push @{$values_corpus{$text}}, $freq{word}{ALL}{$word};
				push @{$values_ratio{$text}}, $freq{word}{$text}{$word} / $freq{word}{ALL}{$word};
				push @{$values_diff{$text}}, $freq{word}{$text}{$word} - $freq{word}{ALL}{$word};
			}
		}
		
	 	$param{"F_M_DOC_AVG_$text"} = mean(@{$values_doc{$text}});
		$param{"F_M_COR_AVG_$text"} = mean(@{$values_corpus{$text}});
	}
	
 	$param{"F_M_DOC_AVG_BOTH"} = mean(@{$values_doc{BC}},    @{$values_doc{AEN}});
 	$param{"F_M_COR_AVG_BOTH"} = mean(@{$values_corpus{BC}}, @{$values_corpus{AEN}});
	
	
	#
	# now just the minimum frequencies
	#
	
	for my $text qw(BC AEN) {
	
 		$param{"F_M_DOC_MIN_$text"} = min($values_doc{$text});
 		$param{"F_M_COR_MIN_$text"} = min($values_corpus{$text});
	}
	
	$param{"F_M_DOC_MIN_BOTH"} = min([$param{F_M_DOC_MIN_AEN}, $param{F_M_DOC_MIN_BC}]);
	$param{"F_M_COR_MIN_BOTH"} = min([$param{F_M_COR_MIN_AEN}, $param{F_M_COR_MIN_BC}]);
	
	#
	# sums of frequencies
	#
	
	for my $text qw(BC AEN) {
	
 		$param{"F_M_DOC_SUM_$text"} = sum(@{$values_doc{$text}});
 		$param{"F_M_COR_SUM_$text"} = sum(@{$values_corpus{$text}});
 		$param{"F_M_RAT_SUM_$text"} = sum(@{$values_ratio{$text}});
 		$param{"F_M_DIF_SUM_$text"} = sum(@{$values_diff{$text}}); 		
	}
	
	$param{"F_M_DOC_SUM_BOTH"} = $param{F_M_DOC_SUM_AEN} + $param{F_M_DOC_SUM_BC};
	$param{"F_M_COR_SUM_BOTH"} = $param{F_M_COR_SUM_AEN} + $param{F_M_COR_SUM_BC};
	$param{"F_M_RAT_SUM_BOTH"} = $param{F_M_RAT_SUM_AEN} + $param{F_M_RAT_SUM_BC};
	$param{"F_M_DIF_SUM_BOTH"} = $param{F_M_DIF_SUM_AEN} + $param{F_M_DIF_SUM_BC};
	
	#
	# inverse sums
	#
	
	for my $text qw(BC AEN) {

		my %inv;
			
		for (@{$values_doc{$text}}) {
		
			next if $_ == 0;
			
			push @{$inv{DOC}}, 1/$_;
		}

		for (@{$values_corpus{$text}}) {
		
			next if $_ == 0;
			
			push @{$inv{COR}}, 1/$_;
		}

		for (@{$values_ratio{$text}}) {
		
			next if $_ == 0;
			
			push @{$inv{RAT}}, 1/$_;
		}

		for (@{$values_corpus{$text}}) {
		
			next if $_ == 0;
			
			push @{$inv{DIF}}, 1/$_;
		}
	
		for my $p (qw/DOC COR RAT DIF/) {
	
			$param{"F_M_${p}_INV_$text"} = sum(@{$inv{$p}});
		}
	}

	for my $p (qw/DOC COR RAT DIF/) {

		$param{"F_M_${p}_INV_BOTH"} = $param{"F_M_${p}_INV_AEN"} + $param{"F_M_${p}_INV_BC"};
	}

	for (values %param) { $_ = sprintf("%.8f", $_) }
	
	return @param{@cols};
}

#
# More frequency measurements
#
# F_P_ series is similar to F_M_, but considers the frequencies of 
#      all the words in the phrase, not just the match words, and 
#      thus returns an answer even for parallels with no common words
#
# for description of subscript notation see sub freq_match above

sub freq_phrase {

	# benchmark record

	my ($rec) = shift;
		
	# column headings for results
	
	my @cols = (
	
		# document-specific frequencies
			
		'F_P_DOC_AVG_BC', 'F_P_DOC_AVG_AEN', 'F_P_DOC_AVG_BOTH',
		'F_P_DOC_MIN_BC', 'F_P_DOC_MIN_AEN', 'F_P_DOC_MIN_BOTH',
		'F_P_DOC_SUM_BC', 'F_P_DOC_SUM_AEN', 'F_P_DOC_SUM_BOTH',
		'F_P_DOC_INV_BC', 'F_P_DOC_INV_AEN', 'F_P_DOC_INV_BOTH',
	
		# corpus-wide frequencies

		'F_P_COR_AVG_BC', 'F_P_COR_AVG_AEN', 'F_P_COR_AVG_BOTH',		
		'F_P_COR_MIN_BC', 'F_P_COR_MIN_AEN', 'F_P_COR_MIN_BOTH',
		'F_P_COR_SUM_BC', 'F_P_COR_SUM_AEN', 'F_P_COR_SUM_BOTH',
		'F_P_COR_INV_BC', 'F_P_COR_INV_AEN', 'F_P_COR_INV_BOTH',
		
		# ratio of text-specific freq to corpus-wide freq
		
		'F_P_RAT_SUM_BC', 'F_P_RAT_SUM_AEN', 'F_P_RAT_SUM_BOTH',
		'F_P_RAT_INV_BC', 'F_P_RAT_INV_AEN', 'F_P_RAT_INV_BOTH',
	
		# difference between text-specific and corpus-wide freqs
	
		'F_P_DIF_SUM_BC', 'F_P_DIF_SUM_AEN', 'F_P_DIF_SUM_BOTH',
		'F_P_DIF_INV_BC', 'F_P_DIF_INV_AEN', 'F_P_DIF_INV_BOTH'
	);
	
	# when passed non-rec, return column headings
	
	unless ($rec) {return @cols}
	
	# results
	
	my %param;
	
	# these hold raw frequencies
	
	my %values_doc;
	my %values_corpus;
	my %values_ratio;
	my %values_diff;

	for my $text (qw/BC AEN/) {
		
		for my $word (@{$$rec{$text . "_PHRASE"}}) {
					
			if (! defined $freq{word}{$text}{$word}) { print STDERR "\$freq{$text}{$word} undefined\n"; }
			if (! defined $freq{word}{ALL}{$word})   { print STDERR "\$freq{ALL}{$word} undefined\n"; }
								
			push @{$values_doc{$text}},    $freq{word}{$text}{$word};
			push @{$values_corpus{$text}}, $freq{word}{ALL}{$word};
			push @{$values_ratio{$text}},  $freq{word}{$text}{$word} / $freq{word}{ALL}{$word};
			push @{$values_diff{$text}},   $freq{word}{$text}{$word} - $freq{word}{ALL}{$word};
		}
		
		
		$param{"F_P_DOC_AVG_$text"} = mean(@{$values_doc{$text}});
		$param{"F_P_COR_AVG_$text"} = mean(@{$values_corpus{$text}});
	}
	
	$param{F_P_DOC_AVG_BOTH} = mean(@{$values_doc{AEN}},    @{$values_doc{BC}});
	$param{F_P_COR_AVG_BOTH} = mean(@{$values_corpus{AEN}}, @{$values_corpus{BC}});
	
	#
	# now just the minimum frequencies
	#
	
	for my $text qw(BC AEN) {
	
 		$param{"F_P_DOC_MIN_$text"} = min($values_doc{$text});
		$param{"F_P_COR_MIN_$text"} = min($values_corpus{$text});
	}
	
	$param{F_P_DOC_MIN_BOTH} = min([$param{F_P_DOC_MIN_AEN}, $param{F_P_DOC_MIN_BC}]);
	$param{F_P_COR_MIN_BOTH} = min([$param{F_P_COR_MIN_AEN}, $param{F_P_COR_MIN_BC}]);

	#
	# sums of frequencies
	#
	
	for my $text qw(BC AEN) {
	
 		$param{"F_P_DOC_SUM_$text"} = sum(@{$values_doc{$text}});
 		$param{"F_P_COR_SUM_$text"} = sum(@{$values_corpus{$text}});
 		$param{"F_P_RAT_SUM_$text"} = sum(@{$values_ratio{$text}});
 		$param{"F_P_DIF_SUM_$text"} = sum(@{$values_diff{$text}}); 		
	}
	
	$param{"F_P_DOC_SUM_BOTH"} = $param{F_P_DOC_SUM_AEN} + $param{F_P_DOC_SUM_BC};
	$param{"F_P_COR_SUM_BOTH"} = $param{F_P_COR_SUM_AEN} + $param{F_P_COR_SUM_BC};
	$param{"F_P_RAT_SUM_BOTH"} = $param{F_P_RAT_SUM_AEN} + $param{F_P_RAT_SUM_BC};
	$param{"F_P_DIF_SUM_BOTH"} = $param{F_P_DIF_SUM_AEN} + $param{F_P_DIF_SUM_BC};

	#
	# inverse sums
	#
	
	for my $text qw(BC AEN) {
	
		my %inv;
		
		for (@{$values_doc{$text}}) {
		
			next if $_ == 0;
			
			push @{$inv{DOC}}, 1/$_;
		}

		for (@{$values_corpus{$text}}) {
		
			next if $_ == 0;
			
			push @{$inv{COR}}, 1/$_;
		}
	
		for (@{$values_ratio{$text}}) {
		
			next if $_ == 0;
			
			push @{$inv{RAT}}, 1/$_;
		}

		for (@{$values_corpus{$text}}) {
		
			next if $_ == 0;
			
			push @{$inv{DIF}}, 1/$_;
		}	
	
		for my $p (qw/DOC COR RAT DIF/) {

			$param{"F_P_${p}_INV_$text"} = sum(@{$inv{$p}});
		}
	}

	for my $p (qw/DOC COR RAT DIF/) {

		$param{"F_P_${p}_INV_BOTH"} = $param{"F_P_${p}_INV_AEN"} + $param{"F_P_${p}_INV_BC"};
	}
	
	# round to 8 decimal places
	
	for (values %param) { $_ = sprintf("%.8f", $_) }
		
	return @param{@cols};
}

#
# calculate the tf*idf scores of matching words, if there are any
#
# There are two series, based on two different methods of calculating
# the 'idf' component:
#
# _M_P_ is calculated using every individual phrases in the corpus as a 'document'
# _M_T_ is calculated using each text (i.e. each .tess file) as a 'document'
#
# _SUM_ is the cumulative tf-idf of matching words
# _AVG_ is the mean tf-idf of matching words
# _MAX_ is the max tf-idf of matching words

sub tfidf_match {
	
	# benchmark record

	my ($rec) = shift;
	
	# results of tess mini-search

	my %match = %{$$rec{TESSMATCH}{STEM}};
	
	# column headings for results
	
	my @cols = (

		'TF_M_P_AVG_BC', 'TF_M_P_AVG_AEN', 'TF_M_P_AVG_BOTH',
		'TF_M_P_SUM_BC', 'TF_M_P_SUM_AEN', 'TF_M_P_SUM_BOTH',
		'TF_M_P_MAX_BC', 'TF_M_P_MAX_AEN', 'TF_M_P_MAX_BOTH',
	
		'TF_M_T_AVG_BC', 'TF_M_T_AVG_AEN', 'TF_M_T_AVG_BOTH',
		'TF_M_T_SUM_BC', 'TF_M_T_SUM_AEN', 'TF_M_T_SUM_BOTH',
		'TF_M_T_MAX_BC', 'TF_M_T_MAX_AEN', 'TF_M_T_MAX_BOTH'
	);
	
	# when passed non-rec, return column headings
	
	unless ($rec) {return @cols}
	
	# results
		
	my %param;
	
	# don't proceed if there are no matching words
	
	if (scalar(keys %match) == 0) {
		
		return {};
	}
		
	# tf-idf is calculated using each of the two idf values
	
	my %values_phrase;
	my %values_text;
		
	for my $text (qw/BC AEN/) {
		
		# the frequency of each matching word, in the phrase
		
		my %tf;
		
		# for each matching feature (could be stems)
		
		for my $feature (keys %match) {
		
			# calculate a separate score 
			# for each word form matched by that feature
		
			for my $i (@{$match{$feature}{$text}}) {
				
				my $word = $$rec{$text . "_PHRASE"}[$i];
			
				$tf{$word} += 1/scalar(@{$$rec{$text . "_PHRASE"}});
			}
		}
	
		#
		# now multiply by one of the two idfs
		#
		
		# these hold cumulative scores
		
		my $total_phrase = 0;
		my $total_text = 0;
		
		for my $word (keys %tf) {
			
				push @{$values_phrase{$text}}, $tf{$word} * $idf_by_phrase{$word};
				push @{$values_text{$text}}, $tf{$word} * $idf_by_text{$word};
				
				$total_phrase += $tf{$word} * $idf_by_phrase{$word};
				$total_text += $tf{$word} * $idf_by_text{$word};
		}
		
		# the average tf-idf
		
 		$param{"TF_M_P_AVG_$text"} = mean(@{$values_phrase{$text}});
		$param{"TF_M_T_AVG_$text"} = mean(@{$values_text{$text}});
		
		# the cumulative tf-idf
		
		$param{"TF_M_P_SUM_$text"} = $total_phrase;
		$param{"TF_M_T_SUM_$text"} = $total_text;
	}

	$param{TF_M_P_SUM_BOTH} = $param{TF_M_P_SUM_AEN} + $param{TF_M_P_SUM_BC};
	$param{TF_M_T_SUM_BOTH} = $param{TF_M_T_SUM_AEN} + $param{TF_M_T_SUM_BC};
	
	$param{TF_M_P_AVG_BOTH} = mean(@{$values_phrase{BC}}, @{$values_phrase{AEN}});
	$param{TF_M_T_AVG_BOTH} = mean(@{$values_text{BC}}, @{$values_text{AEN}});
	
	
	#
	# now just the max scores
	#
	
	for my $text qw(BC AEN) {
	
 		@{$values_phrase{$text}} = sort {$a <=> $b} @{$values_phrase{$text}};
 		@{$values_text{$text}} = sort {$a <=> $b} @{$values_text{$text}};

 		$param{"TF_M_P_MAX_$text"} = $values_phrase{$text}[-1];
		$param{"TF_M_T_MAX_$text"} = $values_text{$text}[-1];
	}
	
	$param{TF_M_P_MAX_BOTH} = $param{TF_M_P_MAX_AEN} > $param{TF_M_P_MAX_AEN} ?
									  $param{TF_M_P_MAX_AEN} : $param{TF_M_P_MAX_AEN};
	$param{TF_M_T_MAX_BOTH} = $param{TF_M_T_MAX_AEN} > $param{TF_M_T_MAX_AEN} ?
									  $param{TF_M_T_MAX_AEN} : $param{TF_M_T_MAX_AEN};

	for (values %param) {$_ = sprintf("%.8f", $_) }	
	
	return @param{@cols};
}

# calculate tf-idf scores for all the words in the phrase,
# not just matching words.
#
# subscripts are as for tfidf_match above

sub tfidf_phrase {

	# benchmark record

	my ($rec) = shift;
		
	# column headings for results
	
	my @cols = (

		'TF_P_P_AVG_BC', 'TF_P_P_AVG_AEN', 'TF_P_P_AVG_BOTH',
		'TF_P_P_SUM_BC', 'TF_P_P_SUM_AEN', 'TF_P_P_SUM_BOTH',
		'TF_P_P_MAX_BC', 'TF_P_P_MAX_AEN', 'TF_P_P_MAX_BOTH',
	
		'TF_P_T_AVG_BC', 'TF_P_T_AVG_AEN', 'TF_P_T_AVG_BOTH',
		'TF_P_T_SUM_BC', 'TF_P_T_SUM_AEN', 'TF_P_T_SUM_BOTH',
		'TF_P_T_MAX_BC', 'TF_P_T_MAX_AEN', 'TF_P_T_MAX_BOTH'
	);
	
	# when passed non-rec, return column headings
	
	unless ($rec) {return @cols}
	
	# results
		
	my %param;
	
	# tf-idf is calculated using each of the two idf values
	
	my %values_phrase;
	my %values_text;
	
	for my $text (qw/BC AEN/) {
		
		# the frequency, in the phrase, of each word in the phrase
		
		my %tf;
		
		for my $word (@{$$rec{$text . "_PHRASE"}}) {
					
			$tf{$word} += 1/scalar(@{$$rec{$text . "_PHRASE"}});
		}
		
		#
		# now multiply by one of the two idfs
		#
		
		# these hold cumulative scores
		
		my $total_phrase = 0;
		my $total_text = 0;
		
		for my $word (keys %tf) {
			
				push @{$values_phrase{$text}}, $tf{$word} * $idf_by_phrase{$word};
				push @{$values_text{$text}}, $tf{$word} * $idf_by_text{$word};
				
				$total_phrase += $tf{$word} * $idf_by_phrase{$word};
				$total_text += $tf{$word} * $idf_by_text{$word};
		}
		
		# the average tf-idf

 		$param{"TF_P_P_AVG_$text"} = mean(@{$values_phrase{$text}});
		$param{"TF_P_T_AVG_$text"} = mean(@{$values_text{$text}});

		# the cumulative tf-idf

		$param{"TF_P_P_SUM_$text"} = $total_phrase;
		$param{"TF_P_T_SUM_$text"} = $total_text;

	}

	$param{TF_P_P_AVG_BOTH} = mean(@{$values_phrase{BC}}, @{$values_phrase{AEN}});
	$param{TF_P_T_AVG_BOTH} = mean(@{$values_text{BC}},   @{$values_text{AEN}});

	$param{TF_P_P_SUM_BOTH} = $param{TF_P_P_SUM_AEN} + $param{TF_P_P_SUM_BC};
	$param{TF_P_T_SUM_BOTH} = $param{TF_P_T_SUM_AEN} + $param{TF_P_T_SUM_BC};
	
	#
	# now just the max scores
	#
	
	for my $text qw(BC AEN) {
	
 		@{$values_phrase{$text}} = sort {$a <=> $b} @{$values_phrase{$text}};
 		@{$values_text{$text}} = sort {$a <=> $b} @{$values_text{$text}};

		$param{"TF_P_P_MAX_$text"} = $values_phrase{$text}[-1];
		$param{"TF_P_T_MAX_$text"} = $values_text{$text}[-1];
	}

	$param{TF_P_P_MAX_BOTH} = $param{TF_P_P_MAX_AEN} > $param{TF_P_P_MAX_AEN} ?
									  $param{TF_P_P_MAX_AEN} : $param{TF_P_P_MAX_AEN};
	$param{TF_P_T_MAX_BOTH} = $param{TF_P_T_MAX_AEN} > $param{TF_P_T_MAX_AEN} ?
									  $param{TF_P_T_MAX_AEN} : $param{TF_P_T_MAX_AEN};

	for (values %param) {$_ = sprintf("%.8f", $_) }	

	return @param{@cols};
}

#
# some distance scores
#
# SPAN_ is the distance between the furthest two matching words
# D_F_DOC_ is the distance between the lowest-freq words using doc-specific freqs
# D_F_COR_ is the distance between the lowest-freq words using corpus-wide freqs
# D_TF_P_ is the distance between the two highest tfidf words using phrase-based tfidf
# D_TF_T_ is the distance between the two highest tfidf words using text-based tfidf

sub dist {

	# benchmark record

	my ($rec) = shift;
	
	# results of tess mini-search

	my %match = %{$$rec{TESSMATCH}{STEM}};
	
	# column headings for results
	
	my @cols = (

		'SPAN_BC',    'SPAN_AEN',
		'D_F_DOC_BC', 'D_F_DOC_AEN',
		'D_F_COR_BC', 'D_F_COR_AEN',
		'D_TF_P_BC',  'D_TF_P_AEN',
		'D_TF_T_BC',  'D_TF_T_AEN'
	);
	
	# when passed non-rec, return column headings
	
	unless ($rec) {return @cols}
	
	# results
	
	my %param;
	
	# mark the positions of matching words

	my %mark;
	
	for my $text qw(BC AEN) {
	
		# calculate tf-idf for each matching word,
		# used later to measure dist between the 
		# two most "interesting" words.
		
		# cf. the tfidf subs above
		
		my %tfidf;
		
		for my $feature (keys %match) {
				
			for my $i (@{$match{$feature}{$text}}) {
				
				my $word = $$rec{$text . "_PHRASE"}[$i];
			
				# I shortened this a bit from what I have above
			
				$tfidf{$word}{phrase} += $idf_by_phrase{$word} / scalar(@{$$rec{$text . "_PHRASE"}});
				$tfidf{$word}{text}   += $idf_by_text{$word}   / scalar(@{$$rec{$text . "_PHRASE"}});
			}
		}
	
		#
		# mark the position of matching words
		#
		
		for my $feature (keys %match) {
				
			for my $i (@{$match{$feature}{$text}}) {
				
				my $word = $$rec{$text . "_PHRASE"}[$i];
						
				$mark{$i}{tfidf_phrase} = $tfidf{$word}{phrase};
				$mark{$i}{tfidf_text} = $tfidf{$word}{text};
				$mark{$i}{freq_doc} = $freq{word}{$text}{$word};
				$mark{$i}{freq_corpus} = $freq{word}{ALL}{$word};
			}
		}
		
		# skip this text unless there are at least two matching words

		next if scalar(keys %mark) < 2;
		
		#
		# calculate distances
		#
		
		# span across all matching words
		
		my @marked = sort { $a <=> $b } keys %mark;
		
		$param{"SPAN_$text"} = abs($marked[-1] - $marked[0]);
		
		# distance between two lowest frequency words
		
		@marked = sort { $mark{$a}{freq_doc} <=> $mark{$b}{freq_doc} } keys %mark;
		
 		$param{"D_F_DOC_$text"} = abs($marked[1] - $marked[0]);
		
		@marked = sort { $mark{$a}{freq_corpus} <=> $mark{$b}{freq_corpus} } keys %mark;
		
 		$param{"D_F_COR_$text"} = abs($marked[1] - $marked[0]);
		
		# distance between two highest tfidf words
		
		@marked = sort { $mark{$b}{tfidf_phrase} <=> $mark{$a}{tfidf_phrase} } keys %mark;
		
 		$param{"D_TF_P_$text"} = abs($marked[1] - $marked[0]);

		@marked = sort { $mark{$b}{tfidf_text} <=> $mark{$a}{tfidf_text} } keys %mark;
		
 		$param{"D_TF_T_$text"} = abs($marked[1] - $marked[0]);
	}
			
	$param{SPAN_BOTH} = $param{SPAN_AEN} + $param{SPAN_BC}
			if defined $param{SPAN_AEN} and $param{SPAN_BC};
	
	$param{D_F_DOC_BOTH} = $param{D_F_DOC_AEN} + $param{D_F_DOC_BC}
			if defined $param{D_F_DOC_AEN} and $param{D_F_DOC_BC};
			
	$param{D_F_COR_BOTH} = $param{D_F_COR_AEN} + $param{D_F_COR_BC}
 			if defined $param{D_F_COR_AEN} and $param{D_F_COR_BC};

	$param{D_TF_P_BOTH} = $param{D_TF_P_AEN} + $param{D_TF_P_BC}
			if defined $param{D_TF_P_AEN} and $param{D_TF_P_BC};
			
	$param{D_TF_T_BOTH} = $param{D_TF_T_AEN} + $param{D_TF_T_BC}
			if defined $param{D_TF_T_AEN} and $param{D_TF_T_BC};

	return @param{@cols};
}

#
# edit distance
#
# uses a Levenshtein distance function I got off the web
#

sub edit_dist {

	# benchmark record
	
	my $rec = shift;
	
	# column headings for results

	my @cols = ('LD');
	
	# when passed non-rec, return column headings
	
	unless ($rec) {return @cols}
	
	# results
	
	my %param;

	my $phrase_target = join(" ", @{$$rec{BC_PHRASE}});
	my $phrase_source = join(" ", @{$$rec{AEN_PHRASE}});
	
	$param{LD} = levenshtein($phrase_source, $phrase_target);
	
	return @param{@cols};
}

#
# number of matching character-ngrams
# shared between the two phrases
#

sub count_chr_ngram {
	
	# benchmark record
	
	my $rec = shift;
	
	# values of n for which to calculate n-grams
	
	my @n = (1, 2, 3);
	
	# column headings for results

	my @cols = map { sprintf("CHR_%i_GR_CNT", $_) } @n;
	
	# when passed non-rec, return column headings
	
	unless ($rec) {return @cols}
	
	# results
	
	my %param;
	
	for my $n (@n) {
	
		# this will hold counts
	
		my %count;
	
		# the total length, for normalization
	
		my $total;
	
		for my $text qw(BC AEN) {
	
			for my $word (@{$$rec{$text . "_PHRASE"}}) {
			
				# count the word length towards the total
			
				$total += length($word);
			
				# but can't count ngrams if length < n
		
				next if length($word) < $n;
			
				# count every ngram
			
				for (my $i=0; $i <= length($word) - $n; $i++) {
			
					my $ngram = substr($word, $i, $n);
				
					# add one for each in BC, subtract for each in AEN
				
					$count{$ngram} += ($text eq 'BC' ? 1 : -1);
				}
			}
		}
	
		# see what's left over after common ones are cancelled out
	
		my $remnant;
	
		for (values %count) {
	
			$remnant += abs($_);
		}
		
 		$param{"CHR_${n}_GR_CNT"} = sprintf("%.08f", 1-$remnant/$total);
	}
	
	return @param{@cols};
}

#
# check the similarity of two strings' ngram frequencies
#

sub freq_chr_ngram{
	
	# benchmark record
	
	my $rec = shift;
	
	# values of n for which to calculate n-grams
	
	my @n = (1, 2, 3);
	
	# column headings for results

	my @cols = map { sprintf("CHR_%i_GR_FRQ", $_) } @n;
	
	# when passed non-rec, return column headings
	
	unless ($rec) {return @cols}
	
	# results
	
	my %param;
	
	for my $n (@n) {
	
		# this will hold counts
	
		my %freq;
		
		for my $text qw(BC AEN) {
			
			my %n_count;
			my %n_less1_count;
	
			for my $word (@{$$rec{$text . "_PHRASE"}}) {
						
				# can't count ngrams if length < n
		
				next if length($word) < $n;
			
				# count every ngram, and every (n-1)gram
			
				for (my $i=0; $i <= length($word) - $n; $i++) {
							
					$n_count{substr($word, $i, $n)}++;
					
					if ($n > 1) {
						$n_less1_count{substr($word, $i, $n-1)}++;
					}
				}
				
				if ($n > 1) {
					$n_less1_count{substr($word, 1-$n, $n-1)}++;
				}
				
				# divide the count of each ngram by that 
				# of its respective (n-1)gram
				
				for (keys %n_count) {
				
					my $denom = $n > 1 ? $n_less1_count{substr($_, 0, $n-1)} : 1;

					$freq{$_}{$text} = $n_count{$_}/$denom;
				}
			}
		}
		
		# fill in the gaps with zeros
		# -- is there a better way to do this?
		
		for my $ngram (keys %freq) {
			
			for my $text qw(BC AEN) {
			
				unless (defined $freq{$ngram}{$text}) { $freq{$ngram}{$text} = 0 }
			}
		}
	
		my $sim = cosim(\%freq);

		$param {"CHR_${n}_GR_FRQ"} = $sim;
	}
	
	return @param{@cols};
}

#
# semantic similarity
#

sub semantic {

	# benchmark record
	
	my $rec = shift;
		
	# column headings for results

	my @cols = (
	
		'SEM'		# semantic similarity
	);
		
	# when passed non-rec, return column headings
	
	unless ($rec) {return @cols}
	
	# results
		
	my %param;
	
	# tally english words appearing in defs
	
	my %tag_count;
	
	for my $text qw(BC AEN) {
	
		for my $word (@{$$rec{$text . "_PHRASE"}}) {
		
			next unless defined $semantic{$word};
			
			for my $tag (keys %{$semantic{$word}}) {
			
				$tag_count{$tag}{$text} += $semantic{$word}{$tag};
			}
		}
	}
	
	for my $tag (keys %tag_count) {
		
		for my $text qw(BC AEN) {
		
			unless (defined $tag_count{$tag}{$text}) { $tag_count{$tag}{$text} = 0 }
		}
	}
	
	my $sim = (keys %tag_count > 0 ? cosim(\%tag_count) : 0);
	
	$param{SEM} = $sim;
	
	return @param{@cols};
}

#
# other subroutines
#

# does a Tesserae-style search on two phrases
# the phrases should be passed as two array refs
# with the arrays containing lists of words

sub TessSearch {

	my ($label_target, $phrase_target, $label_source, $phrase_source) = @_;
	
	# these arrays of words are the two phrases to be searched
	
	my @target = @$phrase_target;
	my @source = @$phrase_source;
	
	# these index the words by position
	
	my %w_index_source;
	my %w_index_target;
	
	# these index stems by position
	
	my %s_index_source;
	my %s_index_target;
	
	# this stores the match; 
	# this is what the sub will return at the end.
	
	my %match = ( WORD => {}, STEM => {} );
	
	# populate the indices
	
	for my $i (0..$#source) {
		
		# index by word
		
		my $word = $source[$i];
	
		push @{$w_index_source{$word}}, $i;
		
		# index by stem if we can find any
		
		for my $stem (@{stems($word)}) {
			
			push @{$s_index_source{$stem}}, $i;
		}		
	}
	
	for my $i (0..$#target) {
		
		# index by word
		
		my $word = $target[$i];
	
		push @{$w_index_target{$word}}, $i;
		
		# index by stem if we can find any
		
		for my $stem (@{stems($word)}) {
			
			push @{$s_index_target{$stem}}, $i;
		}
	}
	
	# compare the indices to see what words, stems
	# are found in both phrases
	
	my %w_count_by_text;
	my %s_count_by_text;
	
	for (keys %w_index_target, keys %w_index_source) { $w_count_by_text{$_}++ }
	for (keys %s_index_target, keys %s_index_source) { $s_count_by_text{$_}++ }
	
	my @w_shared = grep {$w_count_by_text{$_} == 2} keys %w_count_by_text;
	my @s_shared = grep {$s_count_by_text{$_} == 2} keys %s_count_by_text;
	
	# record the locations of common words, stems
	
	for (@w_shared) {
	
		$match{WORD}{$_}{$label_target} = $w_index_target{$_};
		$match{WORD}{$_}{$label_source} = $w_index_source{$_};
	}

	for (@s_shared) {
	
		$match{STEM}{$_}{$label_target} = $s_index_target{$_};
		$match{STEM}{$_}{$label_source} = $s_index_source{$_};
	}

	return \%match;
}


#
#  Levenshtein distance
#
#  This and the next sub (min) were copied together
#  from http://www.merriampark.com/ldperl.htm

# Return the Levenshtein distance (also called Edit distance) 
# between two strings
#
# The Levenshtein distance (LD) is a measure of similarity between two
# strings, denoted here by s1 and s2. The distance is the number of
# deletions, insertions or substitutions required to transform s1 into
# s2. The greater the distance, the more different the strings are.
#
# The algorithm employs a proximity matrix, which denotes the distances
# between substrings of the two given strings. Read the embedded comments
# for more info. If you want a deep understanding of the algorithm, print
# the matrix for some test strings and study it
#
# The beauty of this system is that nothing is magical - the distance
# is intuitively understandable by humans
#
# The distance is named after the Russian scientist Vladimir
# Levenshtein, who devised the algorithm in 1965
#

sub levenshtein
{
    # $s1 and $s2 are the two strings
    # $len1 and $len2 are their respective lengths
    #
    my ($s1, $s2) = @_;
    my ($len1, $len2) = (length $s1, length $s2);

    # If one of the strings is empty, the distance is the length
    # of the other string
    #
    return $len2 if ($len1 == 0);
    return $len1 if ($len2 == 0);

    my %mat;

    # Init the distance matrix
    #
    # The first row to 0..$len1
    # The first column to 0..$len2
    # The rest to 0
    #
    # The first row and column are initialized so to denote distance
    # from the empty string
    #
    for (my $i = 0; $i <= $len1; ++$i)
    {
        for (my $j = 0; $j <= $len2; ++$j)
        {
            $mat{$i}{$j} = 0;
            $mat{0}{$j} = $j;
        }

        $mat{$i}{0} = $i;
    }

    # Some char-by-char processing is ahead, so prepare
    # array of chars from the strings
    #
    my @ar1 = split(//, $s1);
    my @ar2 = split(//, $s2);

    for (my $i = 1; $i <= $len1; ++$i)
    {
        for (my $j = 1; $j <= $len2; ++$j)
        {
            # Set the cost to 1 iff the ith char of $s1
            # equals the jth of $s2
            # 
            # Denotes a substitution cost. When the char are equal
            # there is no need to substitute, so the cost is 0
            #
            my $cost = ($ar1[$i-1] eq $ar2[$j-1]) ? 0 : 1;

            # Cell $mat{$i}{$j} equals the minimum of:
            #
            # - The cell immediately above plus 1
            # - The cell immediately to the left plus 1
            # - The cell diagonally above and to the left plus the cost
            #
            # We can either insert a new char, delete a char or
            # substitute an existing char (with an associated cost)
            #
            $mat{$i}{$j} = min([$mat{$i-1}{$j} + 1,
                                $mat{$i}{$j-1} + 1,
                                $mat{$i-1}{$j-1} + $cost]);
        }
    }

    # Finally, the Levenshtein distance equals the rightmost bottom cell
    # of the matrix
    #
    # Note that $mat{$x}{$y} denotes the distance between the substrings
    # 1..$x and 1..$y
    #
    return $mat{$len1}{$len2};
}

# minimal element of a list

sub min {

	my @list = @{$_[0]};
	
	my $min = $list[0];

	for my $i (@list) {
    
		$min = $i if ($i < $min);
	}
	
	return $min;
}

# do a cosine similarity measure

sub cosim {
	
	my $href = shift;
	my %matrix = %$href;
	
	# calculate a distance between the two vectors
	# using the cosine similarity measure
	
	my $dot_product;
	my ($eucl_dist_BC, $eucl_dist_AEN);
	
	for (keys %matrix) {
		
		$dot_product   += $matrix{$_}{BC} * $matrix{$_}{AEN};
		$eucl_dist_BC  += $matrix{$_}{BC}**2;
		$eucl_dist_AEN += $matrix{$_}{AEN}**2;
	}
	
	$eucl_dist_BC  = $eucl_dist_BC**0.5;
	$eucl_dist_AEN = $eucl_dist_AEN**0.5;
	
	if ($eucl_dist_BC * $eucl_dist_AEN == 0) { return 0 }

	my $sim = sprintf("%.08f", $dot_product/($eucl_dist_BC * $eucl_dist_AEN));
	
	return $sim;
}

# print an array as a comma-separated row

sub format_row {

	my ($delim, $ref) = @_;
	
	my @row = @$ref;
	
	# for each parameter requested...
	
	for (@row) {	
		
		# replace missing values with "NA"

		unless (defined $_)		{ $_ = "NA" };

		# put anything that isn't a number in quotation marks

		if (/[^0-9\.-]/) {
			$_ = '"' . $_ . '"';
		}
	}
	
	# format as comma-separated row

	my $row = join($delim, @row) . "\n";
	
	return $row;
}


# return the mean of a list of values

sub mean {

	my @val = @_;
	
	if ($#val < 0) { return "NA" }
	
	my $sum = sum(@val);
		
	my $mean = $sum / scalar(@val);
	
	return $mean;
}

sub sum {

	my @val = @_;
	
	if ($#val < 0) { return "NA" }
	
	my $sum;
	
	for (@val) { $sum += $_ };
	
	return $sum;
}

sub stems {

	my $form = shift;
	
	my @stems;
	
	if (defined $stem{$form}) {
	
		@stems = @{$stem{$form}};
	}
	else {
	
		@stems = ($form);
	}
	
	return \@stems;
}

sub syns {

	my $form = shift;
	
	my %syns;
	
	for my $stem (@{stems($form)}) {
	
		if (defined $syn{$stem}) {
		
			for (@{$syn{$stem}}) {
			
				$syns{$_} = 1;
			}
		}
	}
	
	return [keys %syns];
}

