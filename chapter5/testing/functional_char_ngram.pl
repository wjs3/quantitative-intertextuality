# functional_char_ngram.pl
# Generate a list of the top character-level n-grams in a text. This
# information is roughly analogous to primitive sound.
#
# Assumes all markup/metadata has been removed
#
# Usage: $ perl functional_char_ngram.pl -l <limit> -i <input text> -w <number> -n <size>
#
# <limit> is the number of lines of the input text to consider
# <input text> is the input text
# <number> is the number of desired character-level n-grams
# <size> is the length of the n-gram
#
# Example: top ten bi-grams in the elegiacs of Catullus
# $ perl functional_char_ngram.pl -l 620 -i catullus_elegiac -w 10 -n 2
# 423 er 0.241025641025641
# 384 qu 1
# 328 is 0.168377823408624
# 318 re 0.286744815148783
# 304 um 0.177881802223523
# 274 es 0.156125356125356
# 261 it 0.133983572895277
# 257 am 0.178720445062587
# 257 tu 0.216512215669756
# 238 in 0.12217659137577

use Getopt::Std;

%options=();
getopts("w:i:l:n:",\%options);

die "Missing -w" unless $options{w};
die "Missing -i" unless $options{i};
die "Missing -l" unless $options{l};
die "Missing -n" unless $options{n};

$number = $options{w};
$author_file = $options{i};
$limit = $options{l};
$size = $options{n};

# filter the following content:
# - punctuation
@sorted = wordlist($author_file, $limit);

# collect n-grams in a master list
@ngrams = collect_ngrams($size, @sorted);

# $foo = @ngrams;
# for ($i = 0; $i < @ngrams; $i++) {
#    print "$ngrams[$i]\n";
# }

# generate a frequency count for each unique n-gram in the content array
@ngram_counts = ngram_count(@ngrams);

# generate a relative probability for each n-gram
@ngram_prob = relative_freq($size, $number, @ngram_counts);

for ($i = 0; $i < @ngram_prob; $i++) {
   print "$ngram_prob[$i]\n";
}

# process each identified file
sub wordlist {

   $author_file = shift;
   $limit = shift;
   @sorted;

   @lines;
   open( FILE, "< $author_file" ) or die "Can't open $author_file : $!";

   $count = 0;
   while( <FILE> ) {
  
      $count++;
      if ($count > $limit) {
         last;
      }
      
      # remove any punctuation
      s/[[:punct:]]//g;

      next if /^(\s)*$/;  # skip blank lines

      chomp;              # remove trailing newline characters
      
      # replace tabs with spaces
      s/\t/ /g;
      
      # replace each space with a new line
      s/  / /g;
      s/ /\n/g;

      # remove the carriage return
      s/\r//g;
	
      push @lines, $_;
   }

   close FILE;

   # create a new wordlist array, with one word per element
   for ($i = 0; $i < @lines; $i++) {
      @temp = split(/\n/, $lines[$i]);
      @unsorted_all = (@unsorted_all, @temp);
   }

   @unsorted;
   $data_count = 0;
   for ($i = 0; $i < @unsorted_all; $i++) {
   	if ($unsorted_all[$i]) {
		$unsorted[$data_count] = $unsorted_all[$i];
		$data_count++;
	}
   }

   @sorted = (sort { lc($a) cmp lc($b) } @unsorted);

   # convert the sorted array to lower case
   for ($i = 0; $i < @sorted; $i++) {
      $sorted[$i] = lc($sorted[$i]);
   }

   @sorted;
}

sub collect_ngrams {

   $size = shift;
   @words = @_;
   @ngrams;

   $ngram_count = 0;
   for ($i = 0; $i < @words; $i++) {
      @temp = split(//, $words[$i]);

      $temp_size = @temp;
      if ($temp_size > $size - 1) {
         for ($j = 0; $j < $temp_size - ($size - 1); $j++) {
	    $string = "$temp[$j]";
	    for ($k = 1; $k < $size; $k++) {
	       $string = "$string" . "$temp[$j + $k]";  
            }
            $ngrams[$ngram_count] = $string;

            $ngram_count++;
         }
      }
   }

   @sorted = (sort { lc($a) cmp lc($b) } @ngrams);

   return @sorted;
}

# return an array of unique n-grams and n-gram frequency counts (one 
# line with each per word)
sub ngram_count {

   @sorted = @_;
   @sorted_counts;
   
   $total_ngram_count = @sorted;
   $ngram = $sorted[0];          # prime the pump
   $ngram_count = 1; 
   $sorted_counts_pos = 0;
   for ($i = 0; $i < $total_ngram_count; $i++) {

        # first word matches itself - handle this
        if ($i == 0) {
           $ngram_count = 0;
        }

        if (!($ngram eq $sorted[$i]) && $ngram_count != 1) {
           $sorted_counts[$sorted_counts_pos] = "$ngram $ngram_count";
           $sorted_counts_pos++;
           $ngram = $sorted[$i];
           $ngram_count = 1;
        }
        elsif (!($ngram eq $sorted[$i]) && $ngram_count == 1) {
           $ngram = $sorted[$i];
        }
        else {
           $ngram_count++;
        }
   }

   # handle the last entry
   if ($ngram_count != 1) {
      $sorted_counts[$sorted_counts_pos] = "$ngram $ngram_count";
   }

   @sorted_counts;
}

# calculate the relative frequency for each n-gram by counting every 
# occurance of each bigram (previously calculated) and then dividing
# these counts by the sum of all the n-grams that have the same first
# word
sub relative_freq {

   $size = shift;
   $num = shift;
   @ngram_counts = @_;
   @ngram_probs;

   # build a hash containing total counts for each first character
   # of each n-gram set
   my %counts = ();
   for ($i = 0; $i < @ngram_counts; $i++) {
      @temp = split(/ /, $ngram_counts[$i]);
   
      @char_temp =  split(//, $temp[$0]); 

      $string = "$char_temp[0]";
      for ($j = 1; $j < $size - 1; $j++) {
         $string = "$string" . "$char_temp[$j]"; 
      }
   
      if ($counts{"$string"}) {
         $counts{"$string"} = $counts{"$string"} + $temp[1];
      }
      else {
         $counts{"$string"} = $temp[1];
      }
   }

   for ($i = 0; $i < @ngram_counts; $i++) {
      @ngram = split(/ /, $ngram_counts[$i]);

      @char_ngram = split(//, $ngram[$0]);

      $string = "$char_ngram[0]";
      for ($j = 1; $j < $size - 1; $j++) {
         $string = "$string" . "$char_ngram[$j]";
      }

      $prob = $ngram[1] / $counts{"$string"}; 
      $ngram_probs[$i] = "$ngram[0] $prob $ngram[1]";
   }

   # sort by frequency
   $ngram_probs_count = @ngram_probs;
   my %unsorted = ();
   for ($i = 0; $i < $ngram_probs_count; $i++) {
      @temp = split(/ /, $ngram_probs[$i]);
      $unsorted{"$temp[0] $temp[1]"} = $temp[2];
   }

   @sorted = ();
   $i = 0;
   foreach $key (sort {$unsorted{$b} <=> $unsorted{$a}} (keys(%unsorted))) {
      $sorted[$i] = "$unsorted{$key} $key";
      $i++;
   }

   # return the top n entries
   @final_ngrams;
   $sorted_size = @sorted;
   if ($num > $sorted_size) {
      $num = $sorted_size;
   }

   for ($i = 0; $i < $num; $i++) {
      $final_ngrams[$i] = $sorted[$i];
   }

   return @final_ngrams;
}
