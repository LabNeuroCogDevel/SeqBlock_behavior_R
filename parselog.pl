#!/usr/bin/env perl
use strict; use warnings;
my $ntrialsinblock=10;
my $i=0; 
my @a=();
while(<STDIN>){
 chomp;
 # repeat block type for each trial
 if(s/Block order:\s+//i){
   push(@a,($_)x10) for split/ /;
   #print "$i: @a\n";
   next;
 } 
 my @F=split/,/;
 if(/RT/){
   # remove characters, remove extra spaces, remove starting space
   s/[A-Za-z,_]//g;
   s/ +/ /g;
   s/^ //;
   
   # NA if no value
   my @i=split/ /; 

   #print "\n------\n$_\n";
   #print "\t* $#F: '@F'\n\t*$#i: '@i'\n";
   # bad testinging line?
   if( $#i==4 || $i[4] == 0) {
     print STDERR "bad line $. (no resp?): @F\n";
     $i[5]="NA";
   }
   # item 6 (index 5) is 1 correct, 0 error. 
   # dont want to replace incorrect with NA
   $i[$_]||="NA" for(0..4,6..9);

   print join(" ", $a[$i++], $F[0], @i);
   print "\n";
 }
}
