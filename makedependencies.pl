#!/bin/perl
# On stdout, create dependencies based on USE and MODULE statements in the fortran source files provided on the command line.
# method: from the files in the argument list, extract dependencies from the USE statements and locate the MODULEs

# Gerard Cats, 27 July 2020

# loop over arguments - must be Fortran files
while ( $file = shift ) {
   open( IN, "<$file") or die "cannot open $file: $!\n";
   $target = $file;
   $target =~ s/\.f.*$/.o/i;	# dependencies ara formulated at the object level
   undef %deps;
   while ( <IN> ) {

# in the parsing of Fortran, it is assumed that USE and MODULE are the first words
# the second word, starting with a letter and ending with a letter or digit
# Be aware in Fortran case and spaces are insignificant

# USE statements: this target depends on the used name
      if ( /^\s*u\s*s\s*e\s*([A-Za-z_][A-Za-z0-9_\s]*[A-Za-z0-9_])/i ) { 	# assumes used names are at least two characters
         $s = lc($1);
         $s =~ s/\s//g;
         $deps{ $s } = 1;						# multiple occurences are collapsed
      }
# MODULE statements: remember this is the source of this module
      if ( /^\s*m\s*o\s*d\s*u\s*l\s*e\s*([A-Za-z_][A-Za-z0-9_\s]*[A-Za-z0-9_])/i ) { 	# assumes module names are at least two characters
         $s = lc($1);
         $s =~ s/\s//g;
         $src { $s } = $target;
      }
   }
   close IN;

# remember the list of prerequisites
   if ( %deps ) {
      for my $j ( sort keys %deps ) {
         $prerq{ $target } .= ";$j";
      }
   }
}
# the prerequisites define the dependants in their keys
for my $j ( sort keys %prerq ) {
   undef $deps;
   @prerq = split(";", $prerq{ $j });
   for (my $jj = 1; $jj <= $#prerq; $jj ++ ) {
      $src = $src { $prerq[ $jj ] };
      if ( $src ) {
         $deps .= " $src";
      } else {
         print stderr "Warning: no source found for $prerq[ $jj ] needed by $j\n";
         print "# $j: $prerq[ $jj ]\t\tNo source found\n";
      }
   }
   if ( $deps ) {
      print "$j: $deps\n";
   }
}
