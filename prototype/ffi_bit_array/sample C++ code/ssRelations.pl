# using Jaccard measures, find subsets of grammars
# input file is of form b1 i1 s1 b2 i2 s2 j
# where b is a binary number, name of a grammar
# i is an integer, decimal equivalent of above
# s is an integer, size of grammar set
# j is jaccard measure of two grammars

$filePath = "/Volumes/Backup/eric/school/Linguistics/LD with William";

open (IN, "$filePath/jaccard_alpha.txt") || die "Can't open jaccard file.\n" ;
open (OUT, ">$filePath/subsets.txt") or die "can't open outfile\n";

while (<IN>)
{
    $G1="";
    $G2="";
    $size1=0;
    $size2=0;
    $junk ;
    $j=0.0;
 
    chomp ;
    ($G1, $G1dec, $size1, $G2, $G2dec, $size2, $j) = split ("\t") ;
    
    $first = $size1/$size2;
    $second = $size2/$size1;
    
    $second =~ s/(\d\.\d{5}).*/\1/;
    $first =~ s/(\d\.\d{5}).*/\1/;
    $j =~ s/(\d\.\d{5}).*/\1/;
#     print "$G1dec\t$G2dec\n";
#     print "First = $first\nSecond = $second\nJaccard = $j\n";
    
    if ($j == 1) {
	#  print "$G1 $G2 equivalent\n";    
    }
    elsif ($j == 0){
	# print "$G1 $G2 disjoint\n";
    }
    elsif ( $j == $first )
    {
#     	print "yes\n";
      	print (OUT "$G1dec\t$G2dec\n");
#		$G1dec = ord(pack('B13', "$G1")) ;
# 
# 		print "G1 Decimal: $G1dec\n";
# 
# 		$aChar = <STDIN> ;
    } 
    elsif ( $j == $second )
    {
#     	print "yes\n";
		print OUT "$G2dec\t$G1dec\n";
# 		$aChar = <STDIN> ;
# 
#		$G1dec = ord(pack('B13', "$G1")) ;
# 
# 		print "G1 Decimal: $G1dec\n";

    }
    else
    {
	# print "$G1 $G2 properly intersect\n";
    }
}
