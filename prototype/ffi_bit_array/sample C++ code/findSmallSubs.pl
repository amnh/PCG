$filePath = "/Volumes/Backup/eric/school/Linguistics/LD with William";

open (IN, "$filePath/ld_sss.txt") || die "Can't open infile.\n" ;
open (OUT, ">$filePath/ssmallestSubsets.txt") or die "can't open outfile\n";

while (<IN>)
{
	chomp;
	($G1, $G2) = split ("\t");
	
	$subs{$G1}++;
	$supers{$G2}++;
}

foreach $i ( keys %supers )
{
	delete $subs{$i};
}

foreach $j ( sort {$a<=>$b} ( keys %subs ))
{
	print (OUT "$j\n");
}