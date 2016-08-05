# this program will find subsets in the LD_ids file, in order to verify
# other results

$filePath = "/Volumes/Backup/eric/school/Linguistics/LD with William";
$sub = 0;
$super = 128;


open (IN, "$filePath/ld_ids.txt") or die "can't open ld ids file\n";

while (<IN>)
{
	chomp;
	
	($gram, $sent, $junk) = split ("\t");
	
	if ( $gram == $sub) 
	{
		$0{$sent}++;
	}
	elsif ( $gram == $super )
	{
		$8{$sent}++;
	}
	elsif ( $gram > $super )
	{
		last;
	}
}

foreach $i ( keys %0 )
{
	if (!exists $8{$i})
	{
		print "$sub is not a subset of $super\n";
		last;
	}
}

$j = 364/704;

print "$j\n";
printf "%.5f\n", $j;

$n = .517046;
print "$n\n";
printf "%.5f\n", $n;


	