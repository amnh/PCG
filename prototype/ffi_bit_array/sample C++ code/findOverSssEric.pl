# finds the over laps between two files
# where each file contains two column subset relation
# first column is subset, second is superset

$filePath = "/Volumes/Backup/eric/school/Linguistics/LD with William";

open (SSSIN, "$filePath/ld_sss.txt") || die "Can't open infile.\n" ;
open (ERICIN, "$filePath/subsets.txt");
open (OVER, ">$filePath/output/overlap.txt") or die "can't open outfile\n";
open (SSS, ">$filePath/output/sssOnly.txt");
open (ERIC, ">$filePath/output/ericOnly.txt");

while ($line = <SSSIN>)
{
	chomp $line;
	$sss{$line}++;
}

while ($line = <ERICIN>)
{
	chomp $line;
	$eric{$line}++;
}

foreach $i ( keys %sss )
{
	if ( exists $eric{$i} )
	{
		delete $sss{$i};
		delete $eric{$i};
		$over{$i}++;
	}
}

foreach $j ( keys %eric )
{
	if ( exists $sss{$j} )
	{
		delete $eric{$j};
		delete $sss{$j};
		$over{$j}++;
	}
}

foreach $i ( sort {$a<=>$b} keys %sss )
{
	print ( SSS "$i\n" );
}

foreach $i ( sort {$a<=>$b} keys %eric )
{
	print ( ERIC "$i\n" );
}

foreach $i ( sort {$a<=>$b} keys %over )
{
	print ( OVER "$i\n" );
}