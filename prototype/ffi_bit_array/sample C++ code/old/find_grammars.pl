# written for Janet
# to find overlapping grammars

use strict;

my ($dir, $filename, $gram1, $gram2, $numArgs, $line);

$numArgs = $#ARGV + 1;
if( $numArgs != 2 )
{
	die "I'm sorry, you need to specify two grammars. \n";
}

$gram1 = $ARGV[0];
$gram2 = $ARGV[1];

if( ($gram1 !~ /[0|1]{13}/) || ($gram2 !~ /[0|1]{13}/) )
{
	die "Grammar names must be 13 characters long and consist of ones and zeroes.\n";
}

if( $gram2 < $gram1 )
{
	$gram1 = $ARGV[1];
	$gram2 = $ARGV[0];
}

use Cwd;
$dir = cwd;

$filename = "sorted grammar overlaps.txt";

open (INPUT, "$dir/$filename") or die "Cannot open $filename: $!\n";

while( $line = <INPUT> )
{
	chomp( $line );
	if( ( $line =~ $gram1 ) && ( $line =~ $gram2 ) )
	{
		die "$line\n";
	}
}

print "One or more grammars not found.\n";