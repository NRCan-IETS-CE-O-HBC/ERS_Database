#!/bin/perl

#my $file="EGH_first_line.txt"; 

my $file; 
if  ( $ARGV[0] ) { $file = $ARGV[0];  } 
else {$file = "evaluations_d_e.csv"; }

# Set to 1 to recover everything. 

my $recover_everthing = 1; 

my $file_out ="EGH_parsed_results.txt"; 


open (FILEIN, $file) or die ("could not open $file\n"); 
open (FILEOUT, ">$file_out") or die ("could not open $file_out\n"); 
$line_no = 0; 

my $line_buffer_count = 0; 
my $output =""; 



# Loop through file. 
while ( my $line = <FILEIN> ){

    # Remove spaces from txt. 
	$line =~ s/([^\"]),([^\"])/$1_$2/g;

		
	if ( $line_buffer_count > 4999 ){

		print " Line: $line_no / $line_buffer_count  \n";

	
		#Write out content. 
	    print FILEOUT "$output\n";
		$output = ""; 
	    $line_buffer_count = 0; 
		

	
	}
	
	
	
	
	#print $line ; 

    #Figure out which columns we keep. 
	my $colindex = 0; 
  
	#print "$line HEADER:"; 
  
    my @header = (); 
  
	my @header = split /,/, $line ; 


	if ($line_no == 0) {
  
		foreach my $column (@header) {
			print ">$column\n"; 
			$colindex++;
			if (   $recover_everthing
   		  	       || $column =~ /HOUSE_ID/  
			       || $column =~ /EVAL_TYP/ 
	               || $column =~ /PROVINCE/ 
	               || $column =~ /DECADEBUILT/
 	               || $column =~ /FLOORAREA/
	               || $column =~ /FOOTPRINT/
	               || $column =~ /TYPEOFHOUSE/
	               || $column =~ /STOREYS/
	               || $column =~ /\"FURSSEFF/ 
	               || $column =~ /\"FURNACEFUEL/ 
	               || $column =~ /\"PDHWEF/
	               || $column =~ /\"PDHWFUEL/
                   || $column =~ /\"CEILINS/
	               || $column =~ /\"FNDWALLINS/
	               || $column =~ /\"MAINWALLINS/
 	               || $column =~ /\"WINDOWCODE/
                   || $column =~ /\"AIR50P/
                   || $column =~ /EINCENTIVE/   )  
	
				{ 
				    # Stuff column index in an array to keep later. 
					push @keep_col_index, $colindex-1;
	   
				}
		}
	}



	
	# For this line, store the stfuff you want to keep in a 'output' string. 
	foreach my $column (@keep_col_index ){
		#print "LINE: $line_no COL:  $column \n"; 
		$output .= $header[$column].",";
	}
	
	
    $output.="\n"; 
	
    $line_buffer_count++; 
	$line_no++; 



}

close FILEIN;
close FILEOUT;


# Report: Here's what we kept.

foreach my $column ( @keep_col_index ){
 print "$column | "; 
}

print "\n Wrote $line_no lines to: $file_out \n"; 
