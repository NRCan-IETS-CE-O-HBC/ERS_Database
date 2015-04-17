#!/bin/perl

#my $file="EGH_first_line.txt"; 
my $file = "evaluations_d_e.csv"; 
my $file_out ="EGH_parsed_results.txt"; 
open (FILEIN, $file) or die ("could not open $file\n"); 
open (FILEOUT, ">$file_out") or die ("could not open $file_out\n"); 
$line_no = 0; 

while ( my $line = <FILEIN> ){

$line =~ s/([^\"]),([^\"])/$1_$2/g;

#print $line ; 


my $colindex = 0; 


  
  #print "$line HEADER:"; 
  
  my @header = split /,/, $line ; 


  if ($line_no == 0) {
  
    foreach my $column (@header) {
     #print ">$column\n"; 
	 $colindex++;
     if ( $column =~ /HOUSE_ID/) { push @keep_col_index, $colindex-1; }
	 if ( $column =~ /EVAL_TYP/) { push @keep_col_index, $colindex-1; }
	 if ( $column =~ /PROVINCE/) { push @keep_col_index, $colindex-1; }	
	 if ( $column =~ /DECADEBUILT/) { push @keep_col_index, $colindex-1; }
 	 if ( $column =~ /FLOORAREA/) { push @keep_col_index, $colindex-1; }
	 if ( $column =~ /FOOTPRINT/) { push @keep_col_index, $colindex-1; }
	 if ( $column =~ /TYPEOFHOUSE/) { push @keep_col_index, $colindex-1; }	
	 if ( $column =~ /STOREYS/) { push @keep_col_index, $colindex-1; }
	 if ( $column =~ /\"FURSSEFF/) { push @keep_col_index, $colindex-1; }
	 if ( $column =~ /\"FURNACEFUEL/) { push @keep_col_index, $colindex-1; }	 
	 if ( $column =~ /\"PDHWEF/) { push @keep_col_index, $colindex-1; }	
	 if ( $column =~ /\"PDHWFUEL/) { push @keep_col_index, $colindex-1; }	
     if ( $column =~ /\"CEILINS/) { push @keep_col_index, $colindex-1; }	 
	 if ( $column =~ /\"FNDWALLINS/) { push @keep_col_index, $colindex-1; }	
	 if ( $column =~ /\"MAINWALLINS/) { push @keep_col_index, $colindex-1; }	
 	 if ( $column =~ /\"WINDOWCODE/) { push @keep_col_index, $colindex-1; }	
     if ( $column =~ /\"AIR50P/) { push @keep_col_index, $colindex-1; }  
     if ( $column =~ /EINCENTIVE/) { push @keep_col_index, $colindex-1; }  	 	  


    }

  }

  $output = ""; 
  foreach my $column (@keep_col_index ){
    print "LINE: $line_no COL:  $column \n"; 
	$output .= $header[$column].",";
	
	 
  }
  
  print FILEOUT "$output\n";
  



$line_no++; 
}

foreach my $column ( @keep_col_index ){
 print "$column | "; 
}