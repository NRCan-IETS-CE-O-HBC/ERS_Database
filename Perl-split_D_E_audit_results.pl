#!/bin/perl
 
my $file_1 = "evaluations_d_e.csv";

my $file_D = "split-results_D_audit.txt";
my $file_E = "split-results_E_audit.txt";
 
open (FILEIN1, $file_1) or die ("could not open $file_1\n");

open (FILEOUT_D, ">$file_D") or die ("could not open $file_D\n"); 
open (FILEOUT_E, ">$file_E") or die ("could not open $file_E\n"); 


$line_no = 0; 

my $eval_col_index = 0;

while ( my $line = <FILEIN1> ){

$line =~ s/([^\"]),([^\"])/$1_$2/g;

#print $line ; 


my $colindex = 0; 




  
 
  my @columns = split /,/, $line ; 


  if ($line_no == 0) {
  
    foreach my $column (@columns) {
     #print ">$column\n"; 
	 $colindex++;
     if ( $column =~ /HOUSE_ID/) { push @keep_col_index, $colindex-1; }
	 if ( $column =~ /EVAL_TYP/) { push @keep_col_index, $colindex-1;
        	$eval_col_index =  $colindex -1 ; }
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
	$output .= $columns[$column].",";
	
	 
  }
  print ">>> $eval_col_index $columns[$eval_col_index] \n";  
  if ( $columns[$eval_col_index] =~ /EVAL_TYP/ ) {  print FILEOUT_D "$output\n";
                                                    print FILEOUT_E "$output\n";  }
  if ( $columns[$eval_col_index] =~ /D/ ) {  print FILEOUT_D "$output\n"; }
  if ( $columns[$eval_col_index] =~ /\"E"/ ) {  print FILEOUT_E "$output\n"; }


$line_no++; 
}

foreach my $column ( @keep_col_index ){
 print "$column | "; 
}












