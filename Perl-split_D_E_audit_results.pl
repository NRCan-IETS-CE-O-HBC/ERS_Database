#!/bin/perl
 
 
# Source file for raw D-E dump. 
my $file_1 = "evaluations_d_e.csv";


# File to contain D records
my $file_D = "split-results_D_audit.txt";

# File to contain E records. 
my $file_E = "split-results_E_audit.txt";
 
 
# Oppen files. 
open (FILEIN1, $file_1) or die ("could not open $file_1\n");

open (FILEOUT_D, ">$file_D") or die ("could not open $file_D\n"); 
open (FILEOUT_E, ">$file_E") or die ("could not open $file_E\n"); 

# Counters
my $line_no        = 0; 
my $line_no_buffer = 0; 
my $eval_col_index = 0;

# output 
my $output   = ""; 
my $output_D = ""; 
my $output_E = ""; 

# Loop through file, 
#  1. Find the columns we want to keep, or optionally keep them all, 
#  2. Determine if this is a D or E record,
#  3. Store results in D or E output, 
#  4. Periodically write D or E output to file. 

while ( my $line = <FILEIN1> ){


  # Escape unnecssary spaces. 
  $line =~ s/([^\"]),([^\"])/$1_$2/g;

  
  my $colindex = 0; 

  # Take the line and split it into columns
  my @columns = split /,/, $line ; 
  
  # On the first line, loop through the columns and get the ones we want 
  # to keep. 
  if ($line_no == 0) {
  
    foreach my $column (@columns) {
     #print ">$column\n"; 
	   $colindex++;
	 
     # Store index (Can be tested to produce a subset of the 
     #              current column list )
	   push @keep_col_index, $colindex-1;
	      
        
     # Also sort the location of the 'eval_type' column - we need this to
     # figure out if it's a d or e audit file
     if ( $column =~ /EVAL_TYP/) {	$eval_col_index =  $colindex -1 ;  }
   
     #if ( $column =~ /HOUSE_ID/) { push @keep_col_index, $colindex-1; }
	   # { push @keep_col_index, $colindex-1;
     #   	$eval_col_index =  $colindex -1 ; }
	   #if ( $column =~ /PROVINCE/) { push @keep_col_index, $colindex-1; }	
	   #if ( $column =~ /DECADEBUILT/) { push @keep_col_index, $colindex-1; }
 	   #if ( $column =~ /FLOORAREA/) { push @keep_col_index, $colindex-1; }
	   #if ( $column =~ /FOOTPRINT/) { push @keep_col_index, $colindex-1; }
	   #if ( $column =~ /TYPEOFHOUSE/) { push @keep_col_index, $colindex-1; }	
	   #if ( $column =~ /STOREYS/) { push @keep_col_index, $colindex-1; }
	   #if ( $column =~ /\"FURSSEFF/) { push @keep_col_index, $colindex-1; }
	   #if ( $column =~ /\"FURNACEFUEL/) { push @keep_col_index, $colindex-1; }	 
	   #if ( $column =~ /\"PDHWEF/) { push @keep_col_index, $colindex-1; }	
  	 #if ( $column =~ /\"PDHWFUEL/) { push @keep_col_index, $colindex-1; }	
     #if ( $column =~ /\"CEILINS/) { push @keep_col_index, $colindex-1; }	 
  	 #if ( $column =~ /\"FNDWALLINS/) { push @keep_col_index, $colindex-1; }	
	   #if ( $column =~ /\"MAINWALLINS/) { push @keep_col_index, $colindex-1; }	
 	   #if ( $column =~ /\"WINDOWCODE/) { push @keep_col_index, $colindex-1; }	
     #if ( $column =~ /\"AIR50P/) { push @keep_col_index, $colindex-1; }  
     #if ( $column =~ /EINCENTIVE/) { push @keep_col_index, $colindex-1; }  	 	  


    }

  }

  if ( $line_no_buffer > 4999 ){
     
     print "Dumping $line_no_buffer lines ($line_no) \n"; 
     
     
     print FILEOUT_D "$output_D";
     print FILEOUT_E "$output_E";
     
     
     
     $output_D = ""; 
     $output_E = ""; 
     
     $line_no_buffer = 0; 
     
  }
  
  
  $output = ""; 
  # Append good columns to a temp string. 
  foreach my $column (@keep_col_index ){
    #print "LINE: $line_no COL:  $column \n"; 
	  $output .= $columns[$column].",";
  }
  $output .= "\n"; 
  # determine if this is a D or E string. 


  #print ">>> $line_no $eval_col_index  $columns[0]  $columns[$eval_col_index] \n";  
  
  # Hader row: 
  if ( $columns[$eval_col_index] =~ /EVAL_TYP/ ) {  $output_D .= $output; $output_E .= $output;  }                                  
  if ( $columns[$eval_col_index] =~ /D/ )        {  $output_D .= $output; }
  if ( $columns[$eval_col_index] =~ /\"E"/ )     {  $output_E .= $output; }
  
  # Empty column header. 
  @columns = (); 

  
  # Increment counter. 
  $line_no++; 
  $line_no_buffer++; 
  
}

foreach my $column ( @keep_col_index ){
 print "$column | "; 
}












