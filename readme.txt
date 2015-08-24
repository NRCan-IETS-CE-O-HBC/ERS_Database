This repository contains 3 Perl scripts that manipulate the data in the ERS database.

1. Perl-parse-script.pl

* THIS SCRIPT's Functionality has been supplanted by split_D_E_audit_results.pl *

This script parses the full EGH database for only those columns of interest for the 
particular project. It reads in the full EGH database "evaluations_d_e.csv" 
and writes the parsed data to a text file.

Usage: 

   Perl-parse-script.pl <input csv file> 

2. Perl-split_D_E_audit_results.pl
This script reads in the output file from Perl-parse-script.pl and writes 
out two text files.
	"split-results_D_audit.txt" contains only D-audit data
    "split-results_E_audit.txt" contains only E-audit data

3. combine_D_E.pl
This script takes as input the previously split text files
	"split-results_D_audit.txt"
    "split-results_E_audit.txt"
It searches for a match on the House_ID from the D-audit file in the E-audit file. If a match 
is found, it writes the D-audit and E-audit data on one line of the output file.


