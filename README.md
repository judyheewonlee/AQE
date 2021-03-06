## AQE

###  Alignment Quality Evaluation Tool

-----------------------------------------------

Version: 1.0  
Author: Judy Heewon Lee (heewon.lee@mail.utoronto.ca)  
Versions: 1.0 First Implementation of AQE  

-----------------------------------------------

AQE is a alignment quality evaluation tool that utilizes the BaliBASE suite 
to perform different functions such as evaluating the quality of an alignment 
algorithm, retrieving reference sequences or retrieving reference alignments.

Evaluating alignments is critical in the field of computational biology 
since it validates the many modern alignment algorithms being developed today.
The AQE package was developed to simplify the process of validating alignments 
using reference databases such as BaliBASE. 

A sample script is provided in the "/inst/extdata" folder and is labelled 
"sampleScript.R". The script provides sample uses of each function in AQE.
Additionally, there is a data model provided in the same folder as well to
aid the understanding of how the database (stored in referenceDB.rda)
generated by AQE is structured.

----------------------------------------------
This package follows the structure and process 
suggested by Hadley Wickham in:


  R Packages
  http://r-pkgs.had.co.nz/

-----------------------------------------------
Some useful keyboard shortcuts for package authoring:

Build and Reload Package:  'Cmd + Shift + B'  
Update Documentation:      'Cmd + Shift + D' or devtools::document()  
Check Package:             'Cmd + Shift + E'  

-----------------------------------------------

Load the package with:  
   devtools::install_github("judyheewonlee/AQE")


