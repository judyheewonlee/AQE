Purpose:  Bioinformatics and Computational Biology project for BCB330
Version:  1.0
Date:     2018 03
Author:   Judy Heewon Lee (heewon.lee@mail.utoronto.ca)
Versions: 1.0 First Implementation of AQE

# =========================================================================== #

AQE is a alignment quality evaluation tool that utilizes the BaliBASE suite 
to perform different functions such as evaluating the quality of an alignment 
algorithm, retrieving reference sequences or retrieving reference alignments.

Evaluating alignments is critical in the field of computational biology 
since it validates the many modern alignment algorithms being developed today.
The AQE package was developed to simplify the process of validating alignments 
using reference databases such as BaliBASE. 

A sample script is provided in the "/data" folder and is labelled 
"sampleScript.R". The script provides sample uses of each function in AQE.
Additionally, there is a data model provided in the same folder as well to
aid the understanding of how the database (stored in referenceDB.rda)
generated by AQE is structured.

To install AQE, you can simply install the the zip file on CRAN, use the
'install.packages' functions in R, or import the package from GitHub. 
The link is provided here: www.github.com/judyheewonlee/AQE.

If there are any concerns, please e-mail heewon.lee@mail.utoronto.ca.


