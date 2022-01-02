# prrtvrowcol
Subroutine in RPGLE, prRtvRowCol, to obtain the column and row of all fields in a DSPF display file for the IBM i-Series AS400. 

Call the subroutine with 3 parameters:
1. name of the DSPF file (Input parameter).
2. name of the requested FORM in that DSPF (Input parameter).
3. a table of 999 rows. each row with 4 elements (Output parameter):
  a. Field name
  b. Row number of the field
  c. Column number of the field
  d. Length of the field
