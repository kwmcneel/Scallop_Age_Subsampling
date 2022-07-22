# Scallop_Age_Subsampling
Subsampling Methods to Identify Which Scallops to Age

This script is a standalone file that subsamples observer or survey scallop data and is meant to be ran as a whole using CTRL+SHIFT+ENTER. The workflow also checks for
subfolders in the project and adds them if they are missing. Added excel templates are filled with formatted data and those are used to populate the ADU database
with lists of specimens to age and measure. The files output are lists of errors or rows missing data (Thrownout), used invoices and field data for the selected
specimens, a list of specimens to measure, and excluded specimens from the subsampling routine.

Graphs of the outliers and subsampling summaries are also produced, but are only available in Rstudio.

We are using renv to track package versions and renv::restore() can be used to get packages back to the recorded version. There are other renv functions that could help
initiate or update packages.

After running the scripts, the "Scallop.invoice..." file in the Invoices folder, "Scallop.Field_Data..." file in the Field_Data folder, and the "ToMeasure..." file in the 
Line_Profile_list folder can be sent via email or OASIS to ADU personnel to initiate sample processing. 
