# Electricity Authority data

Downloaded from https://www.emi.ea.govt.nz/Wholesale/Datasets for various purposes.

## Wholesale Generation Data

 * Downloads all or a subset of files from https://www.emi.ea.govt.nz/Wholesale/Datasets/Generation/Generation_MD/
   + NB: checks to see if we already have them and skips if so (can be over-ridden)
 * Converts from wide to long form
 * Sets Time Period (half hour) properly
 * Saves original wide and converted long form data files to /hum-csafe/Research Projects/GREEN Grid/externalData/EA_Generation_Data/
 * Does not create a fancy report but does save some data quality check plots in the same directory

## Embedded Generation Data

 * Basically as for Wholesale Generation data but gets data from https://www.emi.ea.govt.nz/Wholesale/Datasets/Metered_data/Embedded_generation/ to try to add solar gen to the wholesale gen to get the _whole_ picture (tm)
 
----
[#YMMV](https://en.wiktionary.org/wiki/YMMV).
