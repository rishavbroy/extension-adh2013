/****************************************************************************

This do-file runs the files that build the final micro-data sets

Final version: David Dorn, May 12, 2020

*****************************************************************************/


* PEW microdata (county level)
do pew_master_builder.do

* Nielsen microdata (CZ level)
do nielsen_master_builder.do

* House election data (county-district level)
do house_master_builder.do

* Presidential election data (county level)
do president_master_builder.do

