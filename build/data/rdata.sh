RDATA="build/data/rdata.R"

Rscript $RDATA geo_df $1
Rscript $RDATA secsys_list $1 &
Rscript $RDATA prisys_list $1 &
Rscript $RDATA prisysout_list $1 &
Rscript $RDATA lightpolys_list $1 &
wait
exit
