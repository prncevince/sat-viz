H5="build/data/h5.R"

Rscript $H5 input $1
Rscript $H5 secsys_list $1
Rscript $H5 prisys_list $1
Rscript $H5 prisysout_list $1
Rscript $H5 lightpolys_list $1
exit