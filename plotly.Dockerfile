ARG TAG
FROM SAVI-DATA:$TAG

# plotly.js fix
RUN Rscript \
    -e "f <- 'renv/library/R-3.6/x86_64-pc-linux-gnu/plotly/htmlwidgets/lib/plotlyjs/plotly-latest.min.js'" \
    -e "p <- readLines(f)" \
    -e "p[[7]] <- sub(pattern = 't\\\.getTimezoneOffset\\\(\\\)\\\*p', replacement = '0', x = p[[7]])" \
    -e "writeLines(paste(p, collapse = '\n'), f)"
