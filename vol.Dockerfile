ARG TAG
FROM savi:$TAG
VOLUME /app/savi/data

# shiny
COPY server.R ui.R ./
COPY r r/
COPY www www/
CMD Rscript -e "shiny::runApp(host = '0.0.0.0', port = strtoi(Sys.getenv('PORT')))"
