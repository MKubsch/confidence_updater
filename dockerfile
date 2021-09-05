FROM rocker/shiny:4.0.4
RUN install2.r rsconnect
WORKDIR /home/shinyusr
COPY app.r app.r 
COPY deploy.r deploy.r
CMD Rscript deploy.r
