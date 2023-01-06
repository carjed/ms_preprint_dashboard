FROM rocker/shiny-verse:4.2.2
COPY Rprofile.site /etc/R
RUN R -e 'install.packages(c("shinyWidgets", "shinythemes", "kableExtra", "shinyjs", "shinydashboard", "shinyBS", "lubridate", "stringr", "plotly", "DT", "htmlwidgets", "markdown"))'

# RUN rm -rf /srv/shiny-server/*

COPY shiny-server.conf /etc/shiny-server/
COPY ./app/* /srv/shiny-server/ms_preprint_dashboard/

# run app
CMD ["/usr/bin/shiny-server"]
