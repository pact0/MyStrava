FROM rocker/r-base:latest

RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libgdal-dev \
    build-essential \
    r-cran-raster \
    make \
    cmake \
    && rm -rf /var/lib/apt/lists/*

RUN install.r remotes

RUN sudo su - -c "R -e \"install.packages('https://cran.r-project.org/src/contrib/Archive/raster/raster_2.5-8.tar.gz', repos = NULL, type='source')\""

RUN sudo su - -c "R -e \"remotes::install_github('grimbough/FITfileR')\""
RUN sudo su - -c "R -e \"remotes::install_github('rspatial/terra')\""

RUN install.r shiny TfileR leaflet dplyr stringr trackeR tools openxlsx shinymanager shiny

RUN echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site

RUN addgroup --system app \
    && adduser --system --ingroup app app

WORKDIR /home/app

COPY app .

RUN chown app:app -R /home/app

USER app

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/home/app')"]
