FROM openanalytics/r-base

MAINTAINER Tobias Verbeke "tobias.verbeke@openanalytics.eu"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0

# system library dependency for the euler app
RUN apt-get update && apt-get install -y \
    libmpfr-dev

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown', 'devtools', repos='https://cloud.r-project.org/')"

# install dependencies of the euler app
RUN R -e "devtools::install_github('DimitriF/DLC')"

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e DLC::run.tlc_denoising()"]

