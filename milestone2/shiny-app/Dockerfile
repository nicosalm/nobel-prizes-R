FROM rocker/shiny:latest

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libsodium-dev \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('shiny', 'ggplot2', 'dplyr', 'plotly', 'DT', 'lubridate', 'maps', 'reshape2', 'zoo', 'conflicted'), repos='https://cran.rstudio.com/')"

WORKDIR /srv/shiny-server

COPY app.R .
COPY nobel.csv .

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
