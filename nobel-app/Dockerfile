FROM rocker/shiny:latest

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libsodium-dev \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    libudunits2-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('shiny', 'ggplot2', 'dplyr', 'plotly', 'DT', \
    'lubridate', 'maps', 'reshape2', 'zoo', 'conflicted', \
    'rnaturalearth', 'rnaturalearthdata', 'sf', 'scales'), \
    repos='https://cran.rstudio.com/')"

RUN echo 'run_as shiny; \
server { \
  listen 3839; \
  location / { \
    app_dir /srv/shiny-server/nobel-app; \
    log_dir /var/log/shiny-server; \
    directory_index on; \
  } \
}' > /etc/shiny-server/shiny-server.conf

RUN mkdir -p /srv/shiny-server/nobel-app && \
    chown -R shiny:shiny /srv/shiny-server/

COPY app.R /srv/shiny-server/nobel-app/
COPY nobel.csv /srv/shiny-server/nobel-app/

RUN chown -R shiny:shiny /srv/shiny-server/nobel-app/

EXPOSE 3839

CMD ["/usr/bin/shiny-server"]
