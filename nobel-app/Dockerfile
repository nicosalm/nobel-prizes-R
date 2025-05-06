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

# Install R packages
RUN R -e "install.packages(c('shiny', 'ggplot2', 'dplyr', 'plotly', 'DT', \
    'lubridate', 'maps', 'reshape2', 'zoo', 'conflicted', \
    'rnaturalearth', 'rnaturalearthdata', 'sf', 'scales'), \
    repos='https://cran.rstudio.com/')"

RUN R -e "install.packages('remotes', repos='https://cran.rstudio.com/')" && \
    R -e "remotes::install_github('ropensci/rnaturalearthdata')"

WORKDIR /srv/shiny-server

COPY app.R .
COPY nobel.csv .

# 3839 (not shiny default)
EXPOSE 3839

CMD ["sh", "-c", "echo 'server { listen 3839; location / { app_dir /srv/shiny-server; log_dir /var/log/shiny-server; directory_index on; } }' > /etc/shiny-server/shiny-server.conf && /usr/bin/shiny-server"]
