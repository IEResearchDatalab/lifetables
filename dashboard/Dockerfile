FROM rocker/shiny:4.5.0

# Install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    cmake \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN install2.r --error --skipinstalled \
    shiny \
    shinydashboard \
    bslib \
    data.table \
    arrow \
    dplyr \
    dlnm \
    splines \
    ggplot2 \
    plotly \
    DT

# Copy app
COPY app.R /srv/shiny-server/app.R
COPY data/ /srv/shiny-server/data/

# Expose port
EXPOSE 3838

# Run Shiny
CMD ["/usr/bin/shiny-server"]
