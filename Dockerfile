FROM rocker/r-ver:4.5

# Install system dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libharfbuzz-dev \ 
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libgit2-dev \
    libfontconfig1-dev \
    pandoc \
    libglpk-dev \
    libgsl-dev \
    libomp-dev \
    g++ make cmake libnlopt-dev libstdc++-12-dev

RUN R -e "install.packages(c('brms','renv' ,'remotes'), repos='https://cloud.r-project.org')"
RUN R -e "install.packages('cmdstanr', repos = c('https://stan-dev.r-universe.dev', getOption('repos')));cmdstanr::install_cmdstan()"
RUN R -e "cmdstanr::install_cmdstan()"

ENV HOME=/home/r

# Set default workdir
WORKDIR /home/r
COPY renv.lock renv.lock

