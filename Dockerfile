FROM rocker/r-ver:4.4.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libssl-dev libcurl4-openssl-dev libxml2-dev \
    libgit2-dev libfontconfig1-dev \
    pandoc pandoc-citeproc \
    libglpk-dev libgsl-dev libomp-dev \
    g++ make cmake libnlopt-dev

RUN R -e "install.packages(c('brms','renv' ,'remotes'), repos='https://cloud.r-project.org')"
RUN R -e "install.packages('cmdstanr', repos = c('https://stan-dev.r-universe.dev', getOption('repos')));cmdstanr::install_cmdstan()"


# Set default workdir
WORKDIR /home/r

# Default command
CMD ["/bin/bash"]

