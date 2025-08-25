## -- This will:
## -- - Install system deps for Ubuntu 20.04
## -- - Download the yq binary.
## -- - Install scdrake deps (using DESCRIPTION) into global R library.
## -- - All dependencies are then snapshotted by:
## --   renv::snapshot(lockfile = "renv.lock", type = "all", repos = BiocManager::repositories())
## -- You can use this command to create docker:
## docker buildx build --progress plain --platform linux/amd64'--build-arg R_PKG_INSTALL_NCPUS=8 --build-arg R_PKG_INSTALL_MAKE_NCPUS=4 
## -t your_docker_name/scdrake:tag -f deps.Dockerfile .
## Do not forgett the dot!
## -- Then you can develop scdrake in the container by mounting the source directory.
## -- You can also use RStudio Server:
## -- docker run -d -e USERID=$(id -u) -e GROUPID=$(id -g) -e PASSWORD=1234 -p <host port>:8787 -v $(pwd):/home/$(whoami)/scdrake scdrake:<tag> 

FROM bioconductor/bioconductor_docker:RELEASE_3_21

## -- https://github.com/bioinfocz/scdrake/blob/main/required_libs_linux.md -> Ubuntu 24.04
RUN apt-get update && apt-get install -y --no-install-recommends \
  libudunits2-dev \
  libgdal-dev \
  libgeos-dev \
  libproj-dev \
  libmysqlclient-dev \
  libglpk-dev \
  libgmp3-dev \
  libxml2-dev \
  make \
  libicu-dev \
  pandoc \
  libssl-dev \
  libfontconfig1-dev \
  libfreetype6-dev \
  libpng-dev \
  imagemagick \
  libmagick++-dev \
  gsfonts \
  python3 \
  zlib1g-dev \
  libgeos-dev \
  git \
  libgit2-dev \
  libzmq3-dev \
  libfribidi-dev \
  libharfbuzz-dev \
  libjpeg-dev \
  libtiff-dev \
  qpdf

# Download yq, make executable, and create symlinks
RUN rm -f /usr/local/bin/yq /root/.local/bin/yq /home/rstudio/.local/bin/yq

# Download and install yq
RUN curl -L --output /usr/local/bin/yq https://github.com/mikefarah/yq/releases/download/3.4.1/yq_linux_amd64 \
    && chmod +x /usr/local/bin/yq

# Create symlinks
RUN mkdir -p /root/.local/bin /home/rstudio/.local/bin \
    && ln -s /usr/local/bin/yq /root/.local/bin/yq \
    && ln -s /usr/local/bin/yq /home/rstudio/.local/bin/yq
RUN chown -R rstudio:rstudio /home/rstudio/.local


ENV RENV_VERSION 1.1.5
RUN R -e "remotes::install_version('renv', version = '${RENV_VERSION}', repos = 'https://cloud.r-project.org')"

COPY DESCRIPTION /scdrake_deps/DESCRIPTION

ARG R_PKG_INSTALL_NCPUS=1
RUN Rscript -e "\
  options(Ncpus = ${R_PKG_INSTALL_NCPUS});\
  devtools::install_dev_deps('/scdrake_deps', repos = BiocManager::repositories());\
  "

RUN Rscript -e "remotes::install_version('clustermq', version = '0.9.9', repos = BiocManager::repositories())"
RUN Rscript -e "remotes::install_version('tidyselect', version = '1.2.1', repos = BiocManager::repositories())"
RUN Rscript -e "BiocManager::install('eddelbuettel/littler@0.3.19')"

RUN Rscript -e "renv::snapshot(lockfile = '/scdrake_deps/renv.lock', type = 'all', repos = BiocManager::repositories())"

# RUN mkdir -p /home/rstudio/.config/rstudio
# COPY rstudio-prefs.json /home/rstudio/.config/rstudio/

ENV SCDRAKE_DOCKER TRUE

## -- Strip shared libraries, see https://github.com/rocker-org/rocker-versioned2/issues/340
RUN find /usr/local/lib/R/site-library/*/libs/ -name \*.so | xargs strip -s -p


ENV MAKEFLAGS=""

ARG PROXY_IMAGE
ENV ALL_PROXY=${PROXY_IMAGE}
ENV http_proxy=${PROXY_IMAGE}
ENV https_proxy=${PROXY_IMAGE}



## -- This will start RStudio.
CMD ["/init"]
