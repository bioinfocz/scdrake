## -- This will:
## -- - Install system deps for Ubuntu 20.04
## -- - Download the yq binary.
## -- - Install scdrake deps (using DESCRIPTION) into global R library.
## -- - All dependencies are then snapshotted by:
## --   renv::snapshot(lockfile = "renv.lock", type = "all", repos = BiocManager::repositories())
## -- Then you can develop scdrake in the container by mounting the source directory.
## -- You can also use RStudio Server:
## -- docker run -d -e USERID=$(id -u) -e GROUPID=$(id -g) -e PASSWORD=1234 -p <host port>:8787 -v $(pwd):/home/$(whoami)/scdrake scdrake:<tag> /init

FROM bioconductor/bioconductor_docker:RELEASE_3_15

## -- https://github.com/bioinfocz/scdrake/blob/main/required_libs_linux.md -> Ubuntu 20.04
RUN apt-get update && apt-get install -y --no-install-recommends \
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

RUN wget -O /usr/local/bin/yq https://github.com/mikefarah/yq/releases/download/3.4.1/yq_linux_amd64
RUN chmod +x /usr/local/bin/yq

ENV RENV_VERSION 0.16.0
RUN R -e "BiocManager::install('rstudio/renv@${RENV_VERSION}')"

COPY DESCRIPTION /scdrake_deps/DESCRIPTION

ARG R_PKG_INSTALL_NCPUS=1
RUN Rscript -e "\
  options(Ncpus = ${R_PKG_INSTALL_NCPUS});\
  devtools::install_dev_deps('/scdrake_deps', repos = BiocManager::repositories());\
  "

RUN Rscript -e "remotes::install_version('clustermq', version = '0.8.8', repos = BiocManager::repositories())"
RUN Rscript -e "remotes::install_version('tidyselect', version = '1.1.2', repos = BiocManager::repositories())"
RUN Rscript -e "BiocManager::install('eddelbuettel/littler@0.3.17')"

RUN Rscript -e "renv::snapshot(lockfile = '/scdrake_deps/renv.lock', type = 'all', repos = BiocManager::repositories())"

RUN mkdir -p /root/.local/bin
RUN ln -s /usr/local/bin/yq /root/.local/bin/yq

RUN mkdir -p /home/rstudio/.local/bin
RUN ln -s /usr/local/bin/yq /home/rstudio/.local/bin/yq

# RUN mkdir -p /home/rstudio/.config/rstudio
# COPY rstudio-prefs.json /home/rstudio/.config/rstudio/

ENV SCDRAKE_DOCKER TRUE

## -- This will start RStudio.
CMD ["/init"]
