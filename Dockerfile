## -- This will:
## -- - Install system deps for Ubuntu 20.04
## -- - Download the yq binary.
## -- - Install scdrake deps from renv lockfile into global R library.
## -- - Install the scdrake package.
## -- - Install the CLI scripts.
## -- You can build the Dockerfile using
## docker-buildx build --progress plain --platform linux/amd64 --build-arg R_PKG_INSTALL_NCPUS=8 --build-arg R_PKG_INSTALL_MAKE_NCPUS=4 -t jirinovo/scdrake:<version>-bioc3.15 -f Dockerfile

ARG BIOCONDUCTOR_VERSION=3_15
FROM bioconductor/bioconductor_docker:RELEASE_$BIOCONDUCTOR_VERSION

ARG SCDRAKE_VERSION
RUN test -n "$SCDRAKE_VERSION" || (echo "SCDRAKE_VERSION not set" && false)

LABEL name="bioinfocz/scdrake" \
      version=$SCDRAKE_VERSION \
      bioconductor_version=$BIOCONDUCTOR_VERSION \
      url="https://github.com/bioinfocz/scdrake" \
      maintainer="jiri.novotny@img.cas.cz" \
      description="Scdrake package wrapped in the Bioconductor docker image." \
      license="MIT"

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
RUN test -s "/usr/local/bin/yq" || (echo "yq binary is empty" && false)
RUN chmod +x /usr/local/bin/yq
RUN mkdir -p /root/.local/bin
RUN ln -s /usr/local/bin/yq /root/.local/bin/yq
RUN mkdir -p /home/rstudio/.local/bin
RUN ln -s /usr/local/bin/yq /home/rstudio/.local/bin/yq
RUN chown -R rstudio:rstudio /home/rstudio/.local

ENV RENV_VERSION 0.16.0
RUN R -e "BiocManager::install('rstudio/renv@${RENV_VERSION}')"
ARG R_PKG_INSTALL_NCPUS=1
ARG R_PKG_INSTALL_MAKE_NCPUS=1
ENV MAKEFLAGS="-j${R_PKG_INSTALL_MAKE_NCPUS}"
## -- For Rhtslib this error appears during renv::restore(): '/usr/bin/tar: Unexpected EOF in archive'\
RUN Rscript -e "BiocManager::install(c('Rhtslib', 'Rsamtools'), update = FALSE, ask = FALSE, Ncpus = ${R_PKG_INSTALL_NCPUS})"

COPY renv.lock /
RUN Rscript -e "\
  options(Ncpus = ${R_PKG_INSTALL_NCPUS});\
  renv::consent(TRUE);\
  renv::restore(lockfile = 'renv.lock', prompt = FALSE);\
  "

RUN mkdir /scdrake_source
COPY DESCRIPTION /scdrake_source/DESCRIPTION
COPY inst /scdrake_source/inst
COPY man /scdrake_source/man
COPY NAMESPACE /scdrake_source/NAMESPACE
COPY R /scdrake_source/R
COPY tests /scdrake_source/tests
COPY vignettes /scdrake_source/vignettes
RUN Rscript -e "\
  options(Ncpus = ${R_PKG_INSTALL_NCPUS});\
  devtools::install(\
    pkg = '/scdrake_source', dependencies = FALSE, upgrade = FALSE,\
    keep_source = TRUE, build_vignettes = TRUE,\
    repos = BiocManager::repositories()\
  );\
  "

## -- Strip shared libraries, see https://github.com/rocker-org/rocker-versioned2/issues/340
RUN find /usr/local/lib/R/site-library/*/libs/ -name \*.so | xargs strip -s -p

RUN Rscript -e "scdrake::install_cli(type = 'system', ask = FALSE)"

ENV MAKEFLAGS=""
ENV SCDRAKE_DOCKER TRUE

## -- This will start RStudio.
CMD ["/init"]
