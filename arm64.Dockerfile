## -- This will:
## -- - Install Bioconductor using the rocker/rstudio image for arm64 platform.
## --   - This is copypasted and edited from https://github.com/Bioconductor/bioconductor_docker/commit/aa83033e4f8f7909847cec24eadba8cd3335a615
## -- - Install system deps for Ubuntu 20.04
## -- - Download the yq binary.
## -- - Install scdrake deps from renv lockfile into global R library.
## -- - Install the scdrake package.
## -- - Install the CLI scripts.

FROM --platform=linux/arm64 rocker/rstudio:latest-daily

## Set Dockerfile version number
ARG BIOCONDUCTOR_VERSION=3.15
ENV BIOCONDUCTOR_DOCKER_VERSION=$BIOCONDUCTOR_DOCKER_VERSION
ENV BIOCONDUCTOR_VERSION=$BIOCONDUCTOR_VERSION

##### IMPORTANT ########
## The PATCH version number should be incremented each time
## there is a change in the Dockerfile.
ARG BIOCONDUCTOR_PATCH=0
ARG BIOCONDUCTOR_DOCKER_VERSION=${BIOCONDUCTOR_VERSION}.${BIOCONDUCTOR_PATCH}

ARG SCDRAKE_VERSION
RUN test -n "$SCDRAKE_VERSION" || (echo "SCDRAKE_VERSION not set" && false)
ENV SCDRAKE_VERSION=$SCDRAKE_VERSION

LABEL name="jirinovo/scdrake" \
    version=$SCDRAKE_VERSION \
    bioconductor_version=$BIOCONDUCTOR_VERSION \
    url="https://github.com/bioinfocz/scdrake" \
    maintainer="jiri.novotny@img.cas.cz" \
    description="Scdrake package wrapped in arm64 Bioconductor docker image." \
    license="MIT"

ARG PROXY_BUILD
ENV ALL_PROXY=${PROXY_BUILD}
ENV http_proxy=${PROXY_BUILD}
ENV https_proxy=${PROXY_BUILD}

## Do not use binary repositories during container creation
## Avoid using binaries produced for older version of same container
ENV BIOCONDUCTOR_USE_CONTAINER_REPOSITORY=FALSE

## START https://github.com/Bioconductor/bioconductor_docker/blob/master/bioc_scripts/install_bioc_sysdeps.sh
##  Add Bioconductor system dependencies
# This is to avoid the error
# 'debconf: unable to initialize frontend: Dialog'
ENV DEBIAN_FRONTEND=noninteractive

## Update apt-get
RUN apt-get update

RUN apt-get install -y --no-install-recommends apt-utils

## Basic Deps
RUN apt-get install -y --no-install-recommends \
    gdb \
    libxml2-dev \
    python3-pip \
    libz-dev \
    liblzma-dev \
    libbz2-dev \
    libpng-dev \
    libgit2-dev

## sys deps from bioc_full
RUN apt-get install -y --no-install-recommends \
    pkg-config \
    fortran77-compiler \
    byacc \
    automake \
    curl \
    cmake

## This section installs libraries
RUN apt-get install -y --no-install-recommends \
    libpcre2-dev \
    libnetcdf-dev \
    libhdf5-serial-dev \
    libfftw3-dev \
    libopenbabel-dev \
    libopenmpi-dev \
    libxt-dev \
    libudunits2-dev \
    libgeos-dev \
    libproj-dev \
    libcairo2-dev \
    libtiff5-dev \
    libreadline-dev \
    libgsl0-dev \
    libgslcblas0 \
    libgtk2.0-dev \
    libgl1-mesa-dev \
    libglu1-mesa-dev \
    libgmp3-dev \
    libhdf5-dev \
    libncurses-dev \
    libxpm-dev \
    liblapack-dev \
    libv8-dev \
    libgtkmm-2.4-dev \
    libmpfr-dev \
    libmodule-build-perl \
    libapparmor-dev \
    libprotoc-dev \
    librdf0-dev \
    libmagick++-dev \
    libsasl2-dev \
    libpoppler-cpp-dev \
    libprotobuf-dev \
    libpq-dev \
    libarchive-dev

## software - perl extentions and modules
RUN apt-get install -y --no-install-recommends \
    libperl-dev \
    libarchive-extract-perl \
    libfile-copy-recursive-perl \
    libcgi-pm-perl \
    libdbi-perl \
    libdbd-mysql-perl \
    libxml-simple-perl

## new libs
RUN apt-get install -y --no-install-recommends \
    libglpk-dev \
    libeigen3-dev

## Databases and other software
RUN apt-get install -y --no-install-recommends \
    sqlite \
    openmpi-bin \
    mpi-default-bin \
    openmpi-common \
    openmpi-doc \
    tcl8.6-dev \
    tk-dev \
    default-jdk \
    imagemagick \
    tabix \
    ggobi \
    graphviz \
    protobuf-compiler \
    jags \
    libhiredis-dev

## Additional resources
RUN apt-get install -y --no-install-recommends \
    xfonts-100dpi \
    xfonts-75dpi \
    biber \
    libsbml5-dev \
    libzmq3-dev \
    python3-dev \
    python3-venv

## More additional resources
## libavfilter-dev - <infinityFlow, host of other packages>
## mono-runtime - <rawrr, MsBackendRawFileReader>
## libfuse-dev - <Travel>
## ocl-icd-opencl-dev - <gpuMagic> - but machine needs to be a GPU--otherwise it's useless
RUN apt-get install -y --no-install-recommends \
    libmariadb-dev-compat \
    libjpeg-dev \
    libjpeg-turbo8-dev \
    libjpeg8-dev \
    libavfilter-dev \
    libfuse-dev \
    mono-runtime \
    ocl-icd-opencl-dev

## Python installations
RUN curl --output /tmp/PySocks-1.7.1-py3-none-any.whl https://files.pythonhosted.org/packages/8d/59/b4572118e098ac8e46e399a1dd0f2d85403ce8bbaad9ec79373ed6badaf9/PySocks-1.7.1-py3-none-any.whl
RUN pip3 install /tmp/PySocks-1.7.1-py3-none-any.whl
RUN pip3 install scikit-learn pandas pyyaml

## libgdal is needed for sf
RUN apt-get install -y --no-install-recommends \
    libgdal-dev \
    default-libmysqlclient-dev \
    libmysqlclient-dev
## END https://github.com/Bioconductor/bioconductor_docker/blob/master/bioc_scripts/install_bioc_sysdeps.sh

## Add host-site-library
RUN echo "R_LIBS=/usr/local/lib/R/host-site-library:\${R_LIBS}" > /usr/local/lib/R/etc/Renviron.site

## -- https://stackoverflow.com/questions/36725027/r-package-installation-from-source-using-multiple-cores
ARG R_PKG_INSTALL_MAKE_NCPUS=1
ENV MAKEFLAGS="-j${R_PKG_INSTALL_MAKE_NCPUS}"
RUN apt-get update && apt-get install -y --no-install-recommends ccache
ENV CC="ccache gcc"
ENV CXX="ccache g++"

## START https://github.com/Bioconductor/bioconductor_docker/blob/master/bioc_scripts/install.R
## -- For now we need a newer GitHub version because of https://github.com/Bioconductor/BiocManager/issues/142
ARG BIOCMANAGER_COMMIT=67faa90

RUN Rscript -e "install.packages('remotes', repos = 'https://cran.rstudio.com')"
RUN Rscript -e "remotes::install_github('Bioconductor/BiocManager@${BIOCMANAGER_COMMIT}', repos = 'https://cran.rstudio.com')"
RUN Rscript -e "BiocManager::install(version = '${BIOCONDUCTOR_VERSION}', update = TRUE, ask = FALSE)"
RUN Rscript -e "BiocManager::install('devtools')"
## END https://github.com/Bioconductor/bioconductor_docker/blob/master/bioc_scripts/install.R

# DEVEL: Add sys env variables to DEVEL image
# Variables in Renviron.site are made available inside of R.
# Add libsbml CFLAGS
RUN curl -O http://bioconductor.org/checkResults/devel/bioc-LATEST/Renviron.bioc \
    && sed -i '/^IS_BIOC_BUILD_MACHINE/d' Renviron.bioc \
    && cat Renviron.bioc | grep -o '^[^#]*' | sed 's/export //g' >>/etc/environment \
    && cat Renviron.bioc >> /usr/local/lib/R/etc/Renviron.site \
    && echo BIOCONDUCTOR_VERSION=${BIOCONDUCTOR_VERSION} >> /usr/local/lib/R/etc/Renviron.site \
    && echo BIOCONDUCTOR_DOCKER_VERSION=${BIOCONDUCTOR_DOCKER_VERSION} >> /usr/local/lib/R/etc/Renviron.site \
    && echo 'LIBSBML_CFLAGS="-I/usr/include"' >> /usr/local/lib/R/etc/Renviron.site \
    && echo 'LIBSBML_LIBS="-lsbml"' >> /usr/local/lib/R/etc/Renviron.site \
    && rm -rf Renviron.bioc

ENV LIBSBML_CFLAGS="-I/usr/include"
ENV LIBSBML_LIBS="-lsbml"

## -- scdrake stuff
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

## clean up
RUN apt-get clean
RUN apt-get autoremove -y
RUN apt-get autoclean -y
RUN rm -rf /var/lib/apt/lists/*

RUN curl -L --output /usr/local/bin/yq https://github.com/mikefarah/yq/releases/download/3.4.1/yq_linux_amd64
RUN test -s "/usr/local/bin/yq" || (echo "yq binary is empty" && false)
RUN chmod +x /usr/local/bin/yq
RUN mkdir -p /root/.local/bin
RUN ln -s /usr/local/bin/yq /root/.local/bin/yq
RUN mkdir -p /home/rstudio/.local/bin
RUN ln -s /usr/local/bin/yq /home/rstudio/.local/bin/yq
RUN chown -R rstudio:rstudio /home/rstudio/.local

ENV RENV_VERSION=0.16.0
RUN R -e "BiocManager::install('rstudio/renv@${RENV_VERSION}')"

ARG R_PKG_INSTALL_NCPUS=1
COPY renv.lock /

RUN Rscript -e "\
    options(Ncpus = ${R_PKG_INSTALL_NCPUS}, renv.config.rspm.enabled = FALSE);\
    renv::consent(TRUE);\
    renv::restore(lockfile = 'renv.lock', exclude = 'BiocManager', prompt = FALSE, repos = BiocManager::repositories());\
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
    devtools::install(\
        pkg = '/scdrake_source', dependencies = FALSE, upgrade = FALSE,\
        keep_source = TRUE, build_vignettes = TRUE,\
        repos = BiocManager::repositories()\
    );\
    "

## -- Strip shared libraries, see https://github.com/rocker-org/rocker-versioned2/issues/340
RUN find /usr/local/lib/R/site-library/*/libs/ -name \*.so | xargs strip -s -p

## -- Clear ccache
RUN ccache --clear

RUN Rscript -e "scdrake::install_cli(type = 'system', ask = FALSE)"

ENV MAKEFLAGS=""
ENV SCDRAKE_DOCKER=TRUE

ARG PROXY_IMAGE
ENV ALL_PROXY=${PROXY_IMAGE}
ENV http_proxy=${PROXY_IMAGE}
ENV https_proxy=${PROXY_IMAGE}

## -- This will start RStudio.
CMD ["/init"]
