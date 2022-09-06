
# Required libraries for Linux systems

Below you can find commands to install the required libraries for
different Linux distributions and their releases. If you are not sure
which release are you using, check contents of the `/etc/os-release`
file.

  - [ubuntu 20.04](#ubuntu-20.04)
  - [ubuntu 18.04](#ubuntu-18.04)
  - [ubuntu 16.04](#ubuntu-16.04)
  - [sle 12.3](#sle-12.3)
  - [redhat 8](#redhat-8)
  - [redhat 7](#redhat-7)
  - [opensuse 42.3](#opensuse-42.3)
  - [centos 8](#centos-8)
  - [centos 7](#centos-7)

-----

## ubuntu 20.04

``` bash
sudo apt-get install -y libglpk-dev
sudo apt-get install -y libgmp3-dev
sudo apt-get install -y libxml2-dev
sudo apt-get install -y make
sudo apt-get install -y libicu-dev
sudo apt-get install -y pandoc
sudo apt-get install -y libcurl4-openssl-dev
sudo apt-get install -y libssl-dev
sudo apt-get install -y libfontconfig1-dev
sudo apt-get install -y libfreetype6-dev
sudo apt-get install -y libpng-dev
sudo apt-get install -y imagemagick
sudo apt-get install -y libmagick++-dev
sudo apt-get install -y gsfonts
sudo apt-get install -y python3
sudo apt-get install -y zlib1g-dev
sudo apt-get install -y libgeos-dev
sudo apt-get install -y git
sudo apt-get install -y libgit2-dev
sudo apt-get install -y libzmq3-dev
sudo apt-get install -y libfribidi-dev
sudo apt-get install -y libharfbuzz-dev
sudo apt-get install -y libjpeg-dev
sudo apt-get install -y libtiff-dev
```

## ubuntu 18.04

``` bash
sudo apt-get install -y software-properties-common
sudo add-apt-repository -y ppa:cran/libgit2
sudo apt-get update
sudo apt-get install -y libglpk-dev
sudo apt-get install -y libgmp3-dev
sudo apt-get install -y libxml2-dev
sudo apt-get install -y make
sudo apt-get install -y libicu-dev
sudo apt-get install -y pandoc
sudo apt-get install -y libcurl4-openssl-dev
sudo apt-get install -y libssl-dev
sudo apt-get install -y libfontconfig1-dev
sudo apt-get install -y libfreetype6-dev
sudo apt-get install -y libpng-dev
sudo apt-get install -y imagemagick
sudo apt-get install -y libmagick++-dev
sudo apt-get install -y gsfonts
sudo apt-get install -y python3
sudo apt-get install -y zlib1g-dev
sudo apt-get install -y libgeos-dev
sudo apt-get install -y git
sudo apt-get install -y libgit2-dev
sudo apt-get install -y libzmq3-dev
sudo apt-get install -y libfribidi-dev
sudo apt-get install -y libharfbuzz-dev
sudo apt-get install -y libjpeg-dev
sudo apt-get install -y libtiff-dev
```

## ubuntu 16.04

``` bash
sudo apt-get install -y software-properties-common
sudo add-apt-repository -y ppa:ubuntugis/ppa
sudo apt-get update
sudo add-apt-repository -y ppa:cran/libgit2
sudo apt-get install -y libglpk-dev
sudo apt-get install -y libgmp3-dev
sudo apt-get install -y libxml2-dev
sudo apt-get install -y make
sudo apt-get install -y libicu-dev
sudo apt-get install -y pandoc
sudo apt-get install -y libcurl4-openssl-dev
sudo apt-get install -y libssl-dev
sudo apt-get install -y libfontconfig1-dev
sudo apt-get install -y libfreetype6-dev
sudo apt-get install -y libpng-dev
sudo apt-get install -y imagemagick
sudo apt-get install -y libmagick++-dev
sudo apt-get install -y gsfonts
sudo apt-get install -y python3
sudo apt-get install -y zlib1g-dev
sudo apt-get install -y libgeos-dev
sudo apt-get install -y git
sudo apt-get install -y libgit2-dev
sudo apt-get install -y libzmq3-dev
sudo apt-get install -y libfribidi-dev
sudo apt-get install -y libharfbuzz-dev
sudo apt-get install -y libjpeg-dev
sudo apt-get install -y libtiff-dev
```

## sle 12.3

``` bash
sudo zypper install -y gmp-devel
sudo zypper install -y libxml2-devel
sudo zypper install -y make
sudo zypper install -y libicu-devel
sudo zypper install -y libcurl-devel
sudo zypper install -y libopenssl-devel
sudo zypper install -y fontconfig-devel
sudo zypper install -y freetype2-devel
sudo zypper install -y libpng16-compat-devel
sudo zypper install -y ImageMagick
sudo zypper install -y ImageMagick-devel
sudo zypper install -y libMagick++-devel
sudo zypper install -y python
sudo zypper install -y zlib-devel
sudo zypper install -y git
sudo zypper install -y libgit2-24
sudo zypper install -y zeromq-devel
sudo zypper install -y libjpeg8-devel
sudo zypper install -y libtiff-devel
```

## redhat 8

``` bash
sudo dnf install -y https://dl.fedoraproject.org/pub/epel/epel-release-latest-8.noarch.rpm
sudo subscription-manager repos --enable codeready-builder-for-rhel-8-x86_64-rpms
sudo dnf install -y glpk-devel
sudo dnf install -y gmp-devel
sudo dnf install -y libxml2-devel
sudo dnf install -y make
sudo dnf install -y libicu-devel
sudo dnf install -y libcurl-devel
sudo dnf install -y openssl-devel
sudo dnf install -y fontconfig-devel
sudo dnf install -y freetype-devel
sudo dnf install -y libpng-devel
sudo dnf install -y ImageMagick
sudo dnf install -y ImageMagick-c++
sudo dnf install -y python2
sudo dnf install -y zlib-devel
sudo dnf install -y geos-devel
sudo dnf install -y git
sudo dnf install -y libgit2-devel
sudo dnf install -y zeromq-devel
sudo dnf install -y fribidi-devel
sudo dnf install -y harfbuzz-devel
sudo dnf install -y libjpeg-turbo-devel
sudo dnf install -y libtiff-devel
```

## redhat 7

``` bash
sudo rpm -q epel-release || yum install -y https://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm
sudo yum install -y glpk-devel
sudo yum install -y gmp-devel
sudo yum install -y libxml2-devel
sudo yum install -y make
sudo yum install -y libicu-devel
sudo yum install -y pandoc
sudo yum install -y libcurl-devel
sudo yum install -y openssl-devel
sudo yum install -y fontconfig-devel
sudo yum install -y freetype-devel
sudo yum install -y libpng-devel
sudo yum install -y ImageMagick
sudo yum install -y ImageMagick-c++
sudo yum install -y python
sudo yum install -y zlib-devel
sudo yum install -y geos-devel
sudo yum install -y git
sudo yum install -y libgit2-devel
sudo yum install -y zeromq-devel
sudo yum install -y fribidi-devel
sudo yum install -y harfbuzz-devel
sudo yum install -y libjpeg-turbo-devel
sudo yum install -y libtiff-devel
```

## opensuse 42.3

``` bash
sudo zypper install -y gmp-devel
sudo zypper install -y libxml2-devel
sudo zypper install -y make
sudo zypper install -y libicu-devel
sudo zypper install -y pandoc
sudo zypper install -y libcurl-devel
sudo zypper install -y libopenssl-devel
sudo zypper install -y fontconfig-devel
sudo zypper install -y freetype2-devel
sudo zypper install -y libpng16-compat-devel
sudo zypper install -y ImageMagick
sudo zypper install -y ImageMagick-devel
sudo zypper install -y libMagick++-devel
sudo zypper install -y python
sudo zypper install -y zlib-devel
sudo zypper install -y geos-devel
sudo zypper install -y git
sudo zypper install -y libgit2-devel
sudo zypper install -y zeromq-devel
sudo zypper install -y fribidi-devel
sudo zypper install -y harfbuzz-devel
sudo zypper install -y libjpeg8-devel
sudo zypper install -y libtiff-devel
```

## centos 8

``` bash
sudo dnf install -y epel-release
sudo dnf install -y dnf-plugins-core
sudo dnf config-manager --set-enabled powertools
sudo dnf install -y glpk-devel
sudo dnf install -y gmp-devel
sudo dnf install -y libxml2-devel
sudo dnf install -y make
sudo dnf install -y libicu-devel
sudo dnf install -y pandoc
sudo dnf install -y libcurl-devel
sudo dnf install -y openssl-devel
sudo dnf install -y fontconfig-devel
sudo dnf install -y freetype-devel
sudo dnf install -y libpng-devel
sudo dnf install -y ImageMagick
sudo dnf install -y ImageMagick-c++-devel
sudo dnf install -y python2
sudo dnf install -y zlib-devel
sudo dnf install -y geos-devel
sudo dnf install -y git
sudo dnf install -y libgit2-devel
sudo dnf install -y zeromq-devel
sudo dnf install -y fribidi-devel
sudo dnf install -y harfbuzz-devel
sudo dnf install -y libjpeg-turbo-devel
sudo dnf install -y libtiff-devel
```

## centos 7

``` bash
sudo yum install -y epel-release
sudo yum install -y glpk-devel
sudo yum install -y gmp-devel
sudo yum install -y libxml2-devel
sudo yum install -y make
sudo yum install -y libicu-devel
sudo yum install -y pandoc
sudo yum install -y libcurl-devel
sudo yum install -y openssl-devel
sudo yum install -y fontconfig-devel
sudo yum install -y freetype-devel
sudo yum install -y libpng-devel
sudo yum install -y ImageMagick
sudo yum install -y ImageMagick-c++-devel
sudo yum install -y python
sudo yum install -y zlib-devel
sudo yum install -y geos-devel
sudo yum install -y git
sudo yum install -y libgit2-devel
sudo yum install -y zeromq-devel
sudo yum install -y fribidi-devel
sudo yum install -y harfbuzz-devel
sudo yum install -y libjpeg-turbo-devel
sudo yum install -y libtiff-devel
```
