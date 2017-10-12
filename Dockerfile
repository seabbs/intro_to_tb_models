## Start with the tidyverse docker image
FROM rocker/shiny:latest

MAINTAINER "Sam Abbott" contact@samabbott.co.uk

RUN apt-get update && \
    apt-get install -y \
    libssl-dev \
    libssh2-1-dev \
    libnlopt0 \
    libnlopt-dev \
    libudunits2-dev \
    libxml2-dev \
    libgdal-dev \
    libproj-dev \
    && apt-get clean

## install igraph due to CRAN bug from github
RUN install2.r --error \
      --deps TRUE \
      pkgconfig \
      irlba \
      remotes

RUN installGithub.r igraph/rigraph \
&& rm -rf /tmp/downloaded_packages/

## Install cran packages
RUN install2.r --error \
    --deps TRUE \
     shinydashboard \
     tidyverse \
     DT \
     rmarkdown \
     plotly \

RUN rm -r /srv/shiny-server/*
ADD . /srv/shiny-server/intro_to_to_models
