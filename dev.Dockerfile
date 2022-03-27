# syntax=docker/dockerfile:experimental

# Node & NPM & R, SAVi, rhdf5 packages, renv, savi.h5 data, inputs 
FROM cloudfoundry/cflinuxfs3 AS builder 
ENV DOCKER=1
VOLUME /app/savi/data
EXPOSE 4443
ARG REPO 
WORKDIR /app
RUN --mount=type=ssh mkdir ~/.ssh \
  && curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.3/install.sh | bash \
  && . ~/.bashrc \
  && nvm install node \
  && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9 \
  && apt-get update \
  && apt-get install -y software-properties-common libv8-dev libudunits2-dev libgdal-dev libgeos-dev libproj-dev \
  && add-apt-repository -y 'deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/' \
  && apt-get install -y r-base=3.6.3-1bionic r-base-dev=3.6.3-1bionic \
  && ssh-keyscan $REPO >> ~/.ssh/known_hosts \ 
  && git clone git@esgovcloud.com:mAndA/savi.git \
  && cd savi \
  && npm set unsafe-perm true \
  && npm install

# shiny
WORKDIR /app/savi
COPY r/input.R r/
CMD . ~/.bashrc && npm start
