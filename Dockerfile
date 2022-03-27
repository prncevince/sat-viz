FROM cloudfoundry/cflinuxfs3 as base

# Docker listening port. Can be overridden by cloudfoundry
# see https://docs.cloudfoundry.org/devguide/deploy-apps/push-docker.html#port_config
ENV PORT=3838
EXPOSE $PORT

# installs
ENV DOCKER=1
WORKDIR /app/savi
COPY Makefile package.json package-lock.json renv.lock .

# Initial Setup 
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9 \
  && apt-get update

# AWS CLI & PIP
RUN apt-get install -y python3-pip \
  && python3 -m pip install awscli \
  && echo "complete -C aws_completer aws" >> ~/.bashrc

# Node & NPM
RUN curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.3/install.sh | bash \
  && . ~/.bashrc \
  && nvm install node
 
# R
RUN apt-get install -y software-properties-common libv8-dev libudunits2-dev libgdal-dev libgeos-dev libproj-dev \
  && add-apt-repository -y 'deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/' \
  && apt-get install -y r-base=3.6.3-1bionic r-base-dev=3.6.3-1bionic

# rhdf5 packages & renv
RUN . ~/.bashrc && npm set unsafe-perm true && npm install

