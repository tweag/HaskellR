FROM fpco/stack-build:lts-9.0
MAINTAINER Mathieu Boespflug <m@tweag.io>

# Install system dependencies.
RUN apt-add-repository ppa:marutter/rrutter \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
         libzmq3-dev \
         python-dev \
         python-pip \
         r-base \
         r-base-dev \
         r-cran-ggplot2 \
    && rm -rf /var/lib/apt/lists/*

# Pin version for repeatability.
RUN pip install 'ipython[all]==3.2.1'
