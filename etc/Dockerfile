FROM buildpack-deps:trusty-scm
MAINTAINER Mathieu Boespflug <m@tweag.io>

# Add FPCO repo for stack.
RUN wget -q -O- https://s3.amazonaws.com/download.fpcomplete.com/ubuntu/fpco.key | apt-key add - \
    && echo 'deb http://download.fpcomplete.com/ubuntu/trusty stable main' | tee /etc/apt/sources.list.d/fpco.list

# Install system dependencies.
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
         r-base \
         r-base-dev \
         libzmq3-dev \
         ipython \
         stack \
    && rm -rf /var/lib/apt/lists/*
