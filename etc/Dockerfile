FROM buildpack-deps:precise-scm
MAINTAINER Mathieu Boespflug <m@tweag.io>
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
         r-base \
         r-base-dev \
    && rm -rf /var/lib/apt/lists/*
