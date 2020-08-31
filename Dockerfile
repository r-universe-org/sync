FROM ubuntu:focal

ENV DEBIAN_FRONTEND noninteractive

COPY . /pkg
COPY entrypoint.sh /entrypoint.sh

RUN \
	apt-get update && \
	apt-get -y dist-upgrade && \
	apt-get install -y r-base-core git gcc libcurl4-openssl-dev libssl-dev libgit2-dev

RUN \
	R -e 'install.packages("remotes"); remotes::install_local("/pkg")'

ENTRYPOINT ["/entrypoint.sh"]

