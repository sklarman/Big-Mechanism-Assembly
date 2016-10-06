# Version: 0.1

FROM tenforce/virtuoso:1.0.0-virtuoso7.2.4

MAINTAINER Jacek Grzebyta <jgrzebyta@users.noreply.github.com>

ENV HOME /home/panda
ENV PROBLOG_HOME $HOME/problog
ENV DEBIAN_FRONTEND noninteractive

# Update ubuntu
RUN apt-get update \
    && apt-get -y dist-upgrade

# Install core packages
RUN apt-get install -y curl emacs24-nox tar bzip2

# Install SWI prolog
RUN apt-get install -q -y swi-prolog swi-prolog-doc swi-prolog-nox swi-prolog-x python3

# Install problog
RUN mkdir -p $PROBLOG_HOME \
    && curl -fsSL https://bitbucket.org/problog/problog/get/master.tar.bz2 | tar -jxv -C $PROBLOG_HOME --strip-components 1 --null

# Update PATH variable
ENV PATH $PROBLOG_HOME:$PATH

WORKDIR ${HOME}
