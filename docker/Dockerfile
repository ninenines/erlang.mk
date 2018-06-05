FROM ubuntu

RUN echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections
RUN apt-get update && apt-get install -y \
  git \
  mercurial \
  subversion \
  erlang \
  build-essential \
  autoconf \
  automake \
  curl \
  python-sphinx \
  asciidoc \
  p7zip-full \
  unzip

COPY . /erlang.mk
WORKDIR /erlang.mk

# Run specific test-case with verbose output
# RUN make check c=escript-deps V=4

RUN make check -j 8 -k
RUN make check LEGACY=1 -j 8 -k
