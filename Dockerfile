FROM haskell:9.6.4 as builder
RUN stack upgrade
RUN stack update # cache cabal index update in a layer

RUN mkdir -p /src/dbmigrations
WORKDIR /src

ARG BACKEND

# Dependencies
COPY stack.yaml package.yaml ./
RUN stack --system-ghc build --dependencies-only --flag dbmigrations:$BACKEND

# Library
COPY src ./src
COPY LICENSE ./LICENSE
RUN stack --system-ghc build dbmigrations:lib --flag dbmigrations:$BACKEND

# Backend-specific executable
COPY $BACKEND ./$BACKEND
RUN stack --system-ghc install --flag dbmigrations:$BACKEND
RUN cp /root/.local/bin/dbm-$BACKEND /dbm

FROM ubuntu:20.04
RUN \
  apt-get update -qq && \
  apt-get install --assume-yes --no-install-recommends \
    libpq-dev \
    locales && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*
ENV LANG=en_US.UTF-8
COPY --from=builder /dbm /usr/local/bin/dbm
CMD ["dbm", "help"]
