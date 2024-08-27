FROM debian:testing

WORKDIR /opt/build

# occt dependencies
RUN apt-get update -y && apt-get install -y --no-install-recommends \
    libocct-data-exchange-7.8 \
    libocct-data-exchange-dev \
    libocct-draw-7.8 \
    libocct-draw-dev \
    libocct-foundation-7.8 \
    libocct-foundation-dev \
    libocct-modeling-algorithms-7.8 \
    libocct-modeling-algorithms-dev \
    libocct-modeling-data-7.8 \
    libocct-modeling-data-dev \
    libocct-ocaf-7.8 \
    libocct-ocaf-dev \
    libocct-visualization-7.8 \
    libocct-visualization-dev \
    occt-misc

# ghc dependencies
RUN \
    apt-get install -y --no-install-recommends \
        curl \
        libnuma-dev \
        zlib1g-dev \
        libgmp-dev \
        libgmp10 \
        git \
        wget \
        lsb-release \
        software-properties-common \
        gnupg2 \
        apt-transport-https \
        gcc \
        autoconf \
        automake \
        build-essential

# install ghcup
RUN \
    curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > /usr/bin/ghcup && \
    chmod +x /usr/bin/ghcup && \
    ghcup config set gpg-setting GPGStrict

# install ghcup gpg keys
RUN gpg --batch --keyserver keyserver.ubuntu.com --recv-keys 7D1E8AFD1D4A16D71FADA2F2CCC85C0E40C06A8C
RUN gpg --batch --keyserver keyserver.ubuntu.com --recv-keys FE5AB6C91FEA597C3B31180B73EDE9E8CFBAEF01
RUN gpg --batch --keyserver keyserver.ubuntu.com --recv-keys 88B57FCF7DB53B4DB3BFA4B1588764FBE22D19C4
RUN gpg --batch --keyserver keyserver.ubuntu.com --recv-keys EAF2A9A722C0C96F2B431CA511AAD8CEDEE0CAEF

RUN ghcup -v install stack 3.1.1  --isolate /usr/local/bin --gpg lax
        
COPY stack.yaml package.yaml /opt/build/

RUN stack build --dependencies-only --system-ghc

COPY . /opt/build/

RUN stack build --system-ghc

RUN stack run --system-ghc



