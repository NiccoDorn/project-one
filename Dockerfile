FROM ubuntu:noble-20250415.1

ENV TZ="Europe/Berlin"

RUN \
    apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y \
        curl \
        vim \
        nano \
        bash-completion \
        git \
        locales \
        build-essential \
        && \
    rm -rf /var/lib/apt/lists/*

RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && \
    locale-gen
ENV LANG en_US.UTF-8  
ENV LANGUAGE en_US:en  
ENV LC_ALL en_US.UTF-8

RUN \
    apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y \
        openjdk-21-jre-headless \
        python3 \
        python3-pip \
        python3-pandas \
        verilator \
        && \
    rm -rf /var/lib/apt/lists/*

USER ubuntu

WORKDIR /home/ubuntu

# different cases depending on aarch64 or amd64
RUN if [ "$(uname -m)" = "aarch64" ]; then \
    curl -fL "https://github.com/VirtusLab/coursier-m1/releases/latest/download/cs-aarch64-pc-linux.gz" | gzip -d > cs; \
else \
    curl -fL "https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-linux.gz" | gzip -d > cs; \
fi

RUN chmod +x cs && ./cs setup --yes
RUN ./cs install scala:2.13.16
RUN ./cs install sbt
RUN ./cs install scalafmt
RUN rm cs