FROM rust:1.74-bookworm

ARG GROUP_ID
ARG USER_ID

RUN dpkg --add-architecture arm64 \
  && apt update \
  && apt install -y pkg-config crossbuild-essential-arm64 \
  && rustup target add aarch64-unknown-linux-gnu \
  && groupadd -g ${GROUP_ID} builder \
  && useradd -rm -d /home/builder -s /bin/bash -u ${USER_ID} -g ${GROUP_ID} builder \
  && echo '[target.aarch64-unknown-linux-gnu]' > /usr/local/cargo/config.toml \
  && echo 'linker = "/usr/bin/aarch64-linux-gnu-gcc"' >> /usr/local/cargo/config.toml
ENV PKG_CONFIG_SYSROOT_DIR="/usr/lib/aarch64-linux-gnu/pkgconfig"

USER builder
WORKDIR /home/builder/rust-pkg
CMD ["cargo", "build", "-r", "--target", "aarch64-unknown-linux-gnu"]
