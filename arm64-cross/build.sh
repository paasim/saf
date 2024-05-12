#!/bin/bash

docker build -t cross-compiler \
  --build-arg USER_ID=$(id -u) --build-arg GROUP_ID=$(id -g) \
  arm64-cross
