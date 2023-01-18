#! /usr/bin/env bash

set -x

rm -r releases/*
./release-for-linux.sh
./release-for-windows.sh




