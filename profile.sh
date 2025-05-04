#!/usr/bin/env bash

RPATH=/usr/local/bin/R
PROFILE=profile.out
PROFILER=/opt/homebrew/lib/libprofiler.dylib

DYLD_INSERT_LIBRARIES="$PROFILER" CPUPROFILE="$PROFILE" "$RPATH" --vanilla -f ./profile.R
pprof --text "$RPATH" "$PROFILE" > ./profile.txt
