#!/bin/bash

set -e

(mkdir -p dist && cd dist && elm make ../src/Main.elm)