#!/bin/env bash

TEMP_DIR=$(mktemp -d)
curl https://bootstrap.pypa.io/get-pip.py -o $TEMP_DIR/get-pip.py
python $TEMP_DIR/get-pip.py --user

rm -rf $TEMP_DIR

