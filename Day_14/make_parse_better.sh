#!/bin/bash

filename=$1

echo "applying on" $filename

sed -i "s/=> //" $filename
sed -i "s/,//g" $filename

