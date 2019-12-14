#!/bin/bash

filename=$1

echo "applying on" $filename

sed -i "s/=> //" $filename
sed -i "s/,//g" $filename

echo "1 ORE 1 ORE" >> $filename
