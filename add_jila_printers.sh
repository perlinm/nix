#!/bin/env bash

ppd_dir=/etc/cups/ppd

lpadmin -p jila_s240 -P $ppd_dir/jila_s240.ppd \
        -v lpd://jilau1.colorado.edu/hpdbl \
        -D "HP 9050 in JILA S240" -L "jila_s240" \
        -o printer-is-shared=false -E

lpadmin -p jila_s240_color -P $ppd_dir/jila_s240_color.ppd \
        -v lpd://jilau1.colorado.edu/jilacolordbl \
        -D "HP 5500 in JILA S240" -L "jila_s240_color" \
        -o printer-is-shared=false -E

lpadmin -p jila_f6 -P $ppd_dir/jila_f6.ppd \
        -v lpd://jilau1.colorado.edu/hp2dbl \
        -D "HP 9050 on 6th floor of the JILA X-Wing" -L "jila_f6" \
        -o printer-is-shared=false -E

lpadmin -p jila_x320 -P $ppd_dir/jila_x320.ppd \
        -v lpd://jilau1.colorado.edu/hp3dbl \
        -D "HP 9050 in JILA X320" -L "jila_x320" \
        -o printer-is-shared=false -E

for ii in jila_s240 jila_s240_color jila_f6 jila_x320; do
  lpoptions -p $ii -o sides=two-sided-long-edge -o Duplex=DuplexNoTumble
done

lpoptions -d jila_x320
