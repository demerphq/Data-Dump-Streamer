#!/usr/bin/bash
for f in $(find . -name *.pm -or -name *.t | grep -v /blib/)
do 
    perltidy -w -ple -bbb -bbc -bbs -nolq -l=80 -noll -nola -nwls='='   \
             -isbc -nolc -otr -kis -ci=4 -se -sot -sct -nsbl -pt=2 -fs  \
             -fsb='#start-no-tidy' -fse='#end-no-tidy' -cpb -bfvt=2     \
             -b $f
done
