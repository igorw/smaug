#/bin/bash
set -e
for file in examples/*.php; 
do 
    echo Running $file
    diff <(hhvm $file) <(cat $file.expect)
done
