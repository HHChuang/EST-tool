#!/bin/bash
# $1 = MO coefficients

nMO=10
nAO=85
output='MOdensity_'$1
echo "output file: $output"

# square the MO coefficients
C2=[]
rm -f $output 
for ((i=1;i<=$nAO;i++))
do 
    for ((j=1;j<=$nMO;j++))
    do 
        c=$(sed -n "$i,$i p" $1| awk -v col=$j 'BEGIN{OFS=FS="\t"} {print $col}')
        c_square=$(echo "$c*$c" | bc -l)
        index=$(($j-1))
        C2[$index]=$c_square
    done
    echo ${C2[@]} >> $output 
done