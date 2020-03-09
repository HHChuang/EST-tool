#!/bin/bash
#########################################################################
#   Program:                                                            #
#       Plot the MO energy spectra with their MO figures which prepared #
#       in advanced (via. Jmol).                                        #
#                                                                       #
#   Input:                                                              #
#       $1 = MO energy                                                  #
#        2 = there ar a series of MO.$i.png in the same dir.            #
#                                                                       #
#   Output:                                                             #
#                                                                       #
#   History:                                                            #
#   2020/03/05, Grace, generate MO energy spectra function, assignMO(). #
#   2020/03/06, Grace, label HOMO and insert one png MO figure.         #
#   2020/03/09, Grace, use multiplot to insert a series of MO figures.  #
#########################################################################

# global variables
# Ewindow=() # plot all MOs TODO: make a switch in plotMO()
Ewindow=(-0.6 0.4) # select part of MOs
totMO=$(grep -A 1 num $1 | tail -n 1 | awk '{print $1}')

function main {
    assignMO $1 coord.dat # output: coord.dat, format: $num $x $y
    assignHOMO $1  # output variable: $HOMO
    HOMO=$(echo $?)
    plotMO coord.dat $HOMO # output: 
}

function assignMO {
    #   $1 = MO energy 
    #   $2 = coord.dat, name of output file

    x=(-1 0 1)
    xnew=()
    y=( $(grep -A $totMO Energy $1 | tail -n $totMO | awk '{print $3}'| sort -n) )
    highestMO=$(($totMO-1))

    # assign x coordinate for all MOs, except the last one.
    for ((i=0; i < $highestMO ;i++)) 
    do
        j=$(( $i+1 ))
        comp=$(echo ${y[$i]} == ${y[$j]} | bc -l)
        if [ $comp == 1 ] # 1 = true, 0 = false
        then 
            xnew[$i]=${x[0]}
            xnew[$i+1]=${x[2]}
            i=$(($i+1))
        else
            xnew[$i]=${x[1]}
        fi
    done

    # the highest MO
    if [ -z ${xnew[$highestMO]} ]
    then 
        xnew[$highestMO]=${x[1]}
    fi

    # print out result, format: $num $x $y
    num=( $(tail -n $totMO $1 | awk '{print $1}' | sort -n) )
    rm -f $2
    for ((i=0; i < $totMO ;i++))
    do 
        echo ${num[$i]} ${xnew[$i]} ${y[$i]} >> $2
    done
}

function assignHOMO {
    # $1 = MOenergy 
    # $2 = $HOMO 
    double=$(grep -A $totMO 'occ' $1 | awk '{print $2}' | tail -n $totMO | grep -c 2) 
    single=$(grep -A $totMO 'occ' $1 | awk '{print $2}' | tail -n $totMO | grep -c 1) 
    unocc=$(grep -A $totMO 'occ' $1 | awk '{print $2}' | tail -n $totMO | grep -c 0)
    HOMO=$(( $double + $single ))
    return $HOMO 
}

function plotMO {
    # $1 = coord.dat 
    # $2 = $HOMO 
    
    # calculate coordinate
    num=( $(awk '{print $1}' $1 ) )
    x=( $(awk '{print $2}' $1 ) )
    y=( $(awk '{print $3}' $1 ) )
    halfL=(-0.1 0.1)
    labelshift=-0.3
    labelcoord=()
    MOy=()
    for ((i=0;i< $totMO; i++))
    do 
        xleft[$i]=$(  echo ${x[$i]} + ${halfL[0]} | bc )
        xright[$i]=$( echo ${x[$i]} + ${halfL[1]} | bc )
        labelcoord[$i]=$( echo ${x[$i]} + $labelshift | bc )
        MOx[$i]=$( echo ${x[$i]} + 0.3 | bc )
    done
    HOMOcoord=$( echo ${y[$(( $HOMO -1 )) ]} - 0.03 | bc)

gnuplot << EOF
# import array from bash to gnuplot
xLcoord="${xleft[*]}"
xRcoord="${xright[*]}"
ycoord="${y[*]}"
labelcoord="${labelcoord[*]}"
MOxcoord="${MOx[*]}"
num="${num[*]}"
Ewindow="${Ewindow[*]}"

# Environment setting
set term postscript eps enhanced color \
    size 14.0cm,21.0cm font 'Times-Roman,20' # A5 size
set output "test.eps"
unset key
set multiplot
set xrange [-2.5:2.5]
# set yrange [word(Ewindow,1):word(Ewindow,2)]

# Energy spectra; set MO energy bars and their numbering
set size ratio 1.5
# unset xtics 
do for [i=1:$totMO] {
    set arrow from word(xLcoord,i),word(ycoord,i) to word(xRcoord,i),word(ycoord,i) nohead 
    set label word(num,i) at word(labelcoord,i),word(ycoord,i)
}

set label 'HOMO' at word(labelcoord,$HOMO),$HOMOcoord
p 'CO2_MO_E.dat' u 2:3

# MO figures
# first trial - unset multiplot - cannot control the ration of MO figures; bug in dx,dx setting. 
# ref.: https://www.techrepublic.com/blog/linux-and-open-source/how-to-use-clip-art-in-your-gnuplot-charts/
# p 'CO2_MO_E.dat' u 2:3, 'MO.1.png' binary filetype=png with rgbimage
# p 'MO.1.png' binary filetype=png origin=(word(MOxcoord,$HOMO),word(ycoord,$HOMO) )  with rgbimage 
# plot for [i=1:5] 'MO.'.i.'.png' binary filetype=png origin=( word(MOxcoord,i),word(ycoord,i) ) dx=0.01 dy=0.001 with rgbimage

# the secend trial - set multiplot
# ref. change coordinate system from screen to graph 
# 1. http://gnuplot.10905.n7.nabble.com/placing-graph-origin-at-exact-location-respect-to-other-graph-in-multiplot-td20684.html
# 2. https://stackoverflow.com/questions/44043914/conversion-between-gnuplots-coordinates-systems
set size square
unset tics
set size 0.2,0.2

# XScaling = (GPVAL_TERM_XMAX - GPVAL_TERM_XMIN)/(GPVAL_X_MAX - GPVAL_X_MIN) 
# YScaling = (GPVAL_TERM_YMAX - GPVAL_TERM_YMIN)/(GPVAL_Y_MAX - GPVAL_Y_MIN) 
# XMargin = GPVAL_TERM_XMIN/XScaling 
# YMargin = GPVAL_TERM_YMIN/YScaling 
# print XScaling,' ',YScaling,' ',XMargin,' ',YMargin

do for [i=1:10] {
    # change graph coordinate to screen coordinate 
    XOffset = word(MOxcoord,i) * GPVAL_TERM_XMIN * GPVAL_TERM_SCALE / GPVAL_TERM_XSIZE
    YOffset = word(ycoord,i) * GPVAL_TERM_YMIN * GPVAL_TERM_SCALE / GPVAL_TERM_YSIZE 
    set origin XOffset,YOffset  # screen coordinate 
    set title sprintf("%i",i)
    plot 'MO.'.i.'.png' binary filetype=png with rgbimage 
    print word(MOxcoord,i),' ',word(ycoord,i),' ',XOffset,' ',YOffset
}
# set origin word(MOxcoord,i),word(ycoord,i)
# plot for [i=1:2] 'MO.'.i.'.png' binary filetype=png with rgbimage 

EOF

# rm -f $1
}
 
main $1