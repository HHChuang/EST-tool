#!/bin/bash
#########################################################################
#   Program:                                                            #
#       Plot the MO energy spectra with their MO figures which prepared #
#       in advanced (via. Jmol).                                        #
#                                                                       #
#   Input:                                                              #
#       $1 = gaissian output file; *.log                                #
#        2 = there ar a series of MO.$i.png in the same dir.            #
#                                                                       #
#   Output:                                                             #
#       1. Espectra.eps; plot all MO energy spectra                     #
#       2. MO.eps; zoom-in selected window with their MO figures        #
#                                                                       #
#   History:                                                            #
#   2020/03/05, Grace, generate MO energy spectra function, assignMO(). #
#   2020/03/06, Grace, label HOMO and insert one png MO figure.         #
#   2020/03/09, Grace, use multiplot to insert a series of MO figures.  #
#########################################################################

# global variables
# Xwindow=(-2.5 2.5)
Xwindow=(-1.0 1.0)
# Ewindow=(-0.75 -0.35)
Ewindow=(0.15 0.5) # select part of MOs, depends on system
totMO=$(grep -A 1 num $1 | tail -n 1 | awk '{print $1}')

function main {
    # grepAllMO $1 # output: MO_E.dat TODO:
    assignMO $1 coord.dat # output: coord.dat, format: $num $x $y
    assignHOMO $1  # output variable: $HOMO
    HOMO=$(echo $?)
    plotMO coord.dat $HOMO # output: Espectra.eps, MO.eps
}

function grepAllMO {
    # $1 = *.log 
    NBasis=$(grep NBasis $1 | tail -n 1 | awk '{print $2}')
    line=$(( $NBasis/5 )) #TODO:
    grep 'Alpha' $1 | grep 'eigenvalues' | tail -n $line > MOEnergy.dat 
}


function assignMO {
    #   $1 = MO energy 
    #   $2 = coord.dat, name of output file

    x=(-1.2 0 1.2)
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
    HOMOcoord=$( echo ${y[$(( $2 -1 )) ]} - 0.03 | bc)
    
    # orbital in selected energy range 
    for ((i=0;i<=$(($totMO-1));i++))
    do 
        if (( $(echo "${y[$i]} > ${Ewindow[0]}" | bc -l) )); then 
            figMIN=$(($i+1)) #array of gnuplot starts from 1 
            break
        fi 
    done
    for ((i=$figMIN;i<=$(($totMO-1));i++))
    do 
        if (( $(echo "${y[$i]} > ${Ewindow[1]}" | bc -l) )); then 
            figMAX=$i #array of gnuplot starts from 1 
            break
        fi 
    done

# plot all MO energy spectra
gnuplot << EOF
    # import array from bash to gnuplot
    xLcoord="${xleft[*]}"
    xRcoord="${xright[*]}"
    ycoord="${y[*]}"
    labelcoord="${labelcoord[*]}"
    num="${num[*]}"

    # Environment setting
    set term postscript eps enhanced color \
        size 14.0cm,21.0cm font 'Times-Roman,20' # A5 size
    unset key
    set xrange [-2.5:2.5]
    set yrange [word(ycoord,1):word(ycoord,$totMO)]

    # Energy spectra; set MO energy bars and their numbering
    set size ratio 1.5
    unset xtics 
    do for [i=1:$totMO] {
        set arrow from word(xLcoord,i),word(ycoord,i) to word(xRcoord,i),word(ycoord,i) nohead lw 5
        set label word(num,i) at word(labelcoord,i),word(ycoord,i)
    }
    set label 'HOMO' at word(labelcoord,$HOMO),$HOMOcoord

    set output 'Espectra.eps'
    p 'coord.dat' u 2:3

EOF
# select important MO and plot their energy with MO figures
gnuplot << EOF
    # import array from bash to gnuplot
    xLcoord="${xleft[*]}"
    xRcoord="${xright[*]}"
    ycoord="${y[*]}"
    labelcoord="${labelcoord[*]}"
    MOxcoord="${MOx[*]}"
    num="${num[*]}"
    Ewindow="${Ewindow[*]}"
    Xwindow="${Xwindow[*]}"

    # Environment setting
    set term postscript eps enhanced color \
        size 14.0cm,21.0cm font 'Times-Roman,20' # A5 size
    set output "MO.eps"
    unset key
    set multiplot
    set xrange [word(Xwindow,1):word(Xwindow,2)]
    set yrange [word(Ewindow,1):word(Ewindow,2)]

    # Energy spectra; set MO energy bars and their numbering
    set size ratio 1.5
    unset xtics 
    do for [i=$figMIN:$figMAX] {
        set arrow from word(xLcoord,i),word(ycoord,i) to word(xRcoord,i),word(ycoord,i) nohead lw 5
        set label word(num,i) at word(labelcoord,i),word(ycoord,i)
    }

    set label 'HOMO' at word(labelcoord,$HOMO),$HOMOcoord
    p 'coord.dat' u 2:3

    # MO figures - 2020/03/12 - the secend trial - set multiplot
    # ref. change coordinate system from screen to graph 
    # 1. http://gnuplot.10905.n7.nabble.com/placing-graph-origin-at-exact-location-respect-to-other-graph-in-multiplot-td20684.html
    # 2. https://stackoverflow.com/questions/44043914/conversion-between-gnuplots-coordinates-systems
    unset xrange
    unset yrange
    unset label
    set size square
    unset tics
    # set size 0.15,0.15
    set size 0.12,0.12

   xlength= word(Xwindow,2) - word(Xwindow,1) 
   ylength= word(Ewindow,2) - word(Ewindow,1) 
    do for [i=$figMIN:$figMAX] {
        # change graph coordinate to screen coordinate 
        FRAC_X = ( word(MOxcoord,i) - word(Xwindow,1) ) / xlength
        FRAC_Y = ( word(ycoord,i) - word(Ewindow,1) ) * 0.9 / ylength 
        set origin FRAC_X,FRAC_Y  # screen coordinate 
        set title sprintf("%i",i) offset 0,-1
        plot 'MO.'.i.'.png' binary filetype=png with rgbimage 
        # print i,' ',word(MOxcoord,i),' ',word(ycoord,i),' ',FRAC_X,' ',FRAC_Y
    }
EOF

rm -f $1
}
 
main $1
