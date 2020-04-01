#!/bin/bash
#############################################################
#  Program:                                                 #
#   Scan PEC for CASSCF/CASPT2 w/ one d.o.f. = bond length  #
#                                                           #
#   History:                                                #
#   2020/04/01, Grace, add readPrev{} which read the        #
#   initial orbital from previous point, also add check     #
#   function to see the desired active space.               #
#                                                           #
#############################################################

nameroot='R'
Req=1.50832
Ri=0.5
Rf=5.0
step=3 #45 dR=0.1
dR=$( echo "($Rf - $Ri)/$step" | bc -l )
nroot=2
nele=2
norb=2
basis='6-31G(d)'

function main {

    for ((i=0;i<=$step;i++))
    do 
	    # Bond=$( echo $Ri + $dR*$i | bc -l ) # increasing bond length
        Bond=$( echo $Rf - $dR*$i | bc -l | xargs printf "%.2f" ) # decreasing bond length 
	    name=$( echo $nameroot\_$Bond )

        # tune the initial guess
        # 1. read the orbitals from the optimized ground state
        # readMin Min.chk $Bond $name 

        # 2. read the orbitals from UHF
        # readS0 $Bond $name 

        # 3. read the orbitals form the previous point 
        readPrev 10.00_triplet_ROHF.chk $i $Bond $name  

        # submit jobs
        # g16sub $name.com $name.log
    done
}

function readPrev {
    # $1 = initial checkpoint file
    # $2 = index 
    # $3 = bond length
    # $4 = file name 

    # 1. checkpoint file 
    if [ $2 == 0 ]; then 
        # cp $1 $4.chk 
        echo $1 $4.chk 
    else 
        preOrb=$(echo $3 + $dR | bc -l | xargs printf "%.2f" )
        preOrbname=$( echo $nameroot\_$preOrb)
        # cp $preOrbname.chk $4.chk
        echo  $preOrbname.chk $4.chk
    fi 

    # 2. generate g16 input file FIXME: bugs while printing coord. 
cat << EOF > $4.com
%chk=$4.chk
%mem=600MW
%nprocshared=4
#p CASSCF($nele,$norb,NRoot=$nroot)/$basis Guess=read  Nosymm  SCF=(NoVarAcc, NoIncFock)  

$nele electrons, $norb orbital

0 1
EOF 

# addGeoCoord $3 $4.com

}

function readMin {
    # $1 = Min.chk
    # $2 = bond length
    # $3 = name of file

    cp $1 $3.chk
    # generate G09 input file
cat << EOF > $3.com
%chk=$3.chk
%mem=600MW
%nprocshared=4
#p CASSCF($nele,$norb,NRoot=$nroot)/$basis Guess=read  Nosymm  SCF=(NoVarAcc, NoIncFock)  

$nele electrons, $norb orbital

0 1
EOF 

addGeoCoord $2 $3.com

}


function readS0 {
    # $1 = bond 
    # $2 = name of file

    # generate G09 input file
cat << EOF > $2.com
%chk=$2.chk
%mem=600MW
%nprocshared=4
# UHF/$basis Nosymm

ground state 

0 1
EOF 

addGeoCoord $1 $2.com

cat << EOF >> $2.com
--Link1--
%chk=$2.chk
%mem=600MW
%nprocshared=4
#p CASSCF($nele,$norb,NRoot=$nroot)/$basis Guess=read geom=check Nosymm SCF=(NoVarAcc,NoIncFock) 

$nele electrons, $norb orbital

0 1

EOF
}

function addGeoCoord {
    # $1 = Bond length
    # $2 = file name
cat << EOF >> $2
 C              
 O                  1            B1
 O                  1            B2    2            A1
 H                  3            B3    1            A2    2            D1    0
 C                  1            B4    2            A3    3            D2    0
 H                  5            B5    1            A4    2            D3    0
 H                  5            B6    1            A5    2            D4    0
 N                  5            B7    1            A6    2            D5    0
 H                  8            B8    5            A7    1            D6    0
 H                  8            B9    5            A8    1            D7    0

   B1             1.18694126
   B2             1.32858791
   B3             0.95244923
   B4             $1
   B5             1.08274225
   B6             1.09290913
   B7             1.44298247
   B8             1.00005640
   B9             1.00058045
   A1           122.88609185
   A2           108.26803601
   A3           125.15773105
   A4           108.61258999
   A5           105.73927629
   A6           110.21098033
   A7           111.02371786
   A8           110.69022985
   D1             0.23997401
   D2           177.02736831
   D3           142.70223846
   D4          -103.08272599
   D5            21.80149795
   D6          -166.73798541
   D7           -46.08017246

EOF

}

main 