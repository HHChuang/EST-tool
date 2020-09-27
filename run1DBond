#!/bin/bash
#################################################################
#  Program:                                                     #
#   Scan a PEC where the varied d.o.f. is a bond length         #
#                                                               #
#   History:                                                    #
#   2020/04/01, Grace, add readPrev{} which read the            #
#       initial orbital from previous point, also add check     #
#       function to see the desired active space.               #
#   2020/09/27, Grace, separate ground state input and CASSCF   #
#                                                               #
#################################################################

nameroot='H2'

# parameters for PEC ###
Ri=0.5
Rf=5.0
step=50
dR=$( echo "($Rf - $Ri)/$step" | bc -l )

# parameters for EST ###
EST_S0='UHF'
basis='6-31G(d)'
charge=0
multi=1

# CASSCF ###
# nroot=2
# nele=2
# norb=2

function main() {

    for ((i=0;i<=$step;i++))
    do 
        # increasing bond length
	    Bond=$( echo $Ri + $dR*$i | bc -l | xargs printf "%.2f" ) 
        # decreasing bond length 
        # Bond=$( echo $Rf - $dR*$i | bc -l | xargs printf "%.2f" ) 
	    name=$( echo $nameroot\_$Bond )

        # tune the initial guess 
         # 1. default, no initial guess
        genG16input $Bond $name # output: $name.com  
        
        # 2. read the orbitals from the optimized ground state
        # readMin Min.chk $Bond $name # output: $name.com

        # 3. read the orbitals from UHF
        # readS0 $Bond $name # output: $name.com

        # 4. read the orbitals form the previous point 
        # readPrev 10.00_triplet_ROHF.chk $i $Bond $name # output: $name.com

        # submit jobs; depend on schedule system (i.e., PBS, SGE or slurm), this part maybe different.
        g16sub $name.com $name.log
    done
}

function genG16input(){
    # $1 = bond 
    # $2 = name of file

    # generate G09 input file
cat << EOF > $2.com
%mem=600MW
%nprocshared=4
# $EST_S0/$basis Nosymm Guess=mix

ground state 

$charge $multi
EOF

addGeoCoord $1 $2.com
}

function readPrev() {
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

function readMin() {
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

$charge $multi
EOF 

addGeoCoord $2 $3.com

}


function readS0() {
    # $1 = bond 
    # $2 = name of file

    # generate G09 input file
cat << EOF > $2.com
%chk=$2.chk
%mem=600MW
%nprocshared=4
# $EST_S0/$basis Nosymm

ground state 

$charge $multi
EOF 

addGeoCoord $1 $2.com

cat << EOF >> $2.com
--Link1--
%chk=$2.chk
%mem=600MW
%nprocshared=4
#p CASSCF($nele,$norb,NRoot=$nroot)/$basis Guess=read geom=check Nosymm SCF=(NoVarAcc,NoIncFock) 

$nele electrons, $norb orbital

$charge $multi
EOF
}

function addGeoCoord {
    # $1 = Bond length
    # $2 = file name
cat << EOF >> $2
H
H 1 $1

EOF
}

main 