 #!/bin/bash

nameroot='glycine'
R0=1.0
Req=1.50832
dR=0.1
step=30
nroot=2
nele=6
norb=7
basis='6-31G(d)'

function main {
    
    for ((i=0;i<$step;i++))
    do 
	    Bond=$( echo $R0 + $dR*$i | bc -l )
	    name=$( echo $nameroot\_$Bond )

        # tune the initial guess
        # 1. read the orbitals from the optimized ground state
        readMin Min.chk $Bond $name 
        # 2. read the orbitals from UHF
        # readS0 $Bond $name 
        # 3. read the orbitals form the previous point TODO:
        # readPrevious $Bond $name  

        # submit jobs
        g16sub $name.com $name.log
    done
}

function readMin {
    # $1 = Min.chk
    # $2 = bond 
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
   B4             $2
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

--Link1--
%chk=$2.chk
%mem=600MW
%nprocshared=4
#p CASSCF($nele,$norb,NRoot=$nroot)/$basis Guess=read geom=check Nosymm SCF=(NoVarAcc,NoIncFock) 

$nele electrons, $norb orbital

0 1

EOF
}

main 