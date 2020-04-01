%chk=R_5.00.chk
%mem=600MW
%nprocshared=4
#p CASSCF(2,2,NRoot=2)/6-31G(d) Guess=read  Nosymm  SCF=(NoVarAcc, NoIncFock)  

2 electrons, 2 orbital

0 1
EOF 

# addGeoCoord 5.00 R_5.00.com

}

function readMin {
    # 10.00_triplet_ROHF.chk = Min.chk
    # 0 = bond length
    # 5.00 = name of file

    cp 10.00_triplet_ROHF.chk 5.00.chk
    # generate G09 input file
cat << EOF > 5.00.com
%chk=5.00.chk
%mem=600MW
%nprocshared=4
#p CASSCF(2,2,NRoot=2)/6-31G(d) Guess=read  Nosymm  SCF=(NoVarAcc, NoIncFock)  

2 electrons, 2 orbital

0 1
EOF 

addGeoCoord 0 5.00.com

}


function readS0 {
    # 10.00_triplet_ROHF.chk = bond 
    # 0 = name of file

    # generate G09 input file
cat << EOF > 0.com
%chk=0.chk
%mem=600MW
%nprocshared=4
# UHF/6-31G(d) Nosymm

ground state 

0 1
EOF 

addGeoCoord 10.00_triplet_ROHF.chk 0.com

cat << EOF >> 0.com
--Link1--
%chk=0.chk
%mem=600MW
%nprocshared=4
#p CASSCF(2,2,NRoot=2)/6-31G(d) Guess=read geom=check Nosymm SCF=(NoVarAcc,NoIncFock) 

2 electrons, 2 orbital

0 1

