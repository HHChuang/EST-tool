%chk=R_2.00.chk
%mem=600MW
%nprocshared=4
#p CASSCF(2,2,NRoot=2)/6-31G(d) Guess=read  Nosymm  SCF=(NoVarAcc, NoIncFock)  

2 electrons, 2 orbital

0 1
EOF 

# addGeoCoord 2.00 R_2.00.com

}

function readMin {
    # 10.00_triplet_ROHF.chk = Min.chk
    # 2 = bond length
    # 2.00 = name of file

    cp 10.00_triplet_ROHF.chk 2.00.chk
    # generate G09 input file
cat << EOF > 2.00.com
%chk=2.00.chk
%mem=600MW
%nprocshared=4
#p CASSCF(2,2,NRoot=2)/6-31G(d) Guess=read  Nosymm  SCF=(NoVarAcc, NoIncFock)  

2 electrons, 2 orbital

0 1
EOF 

addGeoCoord 2 2.00.com

}


function readS0 {
    # 10.00_triplet_ROHF.chk = bond 
    # 2 = name of file

    # generate G09 input file
cat << EOF > 2.com
%chk=2.chk
%mem=600MW
%nprocshared=4
# UHF/6-31G(d) Nosymm

ground state 

0 1
EOF 

addGeoCoord 10.00_triplet_ROHF.chk 2.com

cat << EOF >> 2.com
--Link1--
%chk=2.chk
%mem=600MW
%nprocshared=4
#p CASSCF(2,2,NRoot=2)/6-31G(d) Guess=read geom=check Nosymm SCF=(NoVarAcc,NoIncFock) 

2 electrons, 2 orbital

0 1

