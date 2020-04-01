%chk=R_3.50.chk
%mem=600MW
%nprocshared=4
#p CASSCF(2,2,NRoot=2)/6-31G(d) Guess=read  Nosymm  SCF=(NoVarAcc, NoIncFock)  

2 electrons, 2 orbital

0 1
EOF 

# addGeoCoord 3.50 R_3.50.com

}

function readMin {
    # 10.00_triplet_ROHF.chk = Min.chk
    # 1 = bond length
    # 3.50 = name of file

    cp 10.00_triplet_ROHF.chk 3.50.chk
    # generate G09 input file
cat << EOF > 3.50.com
%chk=3.50.chk
%mem=600MW
%nprocshared=4
#p CASSCF(2,2,NRoot=2)/6-31G(d) Guess=read  Nosymm  SCF=(NoVarAcc, NoIncFock)  

2 electrons, 2 orbital

0 1
EOF 

addGeoCoord 1 3.50.com

}


function readS0 {
    # 10.00_triplet_ROHF.chk = bond 
    # 1 = name of file

    # generate G09 input file
cat << EOF > 1.com
%chk=1.chk
%mem=600MW
%nprocshared=4
# UHF/6-31G(d) Nosymm

ground state 

0 1
EOF 

addGeoCoord 10.00_triplet_ROHF.chk 1.com

cat << EOF >> 1.com
--Link1--
%chk=1.chk
%mem=600MW
%nprocshared=4
#p CASSCF(2,2,NRoot=2)/6-31G(d) Guess=read geom=check Nosymm SCF=(NoVarAcc,NoIncFock) 

2 electrons, 2 orbital

0 1

