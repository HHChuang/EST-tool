%chk=R_0.50.chk
%mem=600MW
%nprocshared=4
#p CASSCF(2,2,NRoot=2)/6-31G(d) Guess=read  Nosymm  SCF=(NoVarAcc, NoIncFock)  

2 electrons, 2 orbital

0 1
EOF 

# addGeoCoord 0.50 R_0.50.com

}

function readMin {
    # 10.00_triplet_ROHF.chk = Min.chk
    # 3 = bond length
    # 0.50 = name of file

    cp 10.00_triplet_ROHF.chk 0.50.chk
    # generate G09 input file
cat << EOF > 0.50.com
%chk=0.50.chk
%mem=600MW
%nprocshared=4
#p CASSCF(2,2,NRoot=2)/6-31G(d) Guess=read  Nosymm  SCF=(NoVarAcc, NoIncFock)  

2 electrons, 2 orbital

0 1
EOF 

addGeoCoord 3 0.50.com

}


function readS0 {
    # 10.00_triplet_ROHF.chk = bond 
    # 3 = name of file

    # generate G09 input file
cat << EOF > 3.com
%chk=3.chk
%mem=600MW
%nprocshared=4
# UHF/6-31G(d) Nosymm

ground state 

0 1
EOF 

addGeoCoord 10.00_triplet_ROHF.chk 3.com

cat << EOF >> 3.com
--Link1--
%chk=3.chk
%mem=600MW
%nprocshared=4
#p CASSCF(2,2,NRoot=2)/6-31G(d) Guess=read geom=check Nosymm SCF=(NoVarAcc,NoIncFock) 

2 electrons, 2 orbital

0 1

