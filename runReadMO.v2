#!/bin/bash
#2020/10/13, Grace
# $1 = fixed DA

R0=2.20 # integer
dR=0.2
step=14

function main {
	
	Ri=$R0

    rm -f DA_Eff$1.dat 
	for ((i=1;i<=$step;i++))
	do
		# rxn. coord. 
		    R_iplus1=$(echo "scale=2; $Ri + $dR" | bc -l)
            
		
		# create 1. molpro input, 2. sge file
            genInp $1 $Ri $R_iplus1 # output variable: genInp = file_fin

            genSGEfile $genInp 
            (time qsub -sync y -pe serial 8 job$genInp.sge ) 2>>DA_Eff$1.dat	

            Ri=$R_iplus1
            
		# systyem error: too many cases fail because of full of disk
		# solution: 
		# 	1. add loop to assign another node automatically
		# 	2. add two path; TEMDIR TEMDIR2, in the sge files 
            termina=$(grep -c 'Molpro calculation terminated' $genInp.out)
            # [ $termina -eq 0 ] || break 
            rerun=0
            while [ $termina -eq 0 ]
            do 
                rerun=$(($rerun+1))
                echo "rerun: $rerun"
                (time qsub -sync y -pe serial 4 job$genInp.sge ) 2>>DA_Eff$1.dat
                termina=$(grep -c 'Molpro calculation terminated' $genInp.out)
		    done 
	done

	# Std-out to remind myself
	echo "Check DA_Eff$1.dat file for debugging"
}

function genInp()
{
    # $1 = fixed angle
    # $2 = $Ri
    # $3 = $R_iplus1

    NAtoms=10
    dof=22 # fixed 2 dof

    DA=$1 
    Rini=$2
    Rfin=$3
    file_ini="$Rini"_"$DA"
    file_fin="$Rfin"_"$DA"

    cp $file_ini.inp $file_fin.inp
    cp $file_ini.wfu $file_fin.wfu
    
    sed -i "s/$file_ini/$file_fin/g" $file_fin.inp 
    sed -i '21,44d' $file_fin.inp 
    sed -i '21a pointer' $file_fin.inp
    grep -A $dof 'Optimized variables' $file_ini.out \
        | tail -n $dof | awk '{print $1,$2}' > opt.dat 
    echo "B4= $Rfin" >> opt.dat 
    echo "D5= $DA" >> opt.dat
    echo '' >> opt.dat 
    sed -i '/pointer/r opt.dat' $file_fin.inp 
    sed -i '/pointer/d' $file_fin.inp
    rm -f opt.dat 

    genInp=$file_fin
}

function genSGEfile {
# $1 = filename
cat << EOF > job$1.sge
#!/bin/bash
export PATH=\$PATH:\$HOME/bin:/opt/util

### Default Control
#$ -S /bin/sh -w w -j y -cwd        ### Run job through bash shell
#$ -j y                 ### Join stdout and stderr
#$ -cwd                 ### Use current working directory

### SGE Environment
echo '== SGE Environment =='
echo "Working directory is \$SGE_O_WORKDIR"
cd \$SGE_O_WORKDIR

echo 'Job starts'
echo "    Host: \$HOSTNAME"
echo '    Date:' `date`
echo 'Directory:' `pwd` 

### Molpro Setup 
export MOLPRO_EXEDIR=/opt/molpro/molpro_2019_2_linux_x86_64_i8/bin
export MOLPRO_EXE=\$MOLPRO_EXEDIR/molpro
export TMPDIR=/scratch/\$USER/molpro1/\$HOSTNAME
export TMPDIR2=/scratch/\$USER/molpro2/\$HOSTNAME
if [ ! -d \${TMPDIR} ]; then
   mkdir -p \${TMPDIR}
fi
if [ ! -d \${TMPDIR2} ]; then
   mkdir -p \${TMPDIR2}
fi

export PATH=\$MOLPRO_EXEDIR:\$TMPDIR:\$TMPDIR2:$PATH

### Job Script
echo "Your job:"
time \$MOLPRO_EXE  -W \$SGE_O_WORKDIR -o $1.out -s $1.inp
EOF
}

main $1


