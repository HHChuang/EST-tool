# *** Q-Chem 3.1 Parallel Version Configuration

export QCAUX=/opt/qchem/aux/5.1.2
export QC=/opt/qchem/cvs

#
# setting for parallel jobs
# !! need more information about this !!
#
export QCMPI=openmpi
#export QCTHREADS=1
export QCRSH=csh

# manage scratch file output
#
#export QCLOCALSCR=/scratch/$USER/qclocal
export QCSCRATCH=/scratch/$USER/qchem

#if [ $QCGLOBAL ];
#then
#	export QCSCRATCH=$QCGLOBAL
#	export QCLOCALSCR=/scratch/$USER/qchem
#fi

noclobber=""

#if [ -e $QC/bin/qchem.setup.sh ]; then
#        source $QC/bin/qchem.setup.sh
#fi

export PATH=/opt/qchem5/bin:/opt/qchem/bin:$QC/bin:$QC/bin/perl:$QC/exe:$QC/util:$PATH
#source /opt/intel/compilers_and_libraries_2019.3.199/linux/mkl/bin/mklvars.sh intel64
source /opt/intel/compilers_and_libraries_2019.3.199/linux/bin/compilervars.sh intel64
source /opt/intel/compilers_and_libraries_2019.3.199/linux/mpi/intel64/bin/mpivars.sh intel64
source /opt/mpich/mpich.sh
source /opt/openmpi/openmpi.sh
export LD_LIBRARY_PATH=/opt/gcc-8.2.0/lib64:$LD_LIBRARY_PATH

