# Purpose
   Tools for electronic structure theory (EST) calculation (e.g. Gaussian, QChem or Molpro). 
   
## Programming Language : 
- Fortran
- Bash shell script
- Gnuplot
- Python
<!-- - Matlab -->

<!-- ## 2. File name: 

### Single programming language:
1. Bash shell script: start from lowercase, ex. check*,get*
2. Fortran: start from capital, ex. Get*
3. Gnuplot: ex. *.gnu (header: #!/usr/bin/gnuplot -persist)
4. Python: ex. *.py (header: #!/usr/bin/env python)
5. Matlab: ex. *.m (use command line without x-window: $ matlab -nodisplay)

- Mixed programming language
1. Bash shell + Gnuplot : *.sh.gnu
2. Bash shell + Python: *.sh.py -->

## Purpose of main category
1. check*, Check* 
   - Check the status
2. get*, Get*
   - Extract the selected data
3. qsub*
   - Submit input file(s)

## 4. Instruction of each program
1. **Bash shell script**
   - `calcIRCvec`: Create a serious of vectors
   - `checkGau`: Check the status of Gaussian16 output file
   - `checkMol`: Check the status of Molpro output file
   - `checkQch`: Check the status of Q-Chem output file
   - `getCoord`: Extract the optimized coordinate from a Gaussian/QChem output file 
   - `getIRCcurve`: Extract IRC energy curve explicitly form G09 IRC output file
   - `getIRCstruc`: Extract a serious of structures form G09 IRC output file
   - `getNM`: Extract the normal mode eigenvector	from Gaussian/QChem job	
   
   - `getSPE` - 2015/10/22,Grace
	   - Extract the SP energy from g09 output *.out
   - `qesub`: Create the configuration file of EST jobs for SGE queuing system 
      - $1=file
   - `getMO` - 2016/04/13,Grace
      - Extract the MO information and then use Jmol to visualize
      - $1=qchem output file
	- `getGauCoord` - 2016/10/24,Grace
		- Extract the optimized coordinate of G09 output file
		- $1=G09 output
	- `getJob` - 2015/10/13,Grace
		- Extract the job name and directory of the running jobs
	- `cpSelectJob` - 2015/11/24,Grace
		- Copy select jobs to assign path         - $1=select list, $2=assign path
   - `writeGauInp` - 2015/11/30,Grace
		- Extract the coordinate and then write the input file for Gaussian program.
      - $1=header.txt, *.out
   - `qchem2gau` - 2016/04/14,Grace
      - Extract the structure from QChem to gaussian format input file              
      - $1=qchem input
      - $2=header file
2. Fortran
   - `AddWDir` - 2015/10/17,Grace
      - Approximate the H-Bond is linear, and then construct the H-Bond network (1st shell) input files	
      - $1=structure of the system
      - $2=header file for gaussian program route section, description, charge and multiplicity.
      - $3=boundary condition
   - `EffTime` - 2015/10/22,Grace
      - Calculate the average time, as well as maximum and minimum
   - `WritePOPT` - 2015/11/26,Grace
      - Modify the g09 sp input file, and then produce a series of g09 input for partial optimization
      - $1=header file , *.com=sp input
   - `ComW` - 2015/11/27,Grace
      - Find out the centre-of-mass of attached water       
      - $1=list.txt , *.out=opt files
   - `FilterW` - 2015/11/28,Grace
      - Filter out the redundant water molecules
      - $1=comW.txt
   - `AnalyseTS` - 2015/12/01,Grace
     - First, compare the eigenvalue (frequency), and then analyse the eigenvector of normal mode.
      - $1=list.txt, *.out
   - `AnaCub` - 2016/01/15,Grace
     - Produce the numerical density and the corresponding Cartesian coordinate.
      - $1=*.cub
   - `AnalyseIRC` - 2016/02/11,Grace
   - `Vscan1D` - 2016/04/13,Grace
     - Produce a serious of structure along a selected coordinate.
   	- $1=initial structure in xyz formate
      - $2=vector (xyz coordinate)
      - $3=boundary condition	
   - `GetDia` - 2016/05/04,Grace
     - Calculate the diabatic PES after FCD calculation
     - $1=FCD.txt
   - `TISE_1D_DVR` - 2016/04/15,Grace
		- Solve the numerical TISE via DVR method
		- $1=PES.txt
		- $2=reduced mass
1. Others
   - `moviemol` 
     - [ref.](http://www.ifm.liu.se/compchem/moviemol/moviemol.html)
   - `plotContour.sh.gnu` - 2016/08/04, Grace
     - plot contour

<!-- ---
## History:
- 2015/10/13, Grace, 1st. ver.			
- 2016/11/04, Grace, 2nd. ver.									
- 2016/11/04, Grace, 2nd. ver.									 -->
