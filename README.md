# Overview
   **Purpose:** 
   Tools for electronic structure theory (EST) calculation (e.g. Gaussian, QChem). 
   
   **What I learn**
   'We make a living by what we get, but we make a life by what we give.- Winston Churchill'
   

## 1. Programming Language : 
- Bash shell script
- Fortran 
- Gnuplot
- Python
- Matlab

## 2. File name: 

### Single programming language:
1. Bash shell script: start from lowercase, ex. check*,get*
2. Fortran: start from capital, ex. Get*
3. Gnuplot: ex. *.gnu (header: #!/usr/bin/gnuplot -persist)
4. Python: ex. *.py (header: #!/usr/bin/env python)
5. Matlab: ex. *.m (use command line without x-window: $ matlab -nodisplay)

- Mixed programming language
1. Bash shell + Gnuplot : *.sh.gnu
2. Bash shell + Python: *.sh.py

## 3. Purpose of main category
1. check*,Check* 
   - Check the status
2. get*, Get*
   - Extract the selected data
3. qsub*
   - Submit the job
## 4. Instruction of each program
1. **Bash shell script**
   - `qsubGau` - 2015/10/13,Grace
		- Creat the submitted file of GAUSSIAN09 input file in blackberry server.
		- Benchmark/one-file mode
   - `checkGau` - 2016/11/04,Grace
		- Check the status of G09 output file
		- Benchmark/one-file mode
		- $1=file or document
	- `getGauCoord` - 2016/10/24,Grace
		- Extract the optimized coordinate of G09 output file
		- $1=G09 output
	- `getJob` - 2015/10/13,Grace
		- Extract the job name and directory of the running jobs
	- `cpSelectJob` - 2015/11/24,Grace
		- Copy select jobs to assign path         - $1=select list, $2=assign path
	- `checkInOut` - 2016/01/20,Grace
		- Extract the un-submitted jobs by comparing the input and output list
   - `writeGauInp` - 2015/11/30,Grace
		- Extract the coordinate and then write the input file for Gaussian program.
      - $1=header.txt, *.out
   - `getSPE` - 2015/10/22,Grace
	   - Extract the SP energy from g09 output *.out
   - `checkQchem` - 2016/11/04,Grace
      - Check the status of Qchem output file
      - Benchmark/one-file mode
      - $1=file or document
   - `getQchemCoord` - 2016/04/12,Grace
      - Extract the optimized coordinate from a QChem output file
      - $1=file
   - `getMO` - 2016/04/13,Grace
      - Extract the MO information and then use jmol to visualize
      - $1=qchem output file
   - `qchem2gau` - 2016/04/14,Grace
      - Extract the structure from qchem to gaussian format input file              
      - $1=qchem input
      - $2=header file
   - `getQchemPES_SF-CIS` - 2016/04/18,Grace
      - Extract the energy of PES from qchem output
   - `getFCD` - 2016/05/02,Grace
      - Extract the matrix element from the FCD calculation
   - `plotAmpl` - 2016/06/30,Grace
      - Extract the amplitude form SF-CIS calculation and plot it	
   - `getOneRas` - 2016/08/19,Grace
      - Extract energy of several states of RAS-nSF calculation
      - $1=onefile.txt
2. Fortran
   - `AddWDir` - 2015/10/17,Grace
      - Approximate the H-Bond is linear, and then construct the H-Bond network (1st shell) input files	
      - $1=structure of the system
      - $2=header filem for gaussian program route section, description, charge and multiplicity.
      - $3=boundary condition
   - `EffTime` - 2015/10/22,Grace
      - Calculate the average time, as well as maximuma and minimum
   - `WritePOPT` - 2015/11/26,Grace
      - Modify the g09 sp inpt file, and then produce a series of g09 input for partial optimization
      - $1=header file , *.com=sp input
   - `ComW` - 2015/11/27,Grace
      - Find out the centre-of-mass of attached water       
      - $1=list.txt , *.out=opt files
   - `FilterW` - 2015/11/28,Grace
      - Filter out the redundent water molecules
      - $1=comW.txt
   - `AnalyseTS` - 2015/12/01,Grace
     - First, compare the eigenvalue (frequency), and then analyse the eigenvector of normal mode.
      - $1=list.txt, *.out
   - `AnaCub` - 2016/01/15,Grace
     - Produce the numerical density and the corresponding Cartesian coordinate.
      - $1=*.cub
   - `AnalyseIRC` - 2016/02/11,Grace
   - `Vscan1D` - 2016/04/13,Grace
     - Produce a serious of structure along a selected oordinate.
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

---
## History:
- 2015/10/13, Grace, 1st. ver.			
- 2016/11/04, Grace, 2nd. ver.									
