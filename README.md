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
   - `getIRCPEC`: 
   - `getIRCcurve`: Extract IRC energy curve explicitly form G09 IRC output file
   - `getIRCstruc`: Extract a serious of structures form G09 IRC output file
   - `getJob`: Extract the job name and directory of the running jobs
   - `getMOdensity`:
   - `getNM`: Extract the normal mode eigenvector	from Gaussian/QChem job	
   - `hartree2eV`:
   - `hartree2kcal`:
   - `plotMO`:
   - `qesub`: Create the configuration file of EST jobs for SGE queuing system 
   - `rev1Dstruc`:
   - `run1DBond`:
   - `runIRC`:
   - `runReadMO.v2`:
   - `writeGauInp`: Extract the coordinate and then write the input file for Gaussian program.
   - `writeGauInpV`:
2. Fortran
   - `Vscan1D`: Produce a serious of structure along a selected coordinate.
   - `ComW`: Find out the centre-of-mass of attached water
   <!-- - `AddWDir` - 2015/10/17,Grace
      - Approximate the H-Bond is linear, and then construct the H-Bond network (1st shell) input files	
      - $1=structure of the system
      - $2=header file for gaussian program route section, description, charge and multiplicity.
      - $3=boundary condition
   - `EffTime` - 2015/10/22,Grace
      - Calculate the average time, as well as maximum and minimum
   - `WritePOPT` - 2015/11/26,Grace
      - Modify the g09 sp input file, and then produce a series of g09 input for partial optimization
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
   - `GetDia` - 2016/05/04,Grace
     - Calculate the diabatic PES after FCD calculation
     - $1=FCD.txt
   - `TISE_1D_DVR` - 2016/04/15,Grace
		- Solve the numerical TISE via DVR method
		- $1=PES.txt
		- $2=reduced mass -->
3. Python
4. Others
   <!-- - `moviemol` 
     - [ref.](http://www.ifm.liu.se/compchem/moviemol/moviemol.html)
   - `plotContour.sh.gnu` - 2016/08/04, Grace
     - plot contour -->

<!-- ---
## History:
- 2015/10/13, Grace, 1st. ver.			
- 2016/11/04, Grace, 2nd. ver.									
- 2016/11/04, Grace, 2nd. ver.									 -->
