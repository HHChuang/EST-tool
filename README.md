# EST-tool

Tools for electronic structure theory (EST) calculations (e.g. Gaussian16, Q-Chem, ORCA, Molpro).

Electronic structure theory computes the electronic energy and wavefunction of a molecular system (e.g. via Hartree-Fock, DFT, CASSCF or post-HF methods), and is used to characterise minima, transition states and reaction paths on a potential energy surface (PES). Running these calculations at scale involves a lot of repetitive, error-prone bookkeeping: writing input files, submitting jobs to an HPC queue, checking whether a run converged or crashed, and pulling structures, energies, frequencies or normal modes back out of the output. This repository collects small, single-purpose command-line scripts that automate that surrounding workflow — job submission (`qesub`), input generation (`write*`), output/status checking (`check*`), and post-processing/data extraction (`get*`, `hartree2*`, `Vscan2D.py`, `hess2molden.py`) — for Gaussian16, Q-Chem, ORCA and Molpro.

## Programming Languages

- Bash shell script
- Python
- Fortran

## Naming convention

| Prefix | Meaning |
|---|---|
| `check*`, `Check*` | Check job status |
| `get*`, `Get*` | Extract selected data |
| `qesub`, `write*` | Submit / write input file(s) |

Older, single-program predecessors of `checkEst`, `qesub` and `writeInpV` (`checkGau`, `checkQch`, `qesub_SGE`, `writeGauInpV`, `writeQchemInpV`) have been merged into those scripts and moved to `src/Archive`.

## Programs

### Bash

| Program | Purpose |
|---|---|
| `calcIRCvec` | Create a series of vectors along an IRC/structure series |
| `checkEst` | Check the status of a GAUSSIAN16/Q-Chem/ORCA output file (or a `qesub -w` sweep directory): classify stationary points (SP, MIN, Saddle, IRC), extract energies, optionally diff against a reference directory. Merges the former `checkGau`/`checkQch`; `checkMol`'s 2D grid-scan mode remains separate.<br>`$ checkEst [ -e g16\|qchem\|orca ] [ -r refDir ] [ -tol tolerance ] infile\|sweep` |
| `checkMol` | Check the status of Molpro output file(s) and grep energy for gnuplot |
| `getCoord` | Extract the optimised coordinate from a Gaussian/QChem output file |
| `getIRCPEC` | Build the IRC potential energy curve from a forward/reverse IRC pair (run after `getIRCstruc`, `rev1Dstruc`, `rot.py`) |
| `getIRCcurve` | Extract the IRC energy curve from a G09 IRC output file |
| `getIRCstruc` | Extract a series of structures from a G09 IRC output file |
| `getJob` | List the job name and directory of currently running jobs |
| `getMOdensity` | Square a set of MO coefficients into MO densities |
| `getNM` | Extract the normal mode eigenvectors from a Gaussian/QChem job |
| `hartree2eV` | Convert energy units from Hartree to eV |
| `hartree2kcal` | Convert energy units from Hartree to kcal/mol |
| `plotMO` | Plot MO energy spectra alongside pre-rendered MO figures (via Jmol) |
| `qesub` | Create SLURM batch script(s) to submit EST input file(s). Migrated from the SGE queuing system to SLURM (HPC Hannover); the SGE version is kept as `qesub_SGE` in `src/Archive`.<br>`$ qesub [ -e orca\|g16\|jellyfish\|qchem\|molpro ] [ -np procs ] [ -w node\|all\|node1,node2,... ] [ -N n\|auto ] infile`<br>`-w` sweeps the job list across one or more pinned nodes; `-N` splits the job list across N nodes (or `auto`, one per partition node) |
| `rev1Dstruc` | Reverse the order of a 1D structure series (e.g. an IRC reverse leg) and flip the sign of the coordinate index |
| `run1DBond` | Scan a PEC where the varied degree of freedom is a bond length |
| `runIRC` | Generate a batch of IRC inputs with varied parameters to brute-force additional IRC grid points |
| `runReadMO.v2` | Scan a fixed donor-acceptor distance, generating a Molpro input/submission file per grid point |
| `writeGauInp` | Extract the coordinate from a Gaussian/QChem output file and write a new Gaussian input file |
| `writeInpV` | Extract structures from a multi-frame xyz trajectory and write Gaussian, Q-Chem or ORCA input files. Merges the former `writeGauInpV`/`writeQchemInpV`; adds ORCA support.<br>`$ writeInpV trajectory.xyz [ header/options file ]` |

### Fortran

| Program | Purpose |
|---|---|
| `ComW` | Find the centre-of-mass of attached water molecules |

### Python

| Program | Purpose |
|---|---|
| `Vscan2D.py` | Scan a 1D or 2D PEC/PES.<br>`$ python Vscan2D.py mol.xyz scan.dat`<br>output: `scan1D.xyz` or `scan2D.xyz` (see `/run`) |
| `hess2molden.py` | Convert an ORCA `.hess` file (atoms, frequencies, normal modes, IR intensities) into a Molden-format file for visualisation.<br>`$ python hess2molden.py ORCA.hess`<br>output: `ORCA.molden` (open with e.g. `jmol`) |

<details>
<summary>Historical / superseded entries (not present in <code>src/</code>, kept for reference)</summary>

**File naming convention (original scheme)**
- Bash shell script: lowercase, e.g. `check*`, `get*`
- Fortran: capitalised, e.g. `Get*`
- Gnuplot: `*.gnu` (header: `#!/usr/bin/gnuplot -persist`)
- Python: `*.py` (header: `#!/usr/bin/env python`)
- Matlab: `*.m` (run without X-window: `matlab -nodisplay`)
- Mixed: `*.sh.gnu` (Bash + Gnuplot), `*.sh.py` (Bash + Python)

**Fortran utilities (pre-2016, not carried into `src/`)**

| Program | Date | Purpose |
|---|---|---|
| `AddWDir` | 2015/10/17 | Approximate the H-bond as linear and construct the H-bond network (1st shell) input files. `$1`=structure, `$2`=Gaussian route/description/charge/multiplicity header, `$3`=boundary condition |
| `EffTime` | 2015/10/22 | Calculate average, maximum and minimum time |
| `WritePOPT` | 2015/11/26 | Modify a g09 SP input and produce a series of g09 partial-optimisation inputs |
| `FilterW` | 2015/11/28 | Filter out redundant water molecules. `$1`=`comW.txt` |
| `AnalyseTS` | 2015/12/01 | Compare eigenvalues (frequencies), then analyse the normal-mode eigenvector. `$1`=`list.txt`, `*.out` |
| `AnaCub` | 2016/01/15 | Produce the numerical density and corresponding Cartesian coordinate. `$1`=`*.cub` |
| `AnalyseIRC` | 2016/02/11 | — |
| `GetDia` | 2016/05/04 | Calculate the diabatic PES after an FCD calculation. `$1`=`FCD.txt` |
| `TISE_1D_DVR` | 2016/04/15 | Solve the numerical TISE via the DVR method. `$1`=`PES.txt`, `$2`=reduced mass |

**Others**
- `moviemol` ([ref.](http://www.ifm.liu.se/compchem/moviemol/moviemol.html))
- `plotContour.sh.gnu` (2016/08/04) — plot contour

</details>

---

## History

- 2015/10/13, Grace, 1st ver.
- 2016/11/04, Grace, 2nd ver.
- 2026/07/14, Grace, 3rd ver. — merged `checkGau`/`checkQch` into `checkEst`, `writeGauInpV`/`writeQchemInpV` into `writeInpV`, migrated `qesub` to SLURM, added ORCA support and `hess2molden.py`
