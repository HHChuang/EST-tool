#!/usr/bin/env python3
import sys

def parse_hess(filename):
    with open(filename) as f:
        lines = f.readlines()
    data = {}
    i = 0
    while i < len(lines):
        line = lines[i].strip()
        if line == "$atoms":
            i += 1
            natoms = int(lines[i].strip())
            atoms = []
            for _ in range(natoms):
                i += 1
                parts = lines[i].split()
                atoms.append((parts[0], float(parts[2]), float(parts[3]), float(parts[4])))
            data['atoms'] = atoms
        elif line == "$vibrational_frequencies":
            i += 1
            nfreq = int(lines[i].strip())
            freqs = []
            for _ in range(nfreq):
                i += 1
                freqs.append(float(lines[i].split()[1]))
            data['freqs'] = freqs
        elif line == "$normal_modes":
            i += 1
            parts = lines[i].split()
            nrows, ncols = int(parts[0]), int(parts[1])
            modes = [[0.0]*ncols for _ in range(nrows)]
            col_offset = 0
            while col_offset < ncols:
                i += 1
                chunk_cols = len(lines[i].split())
                for r in range(nrows):
                    i += 1
                    vals = lines[i].split()
                    for c, v in enumerate(vals[1:]):
                        modes[r][col_offset + c] = float(v)
                col_offset += chunk_cols
            data['modes'] = modes
        elif line == "$ir_spectrum":
            i += 1
            nir = int(lines[i].strip())
            ir = []
            for _ in range(nir):
                i += 1
                parts = lines[i].split()
                ir.append((float(parts[0]), float(parts[1])))
            data['ir'] = ir
        i += 1
    return data

def bohr_to_ang(x):
    return x * 0.529177210903

def write_molden(data, outfile):
    atoms = data['atoms']
    freqs = data['freqs']
    modes = data['modes']
    ir = data.get('ir', [])
    natoms = len(atoms)
    nmodes = len(freqs)
    sym_to_Z = {'H':1,'C':6,'N':7,'O':8,'F':9,'S':16,'Cl':17}
    with open(outfile, 'w') as f:
        f.write("[Molden Format]\n")
        f.write("[Atoms] Angs\n")
        for idx, (sym, x, y, z) in enumerate(atoms, 1):
            Z = sym_to_Z.get(sym, 0)
            f.write(f"  {sym:<4s} {idx:4d} {Z:4d}   {bohr_to_ang(x):14.8f}  {bohr_to_ang(y):14.8f}  {bohr_to_ang(z):14.8f}\n")
        f.write("[FREQ]\n")
        for freq in freqs:
            f.write(f"  {freq:.6f}\n")
        f.write("[FR-COORD]\n")
        for sym, x, y, z in atoms:
            f.write(f"  {sym:<4s}  {x:14.8f}  {y:14.8f}  {z:14.8f}\n")
        f.write("[FR-NORM-COORD]\n")
        for mode_idx in range(nmodes):
            f.write(f"vibration {mode_idx+1}\n")
            for atom_idx in range(natoms):
                dx = modes[atom_idx*3][mode_idx]
                dy = modes[atom_idx*3+1][mode_idx]
                dz = modes[atom_idx*3+2][mode_idx]
                f.write(f"  {dx:14.8f}  {dy:14.8f}  {dz:14.8f}\n")
        f.write("[INT]\n")
        for mode_idx in range(nmodes):
            intensity = ir[mode_idx][1] if mode_idx < len(ir) else 0.0
            f.write(f"  {intensity:.6f}\n")
    print(f"Written: {outfile}")

if __name__ == "__main__":
    infile = sys.argv[1] if len(sys.argv) > 1 else "ORCA.hess"
    outfile = infile.replace('.hess', '.molden')
    print(f"Reading {infile} ...")
    data = parse_hess(infile)
    write_molden(data, outfile)
    print(f"Open with: jmol {outfile}")