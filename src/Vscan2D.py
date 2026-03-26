# !/usr/bin/env python
#########################################################################
#                                                                       #                        
#   Program: Generate molecular structure by scanning two-dimensional   #
#   potential energy surface.                                           #
#                                                                       #                        
#   Input:                                                              #                        
#       1. molecular structure; $1, extension: .xyz                     #                       
#       2. selected degrees of freedom; extension: .dat                 #                        
#          Format: alternating lines of DOF specification and moving    #
#          atoms                                                        #
#          DOF line: atom_indices initial final stepsize                #                        
#          Moving atoms line: space-separated list of atom indices to   #
#          move                                                         #
#          Example:                                                     #                         
#          # First DOF (bond)                                           #                         
#          1 2 -10 10 0.01                                              #                         
#          # moving atoms                                               #                         
#          2                                                            #                         
#          # Second DOF (angle)                                         #                         
#          1 2 3 -5 5 0.1                                               #                         
#          # moving atoms                                               #                         
#          3                                                            #                         
#                                                                       #                        
#   Output:                                                             #                        
#       1. Std-out:                                                     #                        
#       2. structure of potential energy surface                        #
#                                                                       #
# History:                                                              #
# 2026/03/25, Grace                                                     #               
#                                                                       #
#########################################################################

# %%
import numpy as np
import re
import sys
import os

def read1file(file):
    '''
    Purpose: Read molecular structure from XYZ format file.
    
    Input file format (.xyz):
        Line 1: number of atoms (integer)
        Line 2: comment or description (any text, ignored)
        Lines 3+: atom_symbol  x  y  z  (space-separated)
    
    Output:
        List of tuples: [(atom_symbol, x, y, z), ...]
        Each tuple represents one atom with its element and 3D coordinates.
    
    Example input:
        3
        H2O molecule
        H 1.0 1.414 0.0
        O 0.0 0.0 0.0
        H 1.0 -1.414 0.0
    
    Example output:
        [('H', 1.0, 1.414, 0.0), ('O', 0.0, 0.0, 0.0), ('H', 1.0, -1.414, 0.0)]
    '''
    with open(file, "r") as f:
        lines = [ln.strip() for ln in f if ln.strip()]

    if len(lines) < 2:
        raise ValueError(f"Invalid XYZ file '{file}': not enough lines")

    try:
        natoms = int(lines[0])
    except ValueError as e:
        raise ValueError(f"Invalid number of atoms in '{file}': {lines[0]}") from e

    if len(lines) < 2 + natoms:
        raise ValueError(f"Invalid XYZ file '{file}': expected {natoms} atoms, got {len(lines)-2}")

    struc = []
    for i in range(2, 2 + natoms):
        parts = lines[i].split()
        if len(parts) < 4:
            raise ValueError(f"Invalid geometry line in '{file}': {lines[i]}")
        atom = parts[0]
        x, y, z = map(float, parts[1:4])
        struc.append((atom, x, y, z))

    if len(struc) != natoms:
        raise ValueError(f"Parsed {len(struc)} atoms but header says {natoms}")

    return struc

def read2file(file):
    '''
    Purpose: Read degrees of freedom (DOF) specifications from scan definition file.
    
    Input file format (.dat):
        Lines starting with '#' are comments and ignored.
        Data lines come in alternating pairs:
          1. DOF line: atom_indices initial final stepsize
          2. Moving atoms line: space-separated atom indices
        
        DOF types and atom counts:
          - 2 atoms: bond (e.g., "1 2 -10 10 0.01")
          - 3 atoms: angle (e.g., "1 2 3 -5 5 0.1")
          - 4 atoms: dihedral (e.g., "1 2 3 4 -180 180 5")
    
    Output:
        List of DOF dictionaries, each containing:
          - 'atoms': list of atom indices involved
          - 'type': 'bond', 'angle', or 'dihedral'
          - 'initial': starting value
          - 'final': ending value
          - 'stepsize': step increment
          - 'moving_atoms': list of atoms to move for this DOF
    
    Example input:
        # First DOF (bond)
        1 2 -10 10 0.01
        # moving atoms
        2
        # Second DOF (angle)
        1 2 3 -5 5 0.1
        # moving atoms
        3
    
    Example output:
        [
            {'atoms': [1, 2], 'type': 'bond', 'initial': -10.0, 'final': 10.0, 
             'stepsize': 0.01, 'moving_atoms': [2]},
            {'atoms': [1, 2, 3], 'type': 'angle', 'initial': -5.0, 'final': 5.0, 
             'stepsize': 0.1, 'moving_atoms': [3]}
        ]
    '''
    with open(file, "r") as f:
        raw_lines = [ln.strip() for ln in f]

    lines = []
    for ln in raw_lines:
        if not ln or ln.lstrip().startswith("#"):
            continue
        lines.append(ln)

    if len(lines) % 2 != 0:
        raise ValueError(f"Invalid scan file '{file}': DOF lines must be paired with moving atom lines")

    dofs = []
    for i in range(0, len(lines), 2):
        dof_line = lines[i]
        moving_line = lines[i+1]

        # Parse DOF line
        parts = dof_line.split()
        if len(parts) < 5:
            raise ValueError(f"Invalid DOF line in '{file}': {dof_line}")

        idx_tokens = parts[:-3]
        if len(idx_tokens) not in (2, 3, 4):
            raise ValueError(f"DOF line must have 2, 3, or 4 atom indices: {dof_line}")

        try:
            indices = [int(tok) for tok in idx_tokens]
        except ValueError as e:
            raise ValueError(f"Invalid atom index in DOF line '{dof_line}'") from e

        try:
            initial = float(parts[-3])
            final = float(parts[-2])
            stepsize = float(parts[-1])
        except ValueError as e:
            raise ValueError(f"Invalid numeric value in DOF line '{dof_line}'") from e

        if stepsize <= 0:
            raise ValueError(f"Step size must be positive in DOF line '{dof_line}'")

        if final <= initial:
            raise ValueError(f"Final value must be greater than initial in DOF line '{dof_line}'")

        dof_type = "bond" if len(indices) == 2 else "angle" if len(indices) == 3 else "dihedral"

        # Parse moving atoms line
        moving_parts = moving_line.split()
        if not moving_parts:
            raise ValueError(f"Invalid moving atoms line in '{file}': {moving_line}")

        try:
            moving_atoms = [int(tok) for tok in moving_parts]
        except ValueError as e:
            raise ValueError(f"Invalid moving atom index in line '{moving_line}'") from e

        dofs.append({
            "atoms": indices,
            "type": dof_type,
            "initial": initial,
            "final": final,
            "stepsize": stepsize,
            "moving_atoms": moving_atoms,
        })

    return dofs

# %%
def calc_Bond(struc, dof):
    atoms = dof['atoms']
    moving = dof['moving_atoms']
    
    # Change distance between atoms[0] and atoms[1], move atoms[1]
    i, j = atoms[0]-1, atoms[1]-1
    vec = np.array(struc[j][1:]) - np.array(struc[i][1:])
    dist = np.linalg.norm(vec)
    if dist == 0:
        raise ValueError("Bond length is zero")
    unit_vec = vec / dist

    moveUnitVec = [[0.0, 0.0, 0.0] for _ in struc]
    for idx in moving: 
        moveUnitVec[idx-1] = unit_vec.tolist()
    return moveUnitVec

if __name__ == "__main__":
    """Test scan bond function."""
    # --- hardcoded test inputs ---
    struc = [
        ('H', 1.0,  1.414, 0.0),
        ('O', 0.0,  0.0,   0.0),
        ('H', 1.0, -1.414, 0.0),
    ]
    dofs = [
        {'atoms': [1, 2],    'type': 'bond',  'initial': -10.0, 'final': 10.0, 'stepsize': 0.01, 'moving_atoms': [2]}
    ]

    moveUnitVec = calc_Bond(struc, dofs[0])

    print(f"{len(struc)}")
    print(f"Moving unit vector for bond length changing DOF:")
    for (atom, _, _, _), disp in zip(struc, moveUnitVec):
        print(f"{atom} {disp[0]:.6f} {disp[1]:.6f} {disp[2]:.6f}")

# %% 
def calc_AngDihed(struc, dof, idx):
    '''
    scanning for angle/dihedral.

    idx is the step index from 0..nsteps, and value is computed as:
        val = dof['initial'] + idx * dof['stepsize']
    for angle/dihedral core displacement.
    '''
    val = dof['initial'] + (idx) * dof['stepsize'] # unit: degree
    atoms = dof['atoms']
    moving = dof['moving_atoms']
    displacement_vectors = [[0.0, 0.0, 0.0] for _ in struc]

    if dof['type'] == 'angle':
        i, j, k = atoms[0]-1, atoms[1]-1, atoms[2]-1
        pos_i = np.array(struc[i][1:])
        pos_j = np.array(struc[j][1:])
        pos_k = np.array(struc[k][1:])

        vec_ji = pos_i - pos_j
        vec_jk = pos_k - pos_j
        dist_ji = np.linalg.norm(vec_ji)
        dist_jk = np.linalg.norm(vec_jk)
        if dist_ji == 0 or dist_jk == 0:
            raise ValueError("Bond length is zero")

        axis = np.cross(vec_ji, vec_jk)
        axis_norm = np.linalg.norm(axis)
        if axis_norm == 0:
            raise ValueError("Atoms are collinear")
        axis = axis / axis_norm

        delta_rad = np.radians(val)
        K = np.array([[0, -axis[2], axis[1]],
                      [axis[2], 0, -axis[0]],
                      [-axis[1], axis[0], 0]])
        R = np.eye(3) + np.sin(delta_rad) * K + (1 - np.cos(delta_rad)) * (K @ K)

        new_vec_jk = R @ vec_jk
        new_pos_k = pos_j + new_vec_jk
        disp_vec = (new_pos_k - pos_k).tolist()

        for idx_move in moving:
            displacement_vectors[idx_move-1] = disp_vec

    elif dof['type'] == 'dihedral':
        i, j, k, l = atoms[0]-1, atoms[1]-1, atoms[2]-1, atoms[3]-1
        pos_j = np.array(struc[j][1:])
        pos_k = np.array(struc[k][1:])
        pos_l = np.array(struc[l][1:])

        bond_vec = pos_k - pos_j
        bond_norm = np.linalg.norm(bond_vec)
        if bond_norm == 0:
            raise ValueError("Bond length is zero")
        bond_unit = bond_vec / bond_norm

        val_rad = np.radians(val)
        vec_kl = pos_l - pos_k
        proj = np.dot(vec_kl, bond_unit) * bond_unit
        perp = vec_kl - proj

        K = np.array([[0, -bond_unit[2], bond_unit[1]],
                      [bond_unit[2], 0, -bond_unit[0]],
                      [-bond_unit[1], bond_unit[0], 0]])
        R = np.eye(3) + np.sin(val_rad) * K + (1 - np.cos(val_rad)) * (K @ K)

        new_perp = R @ perp
        new_vec_kl = proj + new_perp
        new_pos_l = pos_k + new_vec_kl
        disp_vec = (new_pos_l - pos_l).tolist()

        for idx_move in moving:
            displacement_vectors[idx_move-1] = disp_vec

    else:
        raise ValueError(f"Invalid DOF type for calc_AngDihed: {dof['type']}")

    return displacement_vectors


if __name__ == "__main__":
    """Test scan angle function."""
    # --- hardcoded test inputs ---
    struc = [
        ('H', 1.0,  1.414, 0.0),
        ('O', 0.0,  0.0,   0.0),
        ('H', 1.0, -1.414, 0.0),
    ]
    dofs = [
        {'atoms': [1, 2, 3], 'type': 'angle', 'initial': -10.0, 'final': 10.0, 'stepsize': 2.0, 'moving_atoms': [3]}
    ]

    nsteps = int((dofs[0]['final'] - dofs[0]['initial']) / dofs[0]['stepsize'])
    
    for idx in range(nsteps + 1):
        displacement_vectors = calc_AngDihed(struc, dofs[0], idx)

        print(f"{len(struc)}")
        print(f"Displacement vectors for angle changing DOF at step index {idx}:")
        for (atom, _, _, _), disp in zip(struc, displacement_vectors):
            print(f"{atom} {disp[0]:.6f} {disp[1]:.6f} {disp[2]:.6f}")
    
    
# %%
def scan2D(struc, dofs):
    
    if len(dofs) == 1:
        # 1D scan
        dof = dofs[0]
        nsteps = int((dof['final'] - dof['initial']) / dof['stepsize'])
        scanStruc = []
        fileidx = []

        if dof['type'] == 'bond':
            displacement_vectors = calc_Bond(struc, dof)
            for idx in range(nsteps + 1):
                val = dof['initial'] + idx * dof['stepsize']
                new_struc = []
                fileidx.append(int(dof['initial']) + dof['stepsize'] * idx)
                for i, ((atom, x, y, z), (dx, dy, dz)) in enumerate(zip(struc, displacement_vectors)):
                    new_x = x + val * dx
                    new_y = y + val * dy
                    new_z = z + val * dz
                    new_struc.append((atom, new_x, new_y, new_z))
                scanStruc.append(new_struc)
        else:
            for idx in range(nsteps + 1):
                displacement_vectors = calc_AngDihed(struc, dof, idx)
                new_struc = []
                fileidx.append(int(dof['initial']) + dof['stepsize'] * idx)
                for i, ((atom, x, y, z), (dx, dy, dz)) in enumerate(zip(struc, displacement_vectors)):
                    new_x = x + dx
                    new_y = y + dy
                    new_z = z + dz
                    new_struc.append((atom, new_x, new_y, new_z))
                scanStruc.append(new_struc)
        return fileidx, scanStruc
    
    elif len(dofs) == 2:
        # 2D scan
        dof1, dof2 = dofs[0], dofs[1]
        nsteps1 = int((dof1['final'] - dof1['initial']) / dof1['stepsize'])
        nsteps2 = int((dof2['final'] - dof2['initial']) / dof2['stepsize'])
        scanStruc = []
        fileidx  = []

        # precompute bond unit vector if dof1 is bond (constant across all steps)
        if dof1['type'] == 'bond':
            displacement_vectors1 = calc_Bond(struc, dof1)
        

        for idx1 in range(nsteps1 + 1):
            val1 = dof1['initial'] + idx1 * dof1['stepsize']

            # --- apply dof1 to get intermediate structure ---
            mid_struc = []
            if dof1['type'] == 'bond':
                for (atom, x, y, z), (dx, dy, dz) in zip(struc, displacement_vectors1):
                    mid_struc.append((atom, x + val1*dx, y + val1*dy, z + val1*dz))
            else:
                disp1 = calc_AngDihed(struc, dof1, idx1)
                for (atom, x, y, z), (dx, dy, dz) in zip(struc, disp1):
                    mid_struc.append((atom, x + dx, y + dy, z + dz))

            # precompute bond unit vector if dof2 is bond (constant for this mid_struc)
            if dof2['type'] == 'bond':
                displacement_vectors2 = calc_Bond(mid_struc, dof2)

            for idx2 in range(nsteps2 + 1):
                val2 = dof2['initial'] + idx2 * dof2['stepsize']

                # --- apply dof2 to mid_struc ---
                new_struc = []
                if dof2['type'] == 'bond':
                    for (atom, x, y, z), (dx, dy, dz) in zip(mid_struc, displacement_vectors2):
                        new_struc.append((atom, x + val2*dx, y + val2*dy, z + val2*dz))
                else:
                    disp2 = calc_AngDihed(mid_struc, dof2, idx2)
                    for (atom, x, y, z), (dx, dy, dz) in zip(mid_struc, disp2):
                        new_struc.append((atom, x + dx, y + dy, z + dz))

                scanStruc.append(new_struc)
                fileidx.append(f"{val1:.4f} {val2:.4f}")

        return fileidx, scanStruc
    else:
        raise ValueError("Only 1D and 2D scans supported")

def write_xyz_frames(frames, fileidx, filename):
    """Write a sequence of structures to an XYZ trajectory file."""
    if not frames:
        raise ValueError("No frames to write")

    with open(filename, "w") as f:
        for idx, frame in zip(fileidx, frames):
            f.write(f"{len(frame)}\n")
            f.write(f"{idx}\n")
            for atom in frame:
                if len(atom) != 4:
                    raise ValueError("Invalid atom tuple in frame")
                sym, x, y, z = atom
                f.write(f"{sym} {x:.8f} {y:.8f} {z:.8f}\n")


if __name__ == "__main__":
    """Test 1D scan function."""
    # --- hardcoded test inputs ---
    struc = [
        ('H', 1.0,  1.414, 0.0),
        ('O', 0.0,  0.0,   0.0),
        ('H', 1.0, -1.414, 0.0),
    ]
    # dofs = [
    #     {'atoms': [1, 2],    'type': 'bond',  'initial': -1.0, 'final': 1.0, 'stepsize': 0.2, 'moving_atoms': [2]}
    # ]
    dofs = [
        {'atoms': [1, 2, 3], 'type': 'angle', 'initial': -10.0, 'final': 10.0, 'stepsize': 2, 'moving_atoms': [3]}
    ]

    fileidx, scanStruc = scan2D(struc, dofs)

    print(f"Generated {len(scanStruc)} structures :")
    for idx, s in enumerate(scanStruc):
        print(len(struc))
        print(fileidx[idx])
        for atom in s:
            print(atom)
        print()

    # output_file = "scantest.xyz"
    # write_xyz_frames(scanStruc, fileidx, output_file)
    # print(f"Wrote {len(scanStruc)} frames to '{output_file}'")


if __name__ == "__main__":
    """Test 2D scan function."""
    # --- hardcoded test inputs ---
    struc = [
        ('H', 1.0,  1.414, 0.0),
        ('O', 0.0,  0.0,   0.0),
        ('H', 1.0, -1.414, 0.0),
    ]
    dofs = [
        {'atoms': [1, 2],    'type': 'bond',  'initial': -1.0, 'final': 1.0, 'stepsize': 0.5, 'moving_atoms': [2]},
        {'atoms': [1, 2, 3], 'type': 'angle', 'initial': -10.0, 'final': 10.0, 'stepsize': 5.0, 'moving_atoms': [3]},
    ]

    fileidx, scanStruc = scan2D(struc, dofs)

    nsteps1 = int((dofs[0]['final'] - dofs[0]['initial']) / dofs[0]['stepsize'])
    nsteps2 = int((dofs[1]['final'] - dofs[1]['initial']) / dofs[1]['stepsize'])
    print(f"Generated {len(scanStruc)} structures ({nsteps1+1} x {nsteps2+1} grid):")
    for idx, s in zip(fileidx, scanStruc):
        print(f"\n--- bond={idx.split()[0]} Å  angle={idx.split()[1]} deg ---")
        print(len(struc))
        print(idx)
        for atom in s:
            sym, x, y, z = atom
            print(f"{sym} {x:.6f} {y:.6f} {z:.6f}")
    
    output_file = "scantest.xyz"
    write_xyz_frames(scanStruc, fileidx, output_file)
    print(f"Wrote {len(scanStruc)} frames to '{output_file}'")


# %%
if __name__ == "__main__":
    if len(sys.argv) >= 3:
        xyz_file  = sys.argv[1]
        scan_file = sys.argv[2]
        try:
            struc = read1file(xyz_file)
            print(f"Read {len(struc)} atoms from {xyz_file}:")
            for atom in struc:
                print(atom)
        except Exception as e:
            print(f"Error reading file '{xyz_file}': {e}")
            sys.exit(1)

        try:
            dofs = read2file(scan_file)
            print(f"Read {len(dofs)} DOFs from {scan_file}:")
            for dof in dofs:
                print(dof)
        except Exception as e:
            print(f"Error reading file '{scan_file}': {e}")
            sys.exit(1)

        try:
            if len(dofs) == 1:
                fileidx, scanStruc = scan2D(struc, dofs)
                output_file = "scan1D.xyz"
            elif len(dofs) == 2:
                fileidx, scanStruc = scan2D(struc, dofs)
                output_file = "scan2D.xyz"
            else:
                raise ValueError(f"Expected 1 or 2 DOFs, got {len(dofs)}")

            write_xyz_frames(scanStruc, fileidx, output_file)
            print(f"Wrote {len(scanStruc)} frames to '{output_file}'")
        except Exception as e:
            print(f"Error during scan: {e}")
            sys.exit(1)

    else:
        print("Usage: python Vscan2D.py <structure.xyz> <scan.dat>")
        sys.exit(1)

# %%
