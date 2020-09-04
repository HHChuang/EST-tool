#!/usr/bin/env python
#################################################################
# Program:                                                      #
#   Rotate trajectory in order to fit the orientation of        #
#   this 2D-PES                                                 #
#                                                               #
# Input:                                                        #
#       $1 = ts.Traj; transition-state structures of trajectory #
#       $2 = ts.PES; transition-state structures of 2D-PES      #
#       $3 = Traj; structures of single trajectory              #
#                                                               #
# Output:                                                       #
#       1. rot.$3                                               #
#       2. cen.$2                                               #
#                                                               #
# History:                                                      #
# 2018/12/21, Grace                                             #
# 2019/06/04, Grace, modify the structure of this code, and     #
# reserve the comment line from the original trajectories.      #
# 2020/09/04, Grace, add one more output as cen.$1              #
#################################################################

import numpy as np
import re
import sys


def main():
    # 1. optimize rotation matrix via Kabsch algorithm
    # by minimize the RMSD between two similar structures
    R, D = rotM_D()  # output: cen.$1

    # 2. use above rotation matrix to rotate all the
    # structures in one dynamic trajectory
    rot_tran_Traj(R, D)  # output: rot.$3


def get_coord(filename):
    global NAtoms
    global atoms

    f = open(filename, 'r')
    V = list()
    atoms = list()
    NAtoms = int(f.readline())

    f.readline()

    for lines_read, line in enumerate(f):

        if lines_read == NAtoms:
            break

        # atom = re.findall(r'[a-zA-Z]+', line)[0]
        atom = re.findall(r'[0-9]*', line)[0]
        atom = atom.upper()

        numbers = re.findall(r'[-]?\d+\.\d*(?:[Ee][-\+]\d+)?', line)
        numbers = [float(number) for number in numbers]

        if len(numbers) == 3:
            V.append(np.array(numbers))
            atoms.append(atom)
        else:
            exit("Problem with reading input files".format(
                lines_read + 2))

    f.close()
    atoms = np.array(atoms)
    V = np.array(V)
    return atoms, V


def rmsd(A, B):
    Coord = len(A[0])
    NAtom = len(A)
    cum = 0.0
    for i in range(NAtom):
        for j in range(Coord):
            cum += (A[i][j] - B[i][j])**2.0
    return np.sqrt(cum / NAtom)


def centroid(A):
    A = A.mean(axis=0)
    return A


def kabsch(coord_var, coord_ref):

    # covariance matrix
    covar = np.dot(coord_var.T, coord_ref)

    # SVD  http://en.wikipedia.org/wiki/Kabsch_algorithm
    v, s, wt = np.linalg.svd(covar)

    # Transposition of v,wt
    d = (np.linalg.det(v) * np.linalg.det(wt)) < 0.0
    # right-hand coord
    if d:
        s[-1] = -s[-1]
        v[:, -1] = -v[:, -1]

    # Create Rotation matrix R
    R = np.dot(v, wt)

    return R


def rotM_D():
    # Input 2 molecular structure; argv1 (var) and argv2 (ref)
    coord_var_atoms, coord_var = get_coord(str(sys.argv[1]))
    coord_ref_atoms, coord_ref = get_coord(str(sys.argv[2]))
    rmsd1 = rmsd(coord_var, coord_ref)
    # Centroid
    coord_var_cen = coord_var - centroid(coord_var)
    coord_ref_cen = coord_ref - centroid(coord_ref)
    # Distance between two centroids
    D = centroid(coord_var) - centroid(coord_ref)
    # print(D)

    R = kabsch(coord_var_cen, coord_ref_cen)
    coord_var_cen = np.dot(coord_var_cen, R)
    rmsd2 = rmsd(coord_var_cen, coord_ref_cen)

    print('RMSD before/after rotation and translation (angstrom): ')
    print("% 7.4f % 7.4f" % (rmsd1, rmsd2))

    # print(R)
    # frag = str(sys.argv[1]).split('.')
    # np.savetxt('.'.join(['rotM', frag[3]]), R, fmt='%10.6f')

    # extract the reference structure with respect to its centroid; output: cen.$2
    filename = 'cen.' + str(sys.argv[2]).split('/')[-1]
    file_coord_ref_cen = open(filename, 'w')
    file_coord_ref_cen.write(str(NAtoms) + '\n')  # NAtoms
    file_coord_ref_cen.write(filename + '\n')
    for i in range(NAtoms):
        line = str(atoms[i]) +\
            ' ' + str('%f' % (coord_ref_cen[i][0])) +\
            ' ' + str('%f' % (coord_ref_cen[i][1])) +\
            ' ' + str('%f' % (coord_ref_cen[i][2])) + '\n'
        file_coord_ref_cen.write(line)

    file_coord_ref_cen.close()

    return R, D


def get_traj(allTraj, n1, n2):
    coord = list()
    atom = list()
    # slicing array
    comment = str(allTraj[n1 - 1])
    tmp = allTraj[n1:n2]

    for i in range(NAtoms):
        # may have bug, change '\t' to ' '
        atom.append(tmp[i].split(' ')[0])
        coord.append(tmp[i].split(' ')[1:4])
    atom = np.array(atom)
    atom = atom.astype(int)
    coord = np.array(coord)
    coord = coord.astype(np.float)
    return comment, atom, coord


def totline(filename):
    with open(filename) as f:
        for i, l in enumerate(f):
            pass
    return i + 1


def rot_tran_Traj(R, D):
    # Set up variables, njobs
    tline = totline(str(sys.argv[3]))
    f = open(str(sys.argv[3]), 'r')
    NAtoms = int(f.readline())
    allTraj = f.read().splitlines()
    njobs = tline / (NAtoms + 2)
    f.close()

    # Save structure in rot.$3
    filename = 'rot.' + str(sys.argv[3]).split('/')[-1]
    traj = open(filename, 'w')

    # Extract structures, and then rotate by R
    for i in range(int(njobs)):
        # for i in range(1):  # testing
        n1 = i * (NAtoms + 2) + 1
        n2 = (i + 1) * (NAtoms + 2) - 1
        comment, atom, coord = get_traj(allTraj, n1, n2)

        # translation
        coord = coord - D

        # rotation
        coord = np.dot(coord, R)

        # export the rotated trajectory
        traj.write(str(NAtoms) + '\n')
        traj.write(str(comment) + '\n')

        for j in range(NAtoms):
            line = str(atom[j]) +\
                ' ' + str('%f' % (coord[j][0])) +\
                ' ' + str('%f' % (coord[j][1])) +\
                ' ' + str('%f' % (coord[j][2])) + '\n'
            traj.write(line)

    traj.close()


main()
