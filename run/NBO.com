%chk=NBO.chk
#p B3LYP/6-311+G(d,p) OPT Pop=(Full,NBORead,SaveNBOs)

Geometry optimization + ground-state NBO analysis

0 1
 C   0.000000   0.000000   0.000000
 H   0.000000   0.000000   1.089000
 H   1.026719   0.000000  -0.363000
 H  -0.513360   0.889165  -0.363000
 H  -0.513360  -0.889165  -0.363000

$NBO BNDIDX RESONANCE NBO PLOT $END

