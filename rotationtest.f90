program rtest
 
use rotations3D
 
IMPLICIT NONE
 
real(kind=dbl)        :: ieu(3), oeu(3), iro(4), oro(4), iho(3), oho(3), icu(3), ocu(3)
real(kind=dbl)        :: iax(4), oax(4), iqu(4), oqu(4), ivec(3), ovec(3)
real(kind=dbl)        :: iom(3,3), oom(3,3), omm(3,3), diff, diffmax, dtor, aux
real(kind=dbl),allocatable :: rots(:,:)
integer(kind=irg)     :: tcnt, rcnt, i
type(orientationtyped):: ot
logical               :: verbose
 
ivec = (/ 1.D0, 1.D0, 0.D0 /)
verbose = .FALSE.
dtor = cPi/180.D0
open(unit=20,file='rotations.txt',status='old')
read(20,"(I5)") rcnt
if (verbose) write (*,*) 'Number of rotations in file = ',rcnt
allocate(rots(3,rcnt))
do i=1,rcnt
  read(20,"(3F9.3)") rots(1:3,i)
if (verbose)  write (*,*) rots(1:3,i)
end do
close(unit=20,status='keep')
do i=1,rcnt
! create the orientation type for a given Euler triplet
 ieu = rots(1:3,i) * dtor
 ot = init_orientation(ieu, 'eu')
 write(*,*) '  '
 write(*,*) '----------------------------------------------------'
 call print_orientation(ot)
! passive/active rotations
if (verbose) write (*,*) 'active/passive test on vector ',ivec
  iom = ot%om
  ovec = RotateVector(ivec,iom,'a')
if (verbose)   write (*,*) '  active, rotation matrix ',ovec
  ovec = RotateVector(ivec,iom,'p')
if (verbose)   write (*,*) '  passive, rotation matrix ',ovec
  iqu = ot%quat
  ovec = RotateVector(ivec,iqu,'a')
if (verbose)   write (*,*) '  active, quaternion ',ovec
  ovec = RotateVector(ivec,iqu,'p')
if (verbose)   write (*,*) '  passive, quaternion ',ovec
if (verbose)  write (*,*) ' '
 
! individual tests  x = Oinv [ O [x] ]
 
  diffmax = 0.D0
tcnt = 0
if (verbose)  write (*,*) 'eu test'
  ieu = ot%eulang
  oeu = om2eu(eu2om(ieu))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)   write (*,*) 'eu2om max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  ieu = ot%eulang
  oeu = ax2eu(eu2ax(ieu))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)   write (*,*) 'eu2ax max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  ieu = ot%eulang
  oeu = ro2eu(eu2ro(ieu))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)   write (*,*) 'eu2ro max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  ieu = ot%eulang
  oeu = qu2eu(eu2qu(ieu))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)   write (*,*) 'eu2qu max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  ieu = ot%eulang
  oeu = ho2eu(eu2ho(ieu))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)   write (*,*) 'eu2ho max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  ieu = ot%eulang
  oeu = cu2eu(eu2cu(ieu))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)   write (*,*) 'eu2cu max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
 
if (verbose)  write (*,*) 'om test'
  iom = ot%om
  oom = eu2om(om2eu(iom))
  diff = maxval(abs(oom-iom))
if (verbose)   write (*,*) 'om2eu max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iom = ot%om
  oom = ax2om(om2ax(iom))
  diff = maxval(abs(oom-iom))
if (verbose)   write (*,*) 'om2ax max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iom = ot%om
  oom = ro2om(om2ro(iom))
  diff = maxval(abs(oom-iom))
if (verbose)   write (*,*) 'om2ro max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iom = ot%om
  oom = qu2om(om2qu(iom))
  diff = maxval(abs(oom-iom))
if (verbose)   write (*,*) 'om2qu max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iom = ot%om
  oom = ho2om(om2ho(iom))
  diff = maxval(abs(oom-iom))
if (verbose)   write (*,*) 'om2ho max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iom = ot%om
  oom = cu2om(om2cu(iom))
  diff = maxval(abs(oom-iom))
if (verbose)   write (*,*) 'om2cu max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
 
if (verbose)  write (*,*) 'ax test'
  iax = ot%axang
  oax = eu2ax(ax2eu(iax))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)   write (*,*) 'ax2eu max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iax = ot%axang
  oax = om2ax(ax2om(iax))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)   write (*,*) 'ax2om max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iax = ot%axang
  oax = ro2ax(ax2ro(iax))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)   write (*,*) 'ax2ro max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iax = ot%axang
  oax = qu2ax(ax2qu(iax))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)   write (*,*) 'ax2qu max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iax = ot%axang
  oax = ho2ax(ax2ho(iax))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)   write (*,*) 'ax2ho max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iax = ot%axang
  oax = cu2ax(ax2cu(iax))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)   write (*,*) 'ax2cu max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
 
if (verbose)  write (*,*) 'ro test'
  iro = ot%rodrigues
  oro = eu2ro(ro2eu(iro))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)   write (*,*) 'ro2eu max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iro = ot%rodrigues
  oro = om2ro(ro2om(iro))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)   write (*,*) 'ro2om max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iro = ot%rodrigues
  oro = ax2ro(ro2ax(iro))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)   write (*,*) 'ro2ax max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iro = ot%rodrigues
  oro = qu2ro(ro2qu(iro))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)   write (*,*) 'ro2qu max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iro = ot%rodrigues
  oro = ho2ro(ro2ho(iro))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)   write (*,*) 'ro2ho max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iro = ot%rodrigues
  oro = cu2ro(ro2cu(iro))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)   write (*,*) 'ro2cu max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
 
if (verbose)  write (*,*) 'qu test'
  iqu = ot%quat
  oqu = eu2qu(qu2eu(iqu))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)   write (*,*) 'qu2eu max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iqu = ot%quat
  oqu = om2qu(qu2om(iqu))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)   write (*,*) 'qu2om max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iqu = ot%quat
  oqu = ax2qu(qu2ax(iqu))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)   write (*,*) 'qu2ax max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iqu = ot%quat
  oqu = ro2qu(qu2ro(iqu))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)   write (*,*) 'qu2ro max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iqu = ot%quat
  oqu = ho2qu(qu2ho(iqu))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)   write (*,*) 'qu2ho max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iqu = ot%quat
  oqu = cu2qu(qu2cu(iqu))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)   write (*,*) 'qu2cu max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
 
if (verbose)  write (*,*) 'ho test'
  iho = ot%homochoric
  oho = eu2ho(ho2eu(iho))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)   write (*,*) 'ho2eu max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iho = ot%homochoric
  oho = om2ho(ho2om(iho))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)   write (*,*) 'ho2om max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iho = ot%homochoric
  oho = ax2ho(ho2ax(iho))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)   write (*,*) 'ho2ax max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iho = ot%homochoric
  oho = ro2ho(ho2ro(iho))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)   write (*,*) 'ho2ro max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iho = ot%homochoric
  oho = qu2ho(ho2qu(iho))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)   write (*,*) 'ho2qu max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iho = ot%homochoric
  oho = cu2ho(ho2cu(iho))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)   write (*,*) 'ho2cu max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
 
if (verbose)  write (*,*) 'cu test'
  icu = ot%cubochoric
  ocu = eu2cu(cu2eu(icu))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)   write (*,*) 'cu2eu max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  icu = ot%cubochoric
  ocu = om2cu(cu2om(icu))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)   write (*,*) 'cu2om max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  icu = ot%cubochoric
  ocu = ax2cu(cu2ax(icu))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)   write (*,*) 'cu2ax max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  icu = ot%cubochoric
  ocu = ro2cu(cu2ro(icu))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)   write (*,*) 'cu2ro max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  icu = ot%cubochoric
  ocu = qu2cu(cu2qu(icu))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)   write (*,*) 'cu2qu max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  icu = ot%cubochoric
  ocu = ho2cu(cu2ho(icu))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)   write (*,*) 'cu2ho max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
 
 write (*,*) ' Maximum difference in pairwise tests  : ', diffmax
 
 
! triple tests 
 
  diffmax = 0.D0
  tcnt = 0
if (verbose) write (*,*) 'triple eu test'
  ieu = ot%eulang
  oeu = ax2eu(om2ax(eu2om(ieu)))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)  write (*,*) 'ax2eu-om2ax-eu2om max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  ieu = ot%eulang
  oeu = ro2eu(om2ro(eu2om(ieu)))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)  write (*,*) 'ro2eu-om2ro-eu2om max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  ieu = ot%eulang
  oeu = qu2eu(om2qu(eu2om(ieu)))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)  write (*,*) 'qu2eu-om2qu-eu2om max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  ieu = ot%eulang
  oeu = ho2eu(om2ho(eu2om(ieu)))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)  write (*,*) 'ho2eu-om2ho-eu2om max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  ieu = ot%eulang
  oeu = cu2eu(om2cu(eu2om(ieu)))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)  write (*,*) 'cu2eu-om2cu-eu2om max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  ieu = ot%eulang
  oeu = om2eu(ax2om(eu2ax(ieu)))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)  write (*,*) 'om2eu-ax2om-eu2ax max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  ieu = ot%eulang
  oeu = ro2eu(ax2ro(eu2ax(ieu)))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)  write (*,*) 'ro2eu-ax2ro-eu2ax max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  ieu = ot%eulang
  oeu = qu2eu(ax2qu(eu2ax(ieu)))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)  write (*,*) 'qu2eu-ax2qu-eu2ax max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  ieu = ot%eulang
  oeu = ho2eu(ax2ho(eu2ax(ieu)))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)  write (*,*) 'ho2eu-ax2ho-eu2ax max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  ieu = ot%eulang
  oeu = cu2eu(ax2cu(eu2ax(ieu)))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)  write (*,*) 'cu2eu-ax2cu-eu2ax max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  ieu = ot%eulang
  oeu = om2eu(ro2om(eu2ro(ieu)))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)  write (*,*) 'om2eu-ro2om-eu2ro max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  ieu = ot%eulang
  oeu = ax2eu(ro2ax(eu2ro(ieu)))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)  write (*,*) 'ax2eu-ro2ax-eu2ro max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  ieu = ot%eulang
  oeu = qu2eu(ro2qu(eu2ro(ieu)))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)  write (*,*) 'qu2eu-ro2qu-eu2ro max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  ieu = ot%eulang
  oeu = ho2eu(ro2ho(eu2ro(ieu)))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)  write (*,*) 'ho2eu-ro2ho-eu2ro max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  ieu = ot%eulang
  oeu = cu2eu(ro2cu(eu2ro(ieu)))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)  write (*,*) 'cu2eu-ro2cu-eu2ro max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  ieu = ot%eulang
  oeu = om2eu(qu2om(eu2qu(ieu)))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)  write (*,*) 'om2eu-qu2om-eu2qu max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  ieu = ot%eulang
  oeu = ax2eu(qu2ax(eu2qu(ieu)))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)  write (*,*) 'ax2eu-qu2ax-eu2qu max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  ieu = ot%eulang
  oeu = ro2eu(qu2ro(eu2qu(ieu)))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)  write (*,*) 'ro2eu-qu2ro-eu2qu max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  ieu = ot%eulang
  oeu = ho2eu(qu2ho(eu2qu(ieu)))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)  write (*,*) 'ho2eu-qu2ho-eu2qu max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  ieu = ot%eulang
  oeu = cu2eu(qu2cu(eu2qu(ieu)))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)  write (*,*) 'cu2eu-qu2cu-eu2qu max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  ieu = ot%eulang
  oeu = om2eu(ho2om(eu2ho(ieu)))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)  write (*,*) 'om2eu-ho2om-eu2ho max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  ieu = ot%eulang
  oeu = ax2eu(ho2ax(eu2ho(ieu)))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)  write (*,*) 'ax2eu-ho2ax-eu2ho max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  ieu = ot%eulang
  oeu = ro2eu(ho2ro(eu2ho(ieu)))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)  write (*,*) 'ro2eu-ho2ro-eu2ho max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  ieu = ot%eulang
  oeu = qu2eu(ho2qu(eu2ho(ieu)))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)  write (*,*) 'qu2eu-ho2qu-eu2ho max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  ieu = ot%eulang
  oeu = cu2eu(ho2cu(eu2ho(ieu)))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)  write (*,*) 'cu2eu-ho2cu-eu2ho max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  ieu = ot%eulang
  oeu = om2eu(cu2om(eu2cu(ieu)))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)  write (*,*) 'om2eu-cu2om-eu2cu max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  ieu = ot%eulang
  oeu = ax2eu(cu2ax(eu2cu(ieu)))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)  write (*,*) 'ax2eu-cu2ax-eu2cu max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  ieu = ot%eulang
  oeu = ro2eu(cu2ro(eu2cu(ieu)))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)  write (*,*) 'ro2eu-cu2ro-eu2cu max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  ieu = ot%eulang
  oeu = qu2eu(cu2qu(eu2cu(ieu)))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)  write (*,*) 'qu2eu-cu2qu-eu2cu max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  ieu = ot%eulang
  oeu = ho2eu(cu2ho(eu2cu(ieu)))
  omm = eu2om(oeu)
  diff = maxval(abs(omm-ot%om))
if (verbose)  write (*,*) 'ho2eu-cu2ho-eu2cu max om difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
 
if (verbose) write (*,*) 'triple om test'
  iom = ot%om
  oom = ax2om(eu2ax(om2eu(iom)))
  diff = maxval(abs(oom-iom))
if (verbose)  write (*,*) 'ax2om-eu2ax-om2eu max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iom = ot%om
  oom = ro2om(eu2ro(om2eu(iom)))
  diff = maxval(abs(oom-iom))
if (verbose)  write (*,*) 'ro2om-eu2ro-om2eu max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iom = ot%om
  oom = qu2om(eu2qu(om2eu(iom)))
  diff = maxval(abs(oom-iom))
if (verbose)  write (*,*) 'qu2om-eu2qu-om2eu max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iom = ot%om
  oom = ho2om(eu2ho(om2eu(iom)))
  diff = maxval(abs(oom-iom))
if (verbose)  write (*,*) 'ho2om-eu2ho-om2eu max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iom = ot%om
  oom = cu2om(eu2cu(om2eu(iom)))
  diff = maxval(abs(oom-iom))
if (verbose)  write (*,*) 'cu2om-eu2cu-om2eu max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iom = ot%om
  oom = eu2om(ax2eu(om2ax(iom)))
  diff = maxval(abs(oom-iom))
if (verbose)  write (*,*) 'eu2om-ax2eu-om2ax max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iom = ot%om
  oom = ro2om(ax2ro(om2ax(iom)))
  diff = maxval(abs(oom-iom))
if (verbose)  write (*,*) 'ro2om-ax2ro-om2ax max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iom = ot%om
  oom = qu2om(ax2qu(om2ax(iom)))
  diff = maxval(abs(oom-iom))
if (verbose)  write (*,*) 'qu2om-ax2qu-om2ax max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iom = ot%om
  oom = ho2om(ax2ho(om2ax(iom)))
  diff = maxval(abs(oom-iom))
if (verbose)  write (*,*) 'ho2om-ax2ho-om2ax max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iom = ot%om
  oom = cu2om(ax2cu(om2ax(iom)))
  diff = maxval(abs(oom-iom))
if (verbose)  write (*,*) 'cu2om-ax2cu-om2ax max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iom = ot%om
  oom = eu2om(ro2eu(om2ro(iom)))
  diff = maxval(abs(oom-iom))
if (verbose)  write (*,*) 'eu2om-ro2eu-om2ro max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iom = ot%om
  oom = ax2om(ro2ax(om2ro(iom)))
  diff = maxval(abs(oom-iom))
if (verbose)  write (*,*) 'ax2om-ro2ax-om2ro max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iom = ot%om
  oom = qu2om(ro2qu(om2ro(iom)))
  diff = maxval(abs(oom-iom))
if (verbose)  write (*,*) 'qu2om-ro2qu-om2ro max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iom = ot%om
  oom = ho2om(ro2ho(om2ro(iom)))
  diff = maxval(abs(oom-iom))
if (verbose)  write (*,*) 'ho2om-ro2ho-om2ro max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iom = ot%om
  oom = cu2om(ro2cu(om2ro(iom)))
  diff = maxval(abs(oom-iom))
if (verbose)  write (*,*) 'cu2om-ro2cu-om2ro max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iom = ot%om
  oom = eu2om(qu2eu(om2qu(iom)))
  diff = maxval(abs(oom-iom))
if (verbose)  write (*,*) 'eu2om-qu2eu-om2qu max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iom = ot%om
  oom = ax2om(qu2ax(om2qu(iom)))
  diff = maxval(abs(oom-iom))
if (verbose)  write (*,*) 'ax2om-qu2ax-om2qu max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iom = ot%om
  oom = ro2om(qu2ro(om2qu(iom)))
  diff = maxval(abs(oom-iom))
if (verbose)  write (*,*) 'ro2om-qu2ro-om2qu max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iom = ot%om
  oom = ho2om(qu2ho(om2qu(iom)))
  diff = maxval(abs(oom-iom))
if (verbose)  write (*,*) 'ho2om-qu2ho-om2qu max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iom = ot%om
  oom = cu2om(qu2cu(om2qu(iom)))
  diff = maxval(abs(oom-iom))
if (verbose)  write (*,*) 'cu2om-qu2cu-om2qu max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iom = ot%om
  oom = eu2om(ho2eu(om2ho(iom)))
  diff = maxval(abs(oom-iom))
if (verbose)  write (*,*) 'eu2om-ho2eu-om2ho max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iom = ot%om
  oom = ax2om(ho2ax(om2ho(iom)))
  diff = maxval(abs(oom-iom))
if (verbose)  write (*,*) 'ax2om-ho2ax-om2ho max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iom = ot%om
  oom = ro2om(ho2ro(om2ho(iom)))
  diff = maxval(abs(oom-iom))
if (verbose)  write (*,*) 'ro2om-ho2ro-om2ho max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iom = ot%om
  oom = qu2om(ho2qu(om2ho(iom)))
  diff = maxval(abs(oom-iom))
if (verbose)  write (*,*) 'qu2om-ho2qu-om2ho max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iom = ot%om
  oom = cu2om(ho2cu(om2ho(iom)))
  diff = maxval(abs(oom-iom))
if (verbose)  write (*,*) 'cu2om-ho2cu-om2ho max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iom = ot%om
  oom = eu2om(cu2eu(om2cu(iom)))
  diff = maxval(abs(oom-iom))
if (verbose)  write (*,*) 'eu2om-cu2eu-om2cu max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iom = ot%om
  oom = ax2om(cu2ax(om2cu(iom)))
  diff = maxval(abs(oom-iom))
if (verbose)  write (*,*) 'ax2om-cu2ax-om2cu max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iom = ot%om
  oom = ro2om(cu2ro(om2cu(iom)))
  diff = maxval(abs(oom-iom))
if (verbose)  write (*,*) 'ro2om-cu2ro-om2cu max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iom = ot%om
  oom = qu2om(cu2qu(om2cu(iom)))
  diff = maxval(abs(oom-iom))
if (verbose)  write (*,*) 'qu2om-cu2qu-om2cu max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iom = ot%om
  oom = ho2om(cu2ho(om2cu(iom)))
  diff = maxval(abs(oom-iom))
if (verbose)  write (*,*) 'ho2om-cu2ho-om2cu max difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
 
if (verbose) write (*,*) 'triple ax test'
  iax = ot%axang
  oax = om2ax(eu2om(ax2eu(iax)))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)  write (*,*) 'om2ax-eu2om-ax2eu max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iax = ot%axang
  oax = ro2ax(eu2ro(ax2eu(iax)))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)  write (*,*) 'ro2ax-eu2ro-ax2eu max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iax = ot%axang
  oax = qu2ax(eu2qu(ax2eu(iax)))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)  write (*,*) 'qu2ax-eu2qu-ax2eu max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iax = ot%axang
  oax = ho2ax(eu2ho(ax2eu(iax)))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)  write (*,*) 'ho2ax-eu2ho-ax2eu max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iax = ot%axang
  oax = cu2ax(eu2cu(ax2eu(iax)))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)  write (*,*) 'cu2ax-eu2cu-ax2eu max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iax = ot%axang
  oax = eu2ax(om2eu(ax2om(iax)))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)  write (*,*) 'eu2ax-om2eu-ax2om max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iax = ot%axang
  oax = ro2ax(om2ro(ax2om(iax)))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)  write (*,*) 'ro2ax-om2ro-ax2om max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iax = ot%axang
  oax = qu2ax(om2qu(ax2om(iax)))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)  write (*,*) 'qu2ax-om2qu-ax2om max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iax = ot%axang
  oax = ho2ax(om2ho(ax2om(iax)))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)  write (*,*) 'ho2ax-om2ho-ax2om max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iax = ot%axang
  oax = cu2ax(om2cu(ax2om(iax)))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)  write (*,*) 'cu2ax-om2cu-ax2om max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iax = ot%axang
  oax = eu2ax(ro2eu(ax2ro(iax)))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)  write (*,*) 'eu2ax-ro2eu-ax2ro max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iax = ot%axang
  oax = om2ax(ro2om(ax2ro(iax)))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)  write (*,*) 'om2ax-ro2om-ax2ro max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iax = ot%axang
  oax = qu2ax(ro2qu(ax2ro(iax)))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)  write (*,*) 'qu2ax-ro2qu-ax2ro max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iax = ot%axang
  oax = ho2ax(ro2ho(ax2ro(iax)))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)  write (*,*) 'ho2ax-ro2ho-ax2ro max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iax = ot%axang
  oax = cu2ax(ro2cu(ax2ro(iax)))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)  write (*,*) 'cu2ax-ro2cu-ax2ro max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iax = ot%axang
  oax = eu2ax(qu2eu(ax2qu(iax)))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)  write (*,*) 'eu2ax-qu2eu-ax2qu max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iax = ot%axang
  oax = om2ax(qu2om(ax2qu(iax)))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)  write (*,*) 'om2ax-qu2om-ax2qu max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iax = ot%axang
  oax = ro2ax(qu2ro(ax2qu(iax)))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)  write (*,*) 'ro2ax-qu2ro-ax2qu max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iax = ot%axang
  oax = ho2ax(qu2ho(ax2qu(iax)))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)  write (*,*) 'ho2ax-qu2ho-ax2qu max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iax = ot%axang
  oax = cu2ax(qu2cu(ax2qu(iax)))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)  write (*,*) 'cu2ax-qu2cu-ax2qu max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iax = ot%axang
  oax = eu2ax(ho2eu(ax2ho(iax)))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)  write (*,*) 'eu2ax-ho2eu-ax2ho max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iax = ot%axang
  oax = om2ax(ho2om(ax2ho(iax)))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)  write (*,*) 'om2ax-ho2om-ax2ho max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iax = ot%axang
  oax = ro2ax(ho2ro(ax2ho(iax)))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)  write (*,*) 'ro2ax-ho2ro-ax2ho max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iax = ot%axang
  oax = qu2ax(ho2qu(ax2ho(iax)))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)  write (*,*) 'qu2ax-ho2qu-ax2ho max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iax = ot%axang
  oax = cu2ax(ho2cu(ax2ho(iax)))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)  write (*,*) 'cu2ax-ho2cu-ax2ho max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iax = ot%axang
  oax = eu2ax(cu2eu(ax2cu(iax)))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)  write (*,*) 'eu2ax-cu2eu-ax2cu max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iax = ot%axang
  oax = om2ax(cu2om(ax2cu(iax)))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)  write (*,*) 'om2ax-cu2om-ax2cu max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iax = ot%axang
  oax = ro2ax(cu2ro(ax2cu(iax)))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)  write (*,*) 'ro2ax-cu2ro-ax2cu max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iax = ot%axang
  oax = qu2ax(cu2qu(ax2cu(iax)))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)  write (*,*) 'qu2ax-cu2qu-ax2cu max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iax = ot%axang
  oax = ho2ax(cu2ho(ax2cu(iax)))
  if ((oax(4)-cPi).lt.1.D-08) then
    diff = maxval(abs(oax(1:3))-abs(ot%axang(1:3)))
  else
    diff = maxval(abs(oax-ot%axang))
  end if 
if (verbose)  write (*,*) 'ho2ax-cu2ho-ax2cu max ax difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
 
if (verbose) write (*,*) 'triple ro test'
  iro = ot%rodrigues
  oro = om2ro(eu2om(ro2eu(iro)))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)  write (*,*) 'om2ro-eu2om-ro2eu max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iro = ot%rodrigues
  oro = ax2ro(eu2ax(ro2eu(iro)))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)  write (*,*) 'ax2ro-eu2ax-ro2eu max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iro = ot%rodrigues
  oro = qu2ro(eu2qu(ro2eu(iro)))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)  write (*,*) 'qu2ro-eu2qu-ro2eu max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iro = ot%rodrigues
  oro = ho2ro(eu2ho(ro2eu(iro)))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)  write (*,*) 'ho2ro-eu2ho-ro2eu max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iro = ot%rodrigues
  oro = cu2ro(eu2cu(ro2eu(iro)))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)  write (*,*) 'cu2ro-eu2cu-ro2eu max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iro = ot%rodrigues
  oro = eu2ro(om2eu(ro2om(iro)))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)  write (*,*) 'eu2ro-om2eu-ro2om max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iro = ot%rodrigues
  oro = ax2ro(om2ax(ro2om(iro)))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)  write (*,*) 'ax2ro-om2ax-ro2om max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iro = ot%rodrigues
  oro = qu2ro(om2qu(ro2om(iro)))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)  write (*,*) 'qu2ro-om2qu-ro2om max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iro = ot%rodrigues
  oro = ho2ro(om2ho(ro2om(iro)))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)  write (*,*) 'ho2ro-om2ho-ro2om max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iro = ot%rodrigues
  oro = cu2ro(om2cu(ro2om(iro)))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)  write (*,*) 'cu2ro-om2cu-ro2om max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iro = ot%rodrigues
  oro = eu2ro(ax2eu(ro2ax(iro)))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)  write (*,*) 'eu2ro-ax2eu-ro2ax max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iro = ot%rodrigues
  oro = om2ro(ax2om(ro2ax(iro)))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)  write (*,*) 'om2ro-ax2om-ro2ax max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iro = ot%rodrigues
  oro = qu2ro(ax2qu(ro2ax(iro)))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)  write (*,*) 'qu2ro-ax2qu-ro2ax max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iro = ot%rodrigues
  oro = ho2ro(ax2ho(ro2ax(iro)))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)  write (*,*) 'ho2ro-ax2ho-ro2ax max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iro = ot%rodrigues
  oro = cu2ro(ax2cu(ro2ax(iro)))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)  write (*,*) 'cu2ro-ax2cu-ro2ax max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iro = ot%rodrigues
  oro = eu2ro(qu2eu(ro2qu(iro)))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)  write (*,*) 'eu2ro-qu2eu-ro2qu max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iro = ot%rodrigues
  oro = om2ro(qu2om(ro2qu(iro)))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)  write (*,*) 'om2ro-qu2om-ro2qu max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iro = ot%rodrigues
  oro = ax2ro(qu2ax(ro2qu(iro)))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)  write (*,*) 'ax2ro-qu2ax-ro2qu max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iro = ot%rodrigues
  oro = ho2ro(qu2ho(ro2qu(iro)))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)  write (*,*) 'ho2ro-qu2ho-ro2qu max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iro = ot%rodrigues
  oro = cu2ro(qu2cu(ro2qu(iro)))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)  write (*,*) 'cu2ro-qu2cu-ro2qu max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iro = ot%rodrigues
  oro = eu2ro(ho2eu(ro2ho(iro)))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)  write (*,*) 'eu2ro-ho2eu-ro2ho max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iro = ot%rodrigues
  oro = om2ro(ho2om(ro2ho(iro)))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)  write (*,*) 'om2ro-ho2om-ro2ho max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iro = ot%rodrigues
  oro = ax2ro(ho2ax(ro2ho(iro)))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)  write (*,*) 'ax2ro-ho2ax-ro2ho max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iro = ot%rodrigues
  oro = qu2ro(ho2qu(ro2ho(iro)))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)  write (*,*) 'qu2ro-ho2qu-ro2ho max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iro = ot%rodrigues
  oro = cu2ro(ho2cu(ro2ho(iro)))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)  write (*,*) 'cu2ro-ho2cu-ro2ho max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iro = ot%rodrigues
  oro = eu2ro(cu2eu(ro2cu(iro)))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)  write (*,*) 'eu2ro-cu2eu-ro2cu max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iro = ot%rodrigues
  oro = om2ro(cu2om(ro2cu(iro)))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)  write (*,*) 'om2ro-cu2om-ro2cu max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iro = ot%rodrigues
  oro = ax2ro(cu2ax(ro2cu(iro)))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)  write (*,*) 'ax2ro-cu2ax-ro2cu max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iro = ot%rodrigues
  oro = qu2ro(cu2qu(ro2cu(iro)))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)  write (*,*) 'qu2ro-cu2qu-ro2cu max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iro = ot%rodrigues
  oro = ho2ro(cu2ho(ro2cu(iro)))
  if (oro(4).eq.infty) then
    diff = maxval(abs(oro(1:3))-abs(ot%rodrigues(1:3)))
  else
    diff = maxval(abs(oro-ot%rodrigues))
  end if 
if (verbose)  write (*,*) 'ho2ro-cu2ho-ro2cu max ro difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
 
if (verbose) write (*,*) 'triple qu test'
  iqu = ot%quat
  oqu = om2qu(eu2om(qu2eu(iqu)))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)  write (*,*) 'om2qu-eu2om-qu2eu max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iqu = ot%quat
  oqu = ax2qu(eu2ax(qu2eu(iqu)))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)  write (*,*) 'ax2qu-eu2ax-qu2eu max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iqu = ot%quat
  oqu = ro2qu(eu2ro(qu2eu(iqu)))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)  write (*,*) 'ro2qu-eu2ro-qu2eu max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iqu = ot%quat
  oqu = ho2qu(eu2ho(qu2eu(iqu)))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)  write (*,*) 'ho2qu-eu2ho-qu2eu max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iqu = ot%quat
  oqu = cu2qu(eu2cu(qu2eu(iqu)))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)  write (*,*) 'cu2qu-eu2cu-qu2eu max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iqu = ot%quat
  oqu = eu2qu(om2eu(qu2om(iqu)))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)  write (*,*) 'eu2qu-om2eu-qu2om max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iqu = ot%quat
  oqu = ax2qu(om2ax(qu2om(iqu)))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)  write (*,*) 'ax2qu-om2ax-qu2om max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iqu = ot%quat
  oqu = ro2qu(om2ro(qu2om(iqu)))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)  write (*,*) 'ro2qu-om2ro-qu2om max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iqu = ot%quat
  oqu = ho2qu(om2ho(qu2om(iqu)))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)  write (*,*) 'ho2qu-om2ho-qu2om max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iqu = ot%quat
  oqu = cu2qu(om2cu(qu2om(iqu)))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)  write (*,*) 'cu2qu-om2cu-qu2om max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iqu = ot%quat
  oqu = eu2qu(ax2eu(qu2ax(iqu)))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)  write (*,*) 'eu2qu-ax2eu-qu2ax max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iqu = ot%quat
  oqu = om2qu(ax2om(qu2ax(iqu)))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)  write (*,*) 'om2qu-ax2om-qu2ax max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iqu = ot%quat
  oqu = ro2qu(ax2ro(qu2ax(iqu)))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)  write (*,*) 'ro2qu-ax2ro-qu2ax max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iqu = ot%quat
  oqu = ho2qu(ax2ho(qu2ax(iqu)))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)  write (*,*) 'ho2qu-ax2ho-qu2ax max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iqu = ot%quat
  oqu = cu2qu(ax2cu(qu2ax(iqu)))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)  write (*,*) 'cu2qu-ax2cu-qu2ax max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iqu = ot%quat
  oqu = eu2qu(ro2eu(qu2ro(iqu)))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)  write (*,*) 'eu2qu-ro2eu-qu2ro max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iqu = ot%quat
  oqu = om2qu(ro2om(qu2ro(iqu)))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)  write (*,*) 'om2qu-ro2om-qu2ro max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iqu = ot%quat
  oqu = ax2qu(ro2ax(qu2ro(iqu)))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)  write (*,*) 'ax2qu-ro2ax-qu2ro max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iqu = ot%quat
  oqu = ho2qu(ro2ho(qu2ro(iqu)))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)  write (*,*) 'ho2qu-ro2ho-qu2ro max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iqu = ot%quat
  oqu = cu2qu(ro2cu(qu2ro(iqu)))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)  write (*,*) 'cu2qu-ro2cu-qu2ro max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iqu = ot%quat
  oqu = eu2qu(ho2eu(qu2ho(iqu)))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)  write (*,*) 'eu2qu-ho2eu-qu2ho max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iqu = ot%quat
  oqu = om2qu(ho2om(qu2ho(iqu)))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)  write (*,*) 'om2qu-ho2om-qu2ho max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iqu = ot%quat
  oqu = ax2qu(ho2ax(qu2ho(iqu)))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)  write (*,*) 'ax2qu-ho2ax-qu2ho max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iqu = ot%quat
  oqu = ro2qu(ho2ro(qu2ho(iqu)))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)  write (*,*) 'ro2qu-ho2ro-qu2ho max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iqu = ot%quat
  oqu = cu2qu(ho2cu(qu2ho(iqu)))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)  write (*,*) 'cu2qu-ho2cu-qu2ho max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iqu = ot%quat
  oqu = eu2qu(cu2eu(qu2cu(iqu)))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)  write (*,*) 'eu2qu-cu2eu-qu2cu max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iqu = ot%quat
  oqu = om2qu(cu2om(qu2cu(iqu)))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)  write (*,*) 'om2qu-cu2om-qu2cu max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iqu = ot%quat
  oqu = ax2qu(cu2ax(qu2cu(iqu)))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)  write (*,*) 'ax2qu-cu2ax-qu2cu max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iqu = ot%quat
  oqu = ro2qu(cu2ro(qu2cu(iqu)))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)  write (*,*) 'ro2qu-cu2ro-qu2cu max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iqu = ot%quat
  oqu = ho2qu(cu2ho(qu2cu(iqu)))
  if (abs(oqu(1)).lt.1.D-08) then
    diff = maxval(abs(oqu(2:4))-abs(ot%quat(2:4)))
  else
    diff = maxval(abs(oqu-ot%quat))
  end if 
if (verbose)  write (*,*) 'ho2qu-cu2ho-qu2cu max qu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
 
if (verbose) write (*,*) 'triple ho test'
  iho = ot%homochoric
  oho = om2ho(eu2om(ho2eu(iho)))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)  write (*,*) 'om2ho-eu2om-ho2eu max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iho = ot%homochoric
  oho = ax2ho(eu2ax(ho2eu(iho)))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)  write (*,*) 'ax2ho-eu2ax-ho2eu max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iho = ot%homochoric
  oho = ro2ho(eu2ro(ho2eu(iho)))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)  write (*,*) 'ro2ho-eu2ro-ho2eu max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iho = ot%homochoric
  oho = qu2ho(eu2qu(ho2eu(iho)))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)  write (*,*) 'qu2ho-eu2qu-ho2eu max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iho = ot%homochoric
  oho = cu2ho(eu2cu(ho2eu(iho)))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)  write (*,*) 'cu2ho-eu2cu-ho2eu max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iho = ot%homochoric
  oho = eu2ho(om2eu(ho2om(iho)))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)  write (*,*) 'eu2ho-om2eu-ho2om max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iho = ot%homochoric
  oho = ax2ho(om2ax(ho2om(iho)))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)  write (*,*) 'ax2ho-om2ax-ho2om max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iho = ot%homochoric
  oho = ro2ho(om2ro(ho2om(iho)))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)  write (*,*) 'ro2ho-om2ro-ho2om max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iho = ot%homochoric
  oho = qu2ho(om2qu(ho2om(iho)))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)  write (*,*) 'qu2ho-om2qu-ho2om max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iho = ot%homochoric
  oho = cu2ho(om2cu(ho2om(iho)))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)  write (*,*) 'cu2ho-om2cu-ho2om max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iho = ot%homochoric
  oho = eu2ho(ax2eu(ho2ax(iho)))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)  write (*,*) 'eu2ho-ax2eu-ho2ax max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iho = ot%homochoric
  oho = om2ho(ax2om(ho2ax(iho)))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)  write (*,*) 'om2ho-ax2om-ho2ax max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iho = ot%homochoric
  oho = ro2ho(ax2ro(ho2ax(iho)))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)  write (*,*) 'ro2ho-ax2ro-ho2ax max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iho = ot%homochoric
  oho = qu2ho(ax2qu(ho2ax(iho)))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)  write (*,*) 'qu2ho-ax2qu-ho2ax max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iho = ot%homochoric
  oho = cu2ho(ax2cu(ho2ax(iho)))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)  write (*,*) 'cu2ho-ax2cu-ho2ax max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iho = ot%homochoric
  oho = eu2ho(ro2eu(ho2ro(iho)))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)  write (*,*) 'eu2ho-ro2eu-ho2ro max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iho = ot%homochoric
  oho = om2ho(ro2om(ho2ro(iho)))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)  write (*,*) 'om2ho-ro2om-ho2ro max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iho = ot%homochoric
  oho = ax2ho(ro2ax(ho2ro(iho)))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)  write (*,*) 'ax2ho-ro2ax-ho2ro max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iho = ot%homochoric
  oho = qu2ho(ro2qu(ho2ro(iho)))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)  write (*,*) 'qu2ho-ro2qu-ho2ro max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iho = ot%homochoric
  oho = cu2ho(ro2cu(ho2ro(iho)))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)  write (*,*) 'cu2ho-ro2cu-ho2ro max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iho = ot%homochoric
  oho = eu2ho(qu2eu(ho2qu(iho)))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)  write (*,*) 'eu2ho-qu2eu-ho2qu max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iho = ot%homochoric
  oho = om2ho(qu2om(ho2qu(iho)))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)  write (*,*) 'om2ho-qu2om-ho2qu max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iho = ot%homochoric
  oho = ax2ho(qu2ax(ho2qu(iho)))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)  write (*,*) 'ax2ho-qu2ax-ho2qu max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iho = ot%homochoric
  oho = ro2ho(qu2ro(ho2qu(iho)))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)  write (*,*) 'ro2ho-qu2ro-ho2qu max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iho = ot%homochoric
  oho = cu2ho(qu2cu(ho2qu(iho)))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)  write (*,*) 'cu2ho-qu2cu-ho2qu max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iho = ot%homochoric
  oho = eu2ho(cu2eu(ho2cu(iho)))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)  write (*,*) 'eu2ho-cu2eu-ho2cu max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iho = ot%homochoric
  oho = om2ho(cu2om(ho2cu(iho)))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)  write (*,*) 'om2ho-cu2om-ho2cu max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iho = ot%homochoric
  oho = ax2ho(cu2ax(ho2cu(iho)))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)  write (*,*) 'ax2ho-cu2ax-ho2cu max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iho = ot%homochoric
  oho = ro2ho(cu2ro(ho2cu(iho)))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)  write (*,*) 'ro2ho-cu2ro-ho2cu max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  iho = ot%homochoric
  oho = qu2ho(cu2qu(ho2cu(iho)))
  aux = sqrt(sum(oho*oho))
  if (abs(aux-LPs%R1).lt.1.D-08) then
    diff = maxval(abs(oho)-abs(ot%homochoric))
  else
    diff = maxval(abs(oho-ot%homochoric))
  end if 
if (verbose)  write (*,*) 'qu2ho-cu2qu-ho2cu max ho difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
 
if (verbose) write (*,*) 'triple cu test'
  icu = ot%cubochoric
  ocu = om2cu(eu2om(cu2eu(icu)))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)  write (*,*) 'om2cu-eu2om-cu2eu max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  icu = ot%cubochoric
  ocu = ax2cu(eu2ax(cu2eu(icu)))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)  write (*,*) 'ax2cu-eu2ax-cu2eu max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  icu = ot%cubochoric
  ocu = ro2cu(eu2ro(cu2eu(icu)))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)  write (*,*) 'ro2cu-eu2ro-cu2eu max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  icu = ot%cubochoric
  ocu = qu2cu(eu2qu(cu2eu(icu)))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)  write (*,*) 'qu2cu-eu2qu-cu2eu max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  icu = ot%cubochoric
  ocu = ho2cu(eu2ho(cu2eu(icu)))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)  write (*,*) 'ho2cu-eu2ho-cu2eu max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  icu = ot%cubochoric
  ocu = eu2cu(om2eu(cu2om(icu)))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)  write (*,*) 'eu2cu-om2eu-cu2om max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  icu = ot%cubochoric
  ocu = ax2cu(om2ax(cu2om(icu)))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)  write (*,*) 'ax2cu-om2ax-cu2om max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  icu = ot%cubochoric
  ocu = ro2cu(om2ro(cu2om(icu)))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)  write (*,*) 'ro2cu-om2ro-cu2om max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  icu = ot%cubochoric
  ocu = qu2cu(om2qu(cu2om(icu)))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)  write (*,*) 'qu2cu-om2qu-cu2om max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  icu = ot%cubochoric
  ocu = ho2cu(om2ho(cu2om(icu)))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)  write (*,*) 'ho2cu-om2ho-cu2om max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  icu = ot%cubochoric
  ocu = eu2cu(ax2eu(cu2ax(icu)))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)  write (*,*) 'eu2cu-ax2eu-cu2ax max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  icu = ot%cubochoric
  ocu = om2cu(ax2om(cu2ax(icu)))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)  write (*,*) 'om2cu-ax2om-cu2ax max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  icu = ot%cubochoric
  ocu = ro2cu(ax2ro(cu2ax(icu)))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)  write (*,*) 'ro2cu-ax2ro-cu2ax max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  icu = ot%cubochoric
  ocu = qu2cu(ax2qu(cu2ax(icu)))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)  write (*,*) 'qu2cu-ax2qu-cu2ax max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  icu = ot%cubochoric
  ocu = ho2cu(ax2ho(cu2ax(icu)))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)  write (*,*) 'ho2cu-ax2ho-cu2ax max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  icu = ot%cubochoric
  ocu = eu2cu(ro2eu(cu2ro(icu)))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)  write (*,*) 'eu2cu-ro2eu-cu2ro max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  icu = ot%cubochoric
  ocu = om2cu(ro2om(cu2ro(icu)))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)  write (*,*) 'om2cu-ro2om-cu2ro max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  icu = ot%cubochoric
  ocu = ax2cu(ro2ax(cu2ro(icu)))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)  write (*,*) 'ax2cu-ro2ax-cu2ro max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  icu = ot%cubochoric
  ocu = qu2cu(ro2qu(cu2ro(icu)))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)  write (*,*) 'qu2cu-ro2qu-cu2ro max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  icu = ot%cubochoric
  ocu = ho2cu(ro2ho(cu2ro(icu)))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)  write (*,*) 'ho2cu-ro2ho-cu2ro max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  icu = ot%cubochoric
  ocu = eu2cu(qu2eu(cu2qu(icu)))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)  write (*,*) 'eu2cu-qu2eu-cu2qu max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  icu = ot%cubochoric
  ocu = om2cu(qu2om(cu2qu(icu)))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)  write (*,*) 'om2cu-qu2om-cu2qu max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  icu = ot%cubochoric
  ocu = ax2cu(qu2ax(cu2qu(icu)))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)  write (*,*) 'ax2cu-qu2ax-cu2qu max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  icu = ot%cubochoric
  ocu = ro2cu(qu2ro(cu2qu(icu)))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)  write (*,*) 'ro2cu-qu2ro-cu2qu max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  icu = ot%cubochoric
  ocu = ho2cu(qu2ho(cu2qu(icu)))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)  write (*,*) 'ho2cu-qu2ho-cu2qu max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  icu = ot%cubochoric
  ocu = eu2cu(ho2eu(cu2ho(icu)))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)  write (*,*) 'eu2cu-ho2eu-cu2ho max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  icu = ot%cubochoric
  ocu = om2cu(ho2om(cu2ho(icu)))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)  write (*,*) 'om2cu-ho2om-cu2ho max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  icu = ot%cubochoric
  ocu = ax2cu(ho2ax(cu2ho(icu)))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)  write (*,*) 'ax2cu-ho2ax-cu2ho max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  icu = ot%cubochoric
  ocu = ro2cu(ho2ro(cu2ho(icu)))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)  write (*,*) 'ro2cu-ho2ro-cu2ho max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
  icu = ot%cubochoric
  ocu = qu2cu(ho2qu(cu2ho(icu)))
  aux = maxval(abs(ocu))
  if (abs(aux-LPs%ap*0.5).lt.1.D-08) then
    diff = maxval(abs(ocu)-abs(ot%cubochoric))
  else
    diff = maxval(abs(ocu-ot%cubochoric))
  end if 
if (verbose)  write (*,*) 'qu2cu-ho2qu-cu2ho max cu difference = ', diff
  diffmax = maxval( (/ diffmax,diff /) )
tcnt = tcnt+1
 
 
 write (*,*) ' Maximum difference in triplet tests  : ', diffmax
end do
end program rtest
