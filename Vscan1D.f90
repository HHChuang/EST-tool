!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   Program :                                                   !
!               Produce a serious of structure along a selected !
!               coordinate.                                     !
!   Input :                                                     !
!           $1 = Gaussian/Qchem output file; initial structure  !
!           $2 = vector (xyz coordinate)                        !
!           $3 = boundary condition                             !
!                formate of $3                                  !
!                $1=upper BC ; postive integer                  !
!                   # of steps in the positive direction        !
!                $2=lower BC ; positive integer                 !
!                   # of step2 in the negative direction        !
!                $3=# of interval ; positive integer            !
!                   interval=1/$3                               !
!                   i.e. $3=10 ; interval=1/10=0.1 angstrom     !
!   Output :                                                    !
!           *.xyz; User define, and it can be used in Jomol.    !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! 2016/04/12, Grace, 1st. ver.
! 2017/10/23, Grace, 2nd. ver. modify input and auxiliary scripts

Program main
    implicit none
    character(len=100)  :: input
    logical :: filestat
    integer(4)  :: i,j,k,NAtoms
    real(8),allocatable,dimension(:,:)  :: Coord,Vec,Delta,Coord_tmp
    integer(4),dimension(3)  :: BC
    ! The output file of qchem record the atom name as character
    character(len=2),allocatable,dimension(:)   :: AtomName
    real(8) :: indexR ! in order to print the coordinate

! Step 0.   Std-out the purpose of this program, and check the
!           status of input file. 
    call print_purpose()
! Step 1. Extract the coordinate and boundary from input file
   ! The first argument; input file = coordinate
    call GETARG(1,input)
    INQUIRE(file=input,exist=filestat)
    if (filestat) then
        open(10,file=input,status='old',action='read')
    else
        write(*,'(A)') TRIM(input)//" doesn't exist"
        stop
    end if
    i=0
    do while ( .not. eof(10) )
        read(10,'(A)') input ! it is a buffer
        i=i+1
    end do
    rewind(10)
    NAtoms=i
    allocate(Coord(NAtoms,3))
    allocate(Coord_tmp(NAtoms,3))
    allocate(AtomName(NAtoms))
    call get_coord(NAtoms,Coord,AtomName)
    close(10)
    ! The second argment; input file = vector
    call GETARG(2,input)
    INQUIRE(file=input,exist=filestat)
    if (filestat) then
        open(10,file=input,status='old',action='read')
    else
        write(*,'(A)') TRIM(input)//" doesn't exist"
        stop
    end if
    allocate(Vec(NAtoms,3))
    do i=1,NAtoms
        read(10,*) Vec(i,1:3)
    end do
    close(10)
    ! The third argument; input file = boundary condition file
    call GETARG(3,input)
    INQUIRE(file=input,exist=filestat)
    if (filestat) then
        open(10,file=input,status='old',action='read')
    else
        write(*,'(A)') TRIM(input)//" doesn't exist"
        stop
    end if
    read(10,*) BC(1:3)
    close(10)
    ! Calculate the interval vector, delta.
    allocate(Delta(NAtoms,3))
    do i=1,NAtoms
        Delta(i,1)=Vec(i,1)/BC(3)
        Delta(i,2)=Vec(i,2)/BC(3)
        Delta(i,3)=Vec(i,3)/BC(3)
    end do
! Step 2. Construct a serious of structures and then write them 
!         into the pes.txt file
    open(10,file='pes.txt',status='replace')
    ! Upper boundary
    do i=BC(1),1,-1
        do j=1,NAtoms
            do k=1,3
                Coord_tmp(j,k)=Coord(j,k)+i*Delta(j,k)
            end do
        end do
        ! write the structure into file, coord.txt
        write(10,'(I2)') NAtoms
        indexR=0.0D0+i*1.0D0/BC(3)
        write(10,'(F7.4)') indexR
        do j=1,NAtoms
            write(10,'(A2,1X,3(F13.10,1X))') AtomName(j),Coord_tmp(j,1:3)
        end do
    end do
    ! Original point
    write(10,'(I2)') NAtoms
    write(10,'(A)') '0.0000' ! this is the transition state
    do i=1,NAtoms
        write(10,'(A2,1X,3(F13.10,1X))') AtomName(i),Coord(i,1:3)
    end do
    ! Lower boundary
    do i=1,BC(2)
        do j=1,NAtoms
            do k=1,3
                Coord_tmp(j,k)=Coord(j,k)-i*Delta(j,k)
            end do
        end do
        ! write the structure into file, coord.txt
        write(10,'(I2)') NAtoms
        indexR=0.0D0-i*1.0D0/BC(3)
        !  linux bug that it cannot recognize the negative sign
        write(input,'(F7.4)') indexR
        write(10,'(A)') 'n'//TRIM(ADJUSTL(input))
        do j=1,NAtoms
            write(10,'(A2,1X,3(F13.10,1X))') AtomName(j),Coord_tmp(j,1:3)
        end do
    end do
    close(10)
end program main

subroutine print_purpose()
    implicit none
    write(*,'()')
    write(*,'(A)') '-------------------------------------------------'
    write(*,'(A)') 'Program Vscan1D produces a serious of structure'
    write(*,'(A)') 'along a given reaction coordinate.'
    write(*,'(A)') '-------------------------------------------------'
    write(*,'()')
    write(*,'(A)') 'Auxiliary script and file'
    write(*,'(A)') '1. script: getCoord'
    write(*,'(A)') '   output: coord.xyz'
    write(*,'(A)') '           (afford Gaussian/QChem jobs, 2017/10/23)'
    write(*,'(A)') '2. script: getNM'
    write(*,'(A)') '   output: NM.dat'
    write(*,'(A)') '           (afford QChem jobs only, 2017/10/23)' 
    write(*,'()')
    write(*,'(A)') 'Final output: *.xyz'
    write(*,'(A)') '              (user define the name and it can be'
    write(*,'(A)') '               visualized by Jmol)'
    write(*,'()')
return
end subroutine print_purpose

subroutine get_coord(NAtoms,Coord,AtomName)
    implicit none
    integer(4),intent(in)   :: NAtoms
    real(8),intent(inout),dimension(NAtoms,3)   :: Coord
    character(len=2),intent(inout),dimension(NAtoms)  :: AtomName
    !local variable
    integer(4) :: i
    do i=1,NAtoms
        read(10,*) AtomName(i),Coord(i,1:3)
    end do
return
end subroutine get_coord

