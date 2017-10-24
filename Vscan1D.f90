!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   Program :                                                   !
!               Produce a serious of structure along a selected !
!               reaction coordinate.                            !
!   Input :                                                     !
!           $1 = Gaussian/Qchem output file; initial structure  !
!           LM                                                  !
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
!           NM                                                  !
!           $2 = boundary condition                             !
!   Output :                                                    !
!           PES.xyz; it can be used in Jmol.                    !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! 2016/04/12, Grace, 1st. ver.
! 2017/10/23, Grace, 2nd. ver. modify input and auxiliary scripts

Program main
    implicit none
    LOGICAL :: RxnCoord
    character(len=100)  :: input
    integer(4)  :: i,NAtoms
    real(8),allocatable,dimension(:,:)  :: Coord,Vec,Delta
    integer(4),dimension(3)  :: BC
    ! The output file of qchem record the atom name as character
    character(len=2),allocatable,dimension(:)   :: AtomName

! Step 0.   Std-out the purpose of this program, and check the
!           status of input files. 
    call print_purpose()
    call get_RxnCoord(RxnCoord)
    
! Step 1. Extract the coordinate and boundary from input file
    call GETARG(1,input)
    call SYSTEM('getCoord '//TRIM(input)//' initial.xyz')
    open(10,file='initial.xyz',status='old',action='read')
    read(10,*) NAtoms
    read(10,*) input ! this is buffer
    allocate(Coord(NAtoms,3))
    allocate(AtomName(NAtoms))
    allocate(Vec(NAtoms,3))
    allocate(Delta(NAtoms,3))
    call get_coord(NAtoms,Coord,AtomName)
    close(10)
    
    if (RxnCoord) then
        ! Normal mode
        write(*,'(A)') 'Normal mode is not finish yet'
        stop ! not yet, 2017/10/24
    else
        ! Local mode; full local mode
        call GETARG(2,input)
        call get_LM(input,NAtoms,Coord,Vec)
    end if
    
    call GETARG(3,input)
    open(10,file=input,status='old',action='read')
    read(10,*) BC(1:3)
    close(10)

    ! Calculate the interval vector, delta.
    do i=1,NAtoms
        Delta(i,1)=Vec(i,1)/BC(3)
        Delta(i,2)=Vec(i,2)/BC(3)
        Delta(i,3)=Vec(i,3)/BC(3)
    end do

! Step 2. Construct a serious of structures and then write them 
!         into the pes.txt file
    call write_pes(NAtoms,AtomName,Coord,BC,Delta)
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
    write(*,'(A)') 'Final output: PES.xyz'
    write(*,'(A)') '              (it can be visualized by Jmol)'
    write(*,'()')
return
end subroutine print_purpose

subroutine get_RxnCoord(RxnCoord)
    implicit none
    LOGICAL,intent(out) :: RxnCoord
    integer(4)  :: i,num
    num = IARGC() 
    if ( num .eq. 2) then
        ! Normal mode
        RxnCoord = .true.
    else if (num .eq. 3) then
        ! Local mode       
        RxnCoord = .false.
    else
        write(*,'(A)') 'Wrong amount of input arguments, stop!'
        STOP
    end if
    do i = 1,num ! check the input file status
        call check_file(i)
    end do
return
end subroutine get_RxnCoord

subroutine check_file(num)
    implicit none
    integer(4),intent(in)   :: num
    character(len=100)  :: input
    LOGICAL     :: filestat
    call GETARG(num,input)
    INQUIRE(file=input,exist=filestat)
    if (filestat) then
        open(10,file=input,status='old',action='read')
    else
        write(*,'(A)') TRIM(input)//" doesn't exist"
        stop
    end if
return
end subroutine check_file

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

! two sub-category: full LM and partial LM
subroutine get_LM(input,NAtoms,Coord,Vec)
    implicit none
    character(len=100),intent(in)  :: input
    integer(4),intent(in)   :: NAtoms
    real(8),dimension(NAtoms,3),intent(in) :: Coord
    real(8),dimension(NAtoms,3),intent(inout)   :: Vec
    character(len=100)  :: buffer
    real(8),dimension(NAtoms,3) :: Coord_final
    integer(4)  :: i,j,Mode,n
    real(4),allocatable,dimension(:)    :: movingAtom

    write(*,'(A)') 'Is the final state a Gaussian/Qchem output? (y/n)'
    read(*,*) buffer
    if ( buffer .eq. 'y') then
        call SYSTEM('getCoord '//TRIM(input)//' final.xyz')
        open(10,file='final.xyz',status='old',action='read')
    else if ( buffer .eq. 'n' ) then
        open(10,file=input,status='old',action='read')
    else
        write(*,'(A)') 'Wrong typing! Stop.'
        STOP
    end if
    read(10,*) buffer
    read(10,*) buffer
    do i=1,NAtoms
        read(10,*) buffer,Coord_final(i,1:3)
    end do

    write(*,'()')
    write(*,'(A)') 'This reaction coordinate follow local mode (LM)'
    write(*,'()')
    write(*,'(A)') 'The order of atom for initial and final'
    write(*,'(A)') 'molecules must be the same!'
    write(*,'()')
    write(*,'(A)') 'Which mode do you prefer: (1 or 2)'
    write(*,'(A)') '1. Full LM: move all atoms '
    write(*,'(A)') '2. Partial LM: assign certain atom(s)'
    read(*,*) Mode
    Vec(:,:) = 0.0D0
    if ( Mode .eq. 1) then
        do i = 1,NAtoms
            do j = 1,3
                Vec(i,j) = Coord_final(i,j) - Coord(i,j)
            end do
        end do
    else if ( Mode .eq. 2 ) then
        write(*,'(A)',advance='no') 'How many atom: '
        read(*,*) n
        allocate(movingAtom(n))
        do i = 1,n
            write(*,'(A)') 'Please assign the index of moving atom'
            read(*,*) movingAtom(i)
            do j = 1, 3
                Vec(movingAtom(i),j) = Coord_final(movingAtom(i),j) - Coord(movingAtom(i),j)
            end do
        end do
    end if
    close(10)
return
end subroutine get_LM

subroutine write_pes(NAtoms,AtomName,Coord,BC,Delta)
    implicit none
    integer(4),intent(in)   :: NAtoms
    character(len=2),intent(inout),dimension(NAtoms)  :: AtomName
    real(8),dimension(NAtoms,3),intent(in)  :: Coord,Delta
    integer(4),dimension(3),intent(in)  :: BC
    integer(4)  :: i,j,k
    real(8),dimension(NAtoms,3) :: Coord_tmp
    real(8) :: indexR ! in order to print the coordinate
    character(len=100)  :: buffer

    open(10,file='PES.xyz',status='replace')
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
    write(10,'(A)') '0.0000' 
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
        write(buffer,'(F7.4)') indexR
        write(10,'(A)') 'n'//TRIM(ADJUSTL(buffer))
        do j=1,NAtoms
            write(10,'(A2,1X,3(F13.10,1X))') AtomName(j),Coord_tmp(j,1:3)
        end do
    end do
    close(10)
return
end subroutine write_pes