!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   Program :                                                                   !
!               Produce a serious of structure along a serious of vectors       !
!                                                                               !
!   Input :                                                                     !
!           $1 = a serious of initial structure in xyz formate                  !
!           $2 = a serious of vector (xyz coordinate)                           !
!   Output :                                                                    !
!           pes.dat                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! 2016/11/14, Grace, 1st. ver

Program main
    implicit none
    integer(4)  :: fid,NAtoms,NFiles_x,NFiles_y
    integer(4),allocatable,dimension(:) :: AtomsName
    real(8),allocatable,dimension(:,:)  :: TotStruc,TotVec
    real(8),allocatable,dimension(:,:) :: Struc,Vec,tmp
    integer(4)  :: i,j,m,n
    character(len=100)  :: coord
! Step 1. Std-out the purpose of this program
    call purpose()
! Step 2. Extract the structures and vectors
    ! check the status of input files
    do fid=1,2
        call checkFile(fid)
    end do
    call getNAtoms(NAtoms) 
    allocate(AtomsName(NAtoms))
    allocate(Struc(NAtoms,3))
    allocate(Vec(NAtoms,3))
    allocate(tmp(NAtoms,3))
    call getAtomsName(NAtoms,AtomsName)
    call getNFiles(1,NAtoms,NFiles_x)
    call getNFiles(2,NAtoms,NFiles_y)
    allocate(TotStruc(NAtoms*NFiles_x,3))
    allocate(TotVec(NAtoms*NFiles_y,3))
    call getTot(1,NAtoms,NFiles_x,TotStruc)
    call getTot(2,NAtoms,NFiles_y,TotVec)
! Step 3. Create the structures
    open(10,file='pes.dat',status='replace')
    do i=1,NFiles_x
        call getPart(i,NAtoms,NFiles_x,TotStruc,Struc)
        tmp=Struc
        write(10,'(I2)') NAtoms
        call getIntCoord(i,0,coord)
        ! call getFloatCoord(,coord)
            write(10,'(A)') TRIM(ADJUSTL(coord))
        do m=1,NAtoms
            write(10,'(I2,3((1X,F10.6)))') AtomsName(m),tmp(m,1:3)
        end do   
        do j=1,NFiles_y
            call getPart(j,NAtoms,NFiles_y,TotVec,Vec) 
            write(10,'(I2)') NAtoms
            call getIntCoord(i,j,coord)
            write(10,'(A)') TRIM(ADJUSTL(coord))
            do n=1,3
                do m=1,NAtoms
                    tmp(m,n)=tmp(m,n)+Vec(m,n)
                end do
            end do
            do m=1,NAtoms
                write(10,'(I2,3((1X,F10.6)))') AtomsName(m),tmp(m,1:3)
            end do
        end do
    end do
    close(10)
stop
end program main

subroutine checkFile(fid)
    implicit none
    integer(4),intent(in)  :: fid
    character(len=100)  :: input
    logical :: filestat
    call GETARG(fid,input)
    INQUIRE(file=input,exist=filestat)
    if (filestat) then
        open(10,file=input,status='old',action='read')
    else
        write(*,'(A)') TRIM(input)//" doesn't exist"
        stop
    end if
    return
end subroutine checkFile

subroutine getNAtoms(NAtoms)
    implicit none
    integer(4),intent(out)  :: NAtoms
    character(len=100)  :: input
    call GETARG(1,input) ! use the first file to assign NAtoms
    open(10,file=input,status='old',action='read')
    read(10,*)  NAtoms
    close(10)
    return
end subroutine getNAtoms

subroutine getAtomsName(NAtoms,AtomsName)
    implicit none
    integer(4),intent(in)   :: NAtoms
    integer(4),dimension(NAtoms),intent(out)    :: AtomsName
    character(len=100)  :: input
    integer(4)  :: i
    call GETARG(1,input) ! use the first file to assign AtomName
    open(10,file=input,status='old',action='read')
    read(10,*) input ! buffer
    read(10,*) input !buffer
    do i=1,NAtoms
        read(10,*) AtomsName(i),input
    end do
    close(10)
    return
end subroutine getAtomsName

subroutine getNFiles(fid,NAtoms,NFiles)
    implicit none
    integer(4),intent(in)  :: fid,NAtoms
    integer(4),intent(out)  :: NFiles
    character(len=100)  :: input
    integer(4)  :: tot
    call GETARG(fid,input) 
    open(10,file=input,status='old',action='read')
    tot=0
    do while (.not. eof(10))
        tot=tot+1
        read(10,*) input ! buffer
    end do 
    NFiles=tot/(NAtoms+2)
    close(10)
    return
end subroutine getNFiles

subroutine getTot(fid,NAtoms,NFiles,Tot)
    implicit none   
    integer(4),intent(in)   :: fid,NAtoms,NFiles
    real(8),dimension(NAtoms*NFiles,3),intent(out)  :: Tot
    character(len=100)  :: input
    integer(4)  :: i,j,k
    call GETARG(fid,input)
    open(10,file=input,status='old',action='read')
    k=0
    select case (fid)
        case(1)
            do i=1,NFiles
                read(10,*) input ! buffer
                read(10,*) input ! buffer
                do j=1,NAtoms
                    k=k+1
                    read(10,*) input,Tot(k,1),Tot(k,2),Tot(k,3)
                end do
            end do
        case(2)
            do i=1,NFiles
                read(10,*) input ! buffer
                read(10,*) input ! buffer
                do j=1,NAtoms
                    k=k+1
                    read(10,*) Tot(k,1),Tot(k,2),Tot(k,3)
                end do
            end do
    end select
    close(10)
    return
end subroutine getTot

subroutine getPart(coord,NAtoms,NFiles,Tot,Part) 
    implicit none
    integer(4),intent(in)   :: coord,NAtoms,NFiles
    real(8),dimension(NAtoms*NFiles,3),intent(in)   :: Tot
    real(8),dimension(NAtoms,3),intent(out) :: Part
    integer(4)  :: i,j,tot_index
    do j=1,3
        tot_index=1+NAtoms*(coord-1)
        do i=1,NAtoms    
            Part(i,j)=Tot(tot_index,j)
            tot_index=tot_index+1
        end do
    end do
    !do i=1,NAtoms
    !    write(*,*) Part(i,1:3)
    !end do
    !stop
    return
end subroutine getPart

subroutine getIntCoord(x,y,coord)
    implicit none
    integer(4),intent(in)   :: x,y
    character(len=100),intent(out)  :: coord
    character(len=100)  :: tmpx,tmpy
    write(tmpx,*) x
    write(tmpy,*) y
    write(coord,*) TRIM(ADJUSTL(tmpx))//'_'//TRIM(ADJUSTL(tmpy))
    return
end subroutine getIntCoord

subroutine purpose()
    implicit none
    write(*,'()')
    write(*,'(A)') '-------------------------------------------------------'
    write(*,'(A)') 'Program Vscan1D_IRCvec produces a serious of structures'
    write(*,'(A)') 'along a serious of given vectors.'
    write(*,'(A)') '-------------------------------------------------------'
    write(*,'()')
    write(*,'(A)') '-------------------------------------------------------'      
    write(*,'(A)') 'Auxiliary bash shell scripti: getIRCstruc, getIRCvec'
    write(*,'()')
    write(*,'(A)') 'Output file : pes.dat'
    write(*,'(A)') '-------------------------------------------------------'
    write(*,'()')
    return
end subroutine purpose