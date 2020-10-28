!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   Program :                                                   !
!           Find out the centre-of-mass of attached water       !
!   Input   :                                                   !
!           $1 = list.txt ; record the filename                 !
!           *.out                                               !
!   Output  :                                                   !
!           comW.txt                                            !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!2015/11/27,Grace, 1st. ver.

Program main
    implicit none
    integer(4)  :: NAtoms,Nsys,nFile,NWater,i,j,k
    character(len=100)  :: filename,buffer,buffer1
    logical :: filestat
    real(8),allocatable,dimension(:,:)  :: Coord
    real(8),dimension(3)    :: W,summ
    
    write(*,'(A)',advance='no') 'Key-in the number of water : '
    read(*,*) NWater
    call GETARG(1,filename)
    INQUIRE(file=filename,exist=filestat)
    if (filestat) then
        open(10,file=filename,status='old',action='read')
    else
        write(*,'(A)') TRIM(filename)//" doesn't exist"
        stop
    end if
    open(100,file=filename,status='old')
    nFile=0
    do while ( .not. eof(100) )
        nFile=nFile+1
        read(100,'(A)') buffer
    end do
    rewind(100)
    open(1000,file='comW.txt',status='replace')
    do i=1,nFile
        read(100,'(A)') filename
        if ( i .eq. 1 ) then
            call system("grep -w 'NAtoms' "//TRIM(filename)//&
                        " | head -n 1 | awk '{print $2}' > natoms.txt" )
            open(300,file='natoms.txt',status='old')
            read(300,*) NAtoms
            close(300,status='delete')
            allocate(Coord(NWater*3,3))
            Nsys=NAtoms-NWater*3
        end if
        open(200,file=filename,status='old')
        write(buffer,*) NAtoms+4
        write(buffer1,*) NAtoms
        call system("grep -A "//TRIM(ADJUSTL(buffer))//&
                    " 'orientation:' "//TRIM(ADJUSTL(filename))//&
                    " | tail -n "//TRIM(ADJUSTL(buffer1))//" > coord.temp")
        call system("awk '{print $2,$4,$5,$6}' coord.temp > coord.txt")       
        call system("rm -f coord.tmp")  
        close(200)
        ! import the rawdata
        open(200,file='coord.txt',status='old')
        do j=1,Nsys
            read(200,'(A)') buffer
        end do
        do j=1,NWater*3
            read(200,*) buffer,Coord(j,:)
        end do
        ! calculate the centre-of-mass of water
        ! weight the coordinate
        do j=1,NWater*3,3
            Coord(j,1)=Coord(j,1)*16            
            Coord(j,2)=Coord(j,2)*16
            Coord(j,3)=Coord(j,3)*16
        end do
        ! sum
        summ(:)=0.0D0
        do j=1,NWater*3
            summ(1)=Coord(j,1)+summ(1)
            summ(2)=Coord(j,2)+summ(2)
            summ(3)=Coord(j,3)+summ(3)
        end do
        do j=1,3
            W(j)=summ(j)/(18.*NWater)
        end do
        ! write the file
        write(buffer,*) W(:)
        write(1000,'(A)') TRIM(ADJUSTL(filename))//' '//TRIM(ADJUSTL(buffer))
        close(200)
    end do
    close(100)
    close(1000)
stop
end program main
