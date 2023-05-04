program dac-dcc

! gfortran dac-dcc.f90 -o dac-dcc
! ./dac-dcc input1.txt input2.txt output.txt

implicit none

character(len=100) :: filename1, filename2, filename3
integer :: i, j, k
integer, parameter :: max = 1000
real, dimension(1:1000) :: occurrence
real, dimension(:), allocatable :: pos, weight, dist

!  integer :: i, j, k, n
!  real :: dx, dy, dz, r, rmax, dr, vol, c
!  real, dimension(:,:), allocatable :: pos
!  real, dimension(:), allocatable :: dist, corr
!  character(len=50) :: infile, outfile
!  logical :: in_file_open, out_file_open

! Read command line arguments
call get_command_argument(1, filename1)
call get_command_argument(2, filename2)
call get_command_argument(3, filename3)

! Open input and output files
open(unit=10, file=filename1, status='old', action='read', iostat=i)
open(unit=20, file=filename2, status='old', action='read', iostat=i)
open(unit=30, file=filename3, status='new', action='write', iostat=i)


! Open the input file
open(unit=10, file=infile, status='old', action='read', iostat=ierr)
  if (ierr /= 0) then
    print *, "Error opening input file"
    stop
  else
    in_file_open = .true.
  end if



! Skip header line
read(10,*)
read(20,*)

! Initialize occurrence array
occurrence = 0

! Loop over lines in input files
do while (.true.)
  ! Read line from first input file
  read(10,*,iostat=i) 
  if (i/=0) exit ! end of file
  ! Skip first value
  read(10,*) 
  ! Read second value and convert to integer
  integer :: val1
  read(10,*) val1
  ! Read count and convert to float
  real :: count1
  read(10,*) count1

  ! Loop over lines in second input file
  do while (.true.)
    ! Read line from second input file
    read(20,*,iostat=i)
    if (i/=0) exit ! end of file
    ! Skip first value
    read(20,*)
    ! Read second value and convert to integer
    integer :: val2
    read(20,*) val2
    ! Read count and convert to float
    real :: count2
    read(20,*) count2

    ! Calculate distance and update occurrence array if within bounds
    integer :: dist
    dist = val2 - val1 + 1
    if (dist >= 0 .and. dist <= max) then
      occurrence(dist) = occurrence(dist) + count1 * count2
    end if
  end do
end do

! Write occurrence array to output file
write(30,'(i4, f10.2)') (i, occurrence(i), i=0,max)

! Close input and output files
close(10)
close(20)
close(30)

end program myprogram

do j = 1, size(all_data1)
  do m = 1, size(all_data2)
    dist = int(all_data2(m)) - int(all_data1(j)) + 1

    if (dist >= 0 .and. dist <= max) then
      occurrence(dist) = occurrence(dist) + real(all_count1(j)) * real(all_count2(m))
    end if
  end do
end do

end program dac-dcc
