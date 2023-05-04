program read_file_into_array
  implicit none

  integer, parameter :: n = 100 ! Size of array
  integer :: i, j
  real :: x(n) ! Array to hold data
  character(len=100) :: filename ! Filename to read from
  integer :: file_unit, status ! File unit and I/O status

  ! Read filename from command line
  call get_command_argument(1, filename)

  ! Open file for reading
  open(unit=file_unit, file=filename, status='old', action='read', iostat=status)
  if (status /= 0) then
    write(*, *) "Error: could not open file"
    stop
  end if

  ! skip first line
  read(file_unit,*)

  ! Read data from file into array
  do i = 1, n
    read(file_unit, *, iostat=status) x(i)
    if (status /= 0) exit ! end of file
  end do

  ! Close file
  close(file_unit)

  ! Print out the data
  do i = 1, n
    write(*,*) x(i)
  end do
end program read_file_into_array

