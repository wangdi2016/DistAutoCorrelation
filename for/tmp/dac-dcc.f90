program distance_autocorrelation

  implicit none
  
  integer :: i, j, k, n
  real :: dx, dy, dz, r, rmax, dr, vol, c
  real, dimension(:,:), allocatable :: pos
  real, dimension(:), allocatable :: dist, corr
  character(len=50) :: infile, outfile
  logical :: in_file_open, out_file_open

  ! Read the input file name and output file name
  infile = "input.txt"
  outfile = "output.txt"

  ! Open the input file
  open(unit=10, file=infile, status='old', action='read', iostat=ierr)
  if (ierr /= 0) then
    print *, "Error opening input file"
    stop
  else
    in_file_open = .true.
  end if

  ! Read the number of particles and the maximum separation distance from the input file
  read(10,*) n
  read(10,*) rmax

  ! Close the input file
  if (in_file_open) close(10)

  ! Allocate memory for the position array and the distance and correlation arrays
  allocate(pos(3,n))
  allocate(dist(n*(n-1)/2))
  allocate(corr(int(rmax/dr)))

  ! Generate random particle positions in a cubic box of volume 1.0
  call random_number(pos)
  pos = pos - 0.5
  pos = pos * (1.0 / n**(1.0/3.0))
  
  ! Compute the pairwise distances between particles
  k = 1
  do i = 1, n-1
    do j = i+1, n
      dx = pos(1,i) - pos(1,j)
      dy = pos(2,i) - pos(2,j)
      dz = pos(3,i) - pos(3,j)
      r = sqrt(dx**2 + dy**2 + dz**2)
      dist(k) = r
      k = k + 1
    end do
  end do

  ! Compute the distance auto-correlation function
  dr = rmax / float(size(corr))
  vol = 1.0
  do i = 1, size(corr)
    c = 0.0
    do j = 1, size(dist)
      if (dist(j) >= float(i-1)*dr .and. dist(j) < float(i)*dr) then
        c = c + 2.0
      end if
    end do
    c = c / (vol * float(size(dist)) * 4.0 * pi * (float(i)*dr)**2 * dr)
    corr(i) = c
  end do

  ! Write the distance auto-correlation function to the output file
  open(unit=20, file=outfile, status='replace', action='write', iostat=ierr)
  if (ierr /= 0) then
    print *, "Error opening output file"
    stop
  else
    out_file_open = .true.
  end if

  do i = 1, size(corr)
    write(20,*) (i-0.5)*dr, corr(i)
  end do

  ! Close the output file
  if (out_file_open) close(20)

  ! Deallocate memory
  deallocate(pos)
  deallocate(dist)
  deallocate(corr)

end program distance_autocorrelation

