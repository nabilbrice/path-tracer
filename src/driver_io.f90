module io_mod
  use, intrinsic :: iso_fortran_env, only: i32 => int32
  use pixels_mod, only: pixel_type
  implicit none

contains
  !> Subroutine to save the data to a new ppm file
  !> filepath needs the correct extension .ppm included
  !> to be viewable.
  !> The header information is inferred from the dimensions
  !> of the data type
  subroutine save_to_ppm(filepath, data)
    character(len=*), intent(in) :: filepath
    type(pixel_type), intent(in) :: data(:,:)

    integer(i32) :: file_handle
    integer(i32) :: i, j, k, image_width, image_height
    ! Obtain the image information from the data shape
    image_width  = size(data,1)
    image_height = size(data,2)

    open(newunit=file_handle, file=filepath)
    ! Set up header
    write(file_handle, '(a)') "P3"
    write(file_handle, '(i3, 1x, i3)') image_width, image_height
    write(file_handle, '(i3)') 255

    ! Write data in lines of 3 values
    ! The dimension of the spectrum is implicitly known from here
    do k=1,image_height
       do j=1,image_width
          write(file_handle, '(3(i3, 1x))') (data(j,k)%spectrum(i), i=1,3)
       end do
    end do

    close(file_handle)
    
  end subroutine save_to_ppm

  !> Subroutine to read data from a ppm file directly
  !> into an image buffer
  subroutine read_from_ppm(filepath, data)
    character(len=*), intent(in)  :: filepath
    type(pixel_type), intent(out) :: data(:,:)

    character(len=2) :: strbuffer
    integer(i32) :: intbuffer
    integer(i32) :: file_handle
    integer(i32) :: i, k, image_width, image_height

    open(newunit=file_handle, file=filepath, status='old')
    ! Read header and check
    read(file_handle, '(a)') strbuffer
    if (trim(strbuffer) /= "P3") then
       write(*,*) "File does not start with P3"
    end if
    read(file_handle, *) image_width, image_height
    read(file_handle, '(i3)') intbuffer
    if (intbuffer /= 255) then
       write(*,*) "File does not use 255"
    end if

    ! Read in the data, flexible format
    do k=1,image_height
       read(file_handle, *) data(:,k)
    end do

    close(file_handle)
  end subroutine read_from_ppm

  !> Function to read data from a ppm file directly
  !> into an allocatable image buffer (not held by a camera)
  function load_from_ppm(filepath) result(buffer)
    character(len=*), intent(in)  :: filepath
    type(pixel_type), allocatable :: buffer(:,:)

    character(len=2) :: strbuffer
    integer(i32) :: intbuffer
    integer(i32) :: file_handle, stat
    integer(i32) :: j, k, image_width, image_height

    open(newunit=file_handle, file=filepath, status='old')
    ! Read the header
    read(file_handle, '(a)') strbuffer
    if (trim(strbuffer) /= "P3") then
       write(*,*) "File does not start with P3"
    end if
    read(file_handle, *) image_width, image_height
    allocate(buffer(image_width, image_height), stat=stat)
    read(file_handle, '(i3)') intbuffer
    if (intbuffer /= 255) then
       write(*,*) "File does not use 255"
    end if

    ! Read in the data, flexible format
    do k=1,image_height
       read(file_handle, *) buffer(:,k)
    end do

    close(file_handle)

  end function load_from_ppm

  !> Function to read data from a dat file directly
  !> into an allocatable image buffer (not held by a camera)
  function load_from_dat(filepath) result(buffer)
    character(len=*), intent(in)  :: filepath
    type(pixel_type), allocatable :: buffer(:,:)

    integer(i32) :: file_handle, stat
    integer(i32) :: i, j, k, image_width, image_height

    open(newunit=file_handle, file=filepath, status='old')
    ! Read the header (standard to the dat files used here)
    read(file_handle, *) image_width, image_height
    ! Allocate size based on the specified width and height
    allocate(buffer(image_width, image_height), stat=stat)
    
    ! Read in the data
    do k=1,image_height
       read(file_handle, *) (buffer(j,k)%spectrum(:), j=1,image_width)
    end do

    close(file_handle)
  end function load_from_dat

  !> Subroutine to save data from an image buffer
  !> into a dat file, formatted with the conventions.
  subroutine save_to_dat(filepath, image)
    character(len=*), intent(in) :: filepath
    type(pixel_type), intent(in) :: image(:,:)

    integer(i32) :: file_handle
    integer(i32) :: i, j, k, image_width, image_height

    image_width = size(image,1)
    image_height = size(image,2)

    open(newunit=file_handle, file=filepath)
    ! Write the dimensions of the image as header
    write(file_handle, '(i3,1x,i3)') image_width, image_height

    ! Write the data
    do k=1,image_height
       do j=1,image_width
          write(file_handle, '(3(i3,1x))') &
               (image(j,k)%spectrum(i), i=1,3)
       end do
    end do

    close(file_handle)

  end subroutine save_to_dat

  !> Subroutine to save data from an image buffer
  !> into data array binary (.dab) file, using unformatted I/O.
  subroutine save_to_dab(filepath, image)
    character(len=*), intent(in) :: filepath
    type(pixel_type), intent(in) :: image(:,:)

    integer(i32) :: file_handle
    integer(i32) :: image_width, image_height

    image_width = size(image,1)
    image_height = size(image,2)

    open(newunit=file_handle, file=filepath, &
         form="unformatted")

    ! Unformatted write of the image size first
    write(file_handle) image_width
    write(file_handle) image_height
    write(file_handle) image

    close(file_handle)

  end subroutine save_to_dab

  !> Function to load data from a dab file directly
  !> into an allocatable image bufer (not held by the camera)
  function load_from_dab(filepath) result(buffer)
    character(len=*), intent(in)  :: filepath
    type(pixel_type), allocatable :: buffer(:,:)

    integer(i32) :: file_handle
    integer(i32) :: image_width, image_height

    open(newunit=file_handle, file=filepath, &
         form="unformatted", status="old")

    ! Read in the image size first,
    ! in the same manner as the storage
    read(file_handle) image_width
    read(file_handle) image_height
    ! Now allocate a buffer of the appropriate size
    allocate(buffer(image_width, image_height))
    ! The data can now be read into the buffer
    read(file_handle) buffer

    close(file_handle)

  end function load_from_dab

end module io_mod
