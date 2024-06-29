module driver_io
  use camera_mod, only: pixel_type
  implicit none

contains
  !> Subroutine to save the data to a new ppm file
  !> filepath needs the correct extension .ppm included
  !> to be viewable
  subroutine save_to_ppm(filepath, data)
    character(len=*), intent(in) :: filepath
    type(pixel_type), intent(in) :: data(:,:)

    integer :: file_handle
    integer :: i, j, k, image_width, image_height
    ! Obtain the image information from the data shape
    image_width  = size(data,1)
    image_height = size(data,2)

    open(newunit=file_handle, file=filepath)
    ! Set up header
    write(file_handle, '(a)') "P3"
    write(file_handle, '(i3, 1x, i3)') image_width, image_height
    write(file_handle, '(i3)') 255

    ! Write data in lines of 3 values
    ! The dimension of the readout is implicitly known from here
    do k=1,image_height
       do j=1,image_width
          write(file_handle, '(3(i3, 1x))') (data(j,k)%readout(i), i=1,3)
       end do
    end do

    close(file_handle)
    
  end subroutine save_to_ppm

  !> Subroutine to read data from a ppm file
  subroutine read_from_ppm(filepath, data)
    character(len=*), intent(in)  :: filepath
    type(pixel_type), intent(out) :: data(:,:)

    character(len=2) :: strbuffer
    integer :: intbuffer
    integer :: file_handle
    integer :: i, j, k, image_width, image_height

    open(newunit=file_handle, file=filepath, status='old')
    ! Read header and check
    read(file_handle, '(a)') strbuffer
    if (trim(strbuffer) /= "P3") then
       write(*,*) "File does not start with P3"
    end if
    read(file_handle, '(i3, 1x, i3)') image_width, image_height
    read(file_handle, '(i3)') intbuffer
    if (intbuffer /= 255) then
       write(*,*) "File does not use 255"
    end if

    ! Read in the data
    do k=1,image_height
       do j=1,image_width
          read(file_handle, '(3(i3, 1x))') (data(j,k)%readout(i), i=1,3)
       end do
    end do

    close(file_handle)
  end subroutine read_from_ppm

end module driver_io

