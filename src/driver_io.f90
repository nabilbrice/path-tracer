module driver_io
  implicit none

contains

  !> Subroutine to save the data to a new ppm file
  !> filepath needs the correct extension .ppm included
  !> to be viewable
  subroutine save_to_ppm(filepath, data)
    character(len=*), intent(in) :: filepath
    integer,          intent(in) :: data(:,:,:)

    integer :: file_handle
    integer :: i, j, k, image_width, image_height
    ! Obtain the image information from the data shape
    image_width  = size(data,2)
    image_height = size(data,3)

    open(newunit=file_handle, file=filepath)
    ! Set up header
    write(file_handle, '(a)') "P3"
    write(file_handle, '(i0, 1x, i0)') image_width, image_height
    write(file_handle, '(i0)') 255

    ! Write data in lines of 3 values
    do k=1,image_height
       do j=1,image_width
          write(file_handle, '(3(i0, 1x))') (data(i,j,k), i=1,3)
       end do
    end do

    close(file_handle)
    
  end subroutine save_to_ppm
  
end module driver_io

