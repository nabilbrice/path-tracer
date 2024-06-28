module driver_io
  implicit none

contains

  !> Subroutine to save the data to a new ppm file
  !> filepath needs the correct extension .ppm included
  !> to be viewable
  subroutine save_to_ppm(filepath, data)
    character(len=*), intent(in) :: filepath
    integer,          intent(in) :: data(:,:)

    integer :: file_handle
    integer :: i, j

    open(newunit=file_handle, file=filepath)
    
    ! Set up header
    write(file_handle, '(a)') "P3"
    write(file_handle, '(i0, 1x, i0)') 3, 2
    write(file_handle, '(i0)') 255

    ! Write data in lines of 3 values
    do j=1,size(data,2)
       write(file_handle, '(3(i0, 1x))') (data(i,j), i=1,3)
    end do

    close(file_handle)
    
  end subroutine save_to_ppm
  
end module driver_io

