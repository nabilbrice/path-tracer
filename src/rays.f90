module rays_mod
  use, intrinsic :: iso_fortran_env, only: r64 => real64
  use constants_mod, only: pi
  implicit none

  type :: ray_type
     real(r64), dimension(3) :: origin
     real(r64), dimension(3) :: direction
   contains
     procedure :: new => new_ray
     procedure :: get_position
     procedure :: intersect_sphere
  end type ray_type

  ! Base type for hittables to inherit
  ! So they can be placed into an array
  type, abstract :: hittable_type
  end type hittable_type

  type, extends(hittable_type) :: sphere_type
     real(r64), dimension(3) :: centre
     real(r64)               :: radius
     ! This should generally be a texture map when possible
     integer, dimension(3) :: colour
   contains
     procedure :: new => new_sphere
     procedure :: get_normal
     procedure :: get_surface_coord
  end type sphere_type

contains
  subroutine new_ray(ray, origin, direction)
    class(ray_type), intent(inout) :: ray
    real(r64), dimension(3), intent(in)  :: origin, direction

    ray%origin = origin
    ray%direction = direction

  end subroutine new_ray

  function get_position(ray, param) result(position)
    class(ray_type), intent(in) :: ray
    real(r64),       intent(in) :: param
    real(r64), dimension(3)     :: position

    position = ray%origin + param * ray%direction

  end function get_position

  function intersect_sphere(ray, sphere) result(param)
    class(ray_type),      intent(in) :: ray
    type(sphere_type),    intent(in) :: sphere

    real(r64) :: param

    real(r64) :: b, c, discrm
    real(r64), dimension(3) :: dir_to_centre

    dir_to_centre = ray%origin - sphere%centre
    b = 2.0*dot_product(dir_to_centre,ray%direction)
    c = dot_product(dir_to_centre,dir_to_centre) - sphere%radius**2

    discrm = b**2 - 4.0*c
    param = -1.0 ! Set as guard
    if (discrm > 0.0) then
       discrm = sqrt(discrm)
       param = -(b + discrm)/2.0
       ! If this is still negative, update
       if (param < 0.0) then
          param = param + discrm
       end if
       ! If the update is still negative
       ! then the function returns a negative value
    end if

  end function intersect_sphere

  subroutine new_sphere(sphere, centre, radius)
    class(sphere_type), intent(inout) :: sphere
    real(r64), dimension(3), intent(in) :: centre
    real(r64), intent(in) :: radius

    sphere%centre = centre
    sphere%radius = radius
    sphere%colour = [255, 0, 0]

  end subroutine new_sphere

  function get_normal(sphere, location) result(normal)
    class(sphere_type),      intent(in)  :: sphere
    real(r64), dimension(3), intent(in)  :: location
    real(r64), dimension(3)              :: normal

    normal = (location - sphere%centre) / sphere%radius

  end function get_normal

  !> Transformation from the global (camera) coordinate system to
  !> local surface chart point (normalised)
  !> The location vector needs to already be normalised
  !> u * pi = theta = arccos(z)
  !> v * 2*pi = phi = arctan(y/x)
  function get_surface_coord(sphere, location) result(surface_coord)
    class(sphere_type),      intent(in) :: sphere
    real(r64), dimension(3), intent(in) :: location
    real(r64), dimension(2) :: surface_coord

    ! u = theta / pi      ; runs from 0 to 1
    surface_coord(1) = acos(location(3)) / pi
    ! v = phi / 2 pi + 0.5; runs from 0 to 1
    surface_coord(2) = atan(location(2),location(1)) / 2 / pi + 0.5

  end function get_surface_coord

end module rays_mod
