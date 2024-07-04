module rays_mod
  use, intrinsic :: iso_fortran_env, only: r64 => real64
  use constants_mod, only: pi, gr_compactness
  use vectors, only: normalise
  implicit none

  !> This derived data type encapsulates commonly used args
  type :: ray_type
     real(r64), dimension(3) :: origin
     real(r64), dimension(3) :: direction
  end type ray_type

  ! Base type for hittables to inherit
  ! So they can be placed into an array
  type, abstract :: hittable_type
  end type hittable_type

  type, extends(hittable_type) :: sphere_type
     real(r64), dimension(3) :: centre = [0_r64, 0_r64, 0_r64]
     real(r64)               :: radius = 1.0_r64
     ! This should generally be a texture map when possible
     integer, dimension(3) :: colour = [255, 0, 0]
     real(r64), dimension(3,3) :: orientation = &
          reshape([1.0_r64, 0.0_r64, 0.0_r64, &
           0.0_r64, 1.0_r64, 0.0_r64, &
           0.0_r64, 0.0_r64, 1.0_r64], [3,3])
  end type sphere_type

contains
  !> The get position function assumes a certain (straight line) path
  function get_position(ray, param) result(position)
    type(ray_type),  intent(in) :: ray
    real(r64),       intent(in) :: param
    real(r64), dimension(3)     :: position

    position = ray%origin + param * ray%direction

  end function get_position

  function intersect_sphere(ray, sphere) result(param)
    type(ray_type),    intent(in) :: ray
    type(sphere_type), intent(in) :: sphere

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

  function gr_intersect_sphere(ray, sphere) result(gr_param)
    type(ray_type),    intent(in) :: ray
    type(sphere_type), intent(in) :: sphere

    real(r64) :: b, cos_angle, gr_param
    ! impact parameter b
    b = norm2( &
         ray%origin - (dot_product(ray%origin, ray%direction) * ray%direction))
    gr_param = -1.1_r64 ! Set as guard, not just any negative value now
    ! Outside of this impact parameter, there is no intersection
    ! with the surface
    if (b < sphere%radius / sqrt(gr_compactness)) then
       ! The cos of the angle of incidence with the surface
       cos_angle = sqrt(1.0_r64 - b**2 * gr_compactness / sphere%radius**2)
       ! The cos of the deflection angle
       ! This being greater than -1.0 is how it is known to be valid
       gr_param = 1.0_r64 - (1.0_r64 - cos_angle) / gr_compactness
    end if

  end function gr_intersect_sphere

  function gr_get_position(ray, param) result(position)
    type(ray_type), intent(in) :: ray
    real(r64),      intent(in) :: param

    real(r64), dimension(3) :: normal, position

    ! The check is necessary to avoid a NaN surfacing in the sqrt
    if (param < 1.0) then
       normal = normalise( &
            ray%origin - dot_product(ray%origin, ray%direction) * ray%direction)

       position = - param * ray%direction &
            + sqrt(1.0_r64 - param**2) * normal
    else
       ! The safe value when the param = 1.0
       position = - ray%direction
    end if
    
  end function gr_get_position
    
  subroutine new_sphere(sphere, centre, radius, theta, phi)
    type(sphere_type), intent(inout) :: sphere
    real(r64), dimension(3), intent(in) :: centre
    real(r64), intent(in) :: radius
    real(r64), optional, intent(in) :: theta, phi

    sphere%centre = centre
    sphere%radius = radius

    ! The surface transforms opposite to the rotation
    ! First, transform about the y-axis, i.e.
    ! rotate in longitude
    if (present(phi)) then
       sphere%orientation = reshape([real(r64) ::  &
            cos(phi),        0, sin(phi),         &
                   0,        1,        0,         &
           -sin(phi),        0, cos(phi)        ],&
       [3,3] )
    end if
    ! Then transform about the x-axis, i.e.
    ! latitude inclination
    if (present(theta)) then
       sphere%orientation = matmul(reshape([real(r64) ::  &
            1,           0,           0,           &
            0,  cos(theta),  sin(theta),           &
            0, -sin(theta),  cos(theta)          ],&
       [3,3]), sphere%orientation )
    end if

  end subroutine new_sphere

  function get_normal(sphere, location) result(normal)
    type(sphere_type),       intent(in)  :: sphere
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
    type(sphere_type),       intent(in) :: sphere
    real(r64), dimension(3), intent(in) :: location

    real(r64), dimension(2) :: surface_coord

    real(r64), dimension(3) :: local_coord

    local_coord = matmul(sphere%orientation, location)

    ! u = theta / pi      ; runs from 0 to 1
    surface_coord(1) = acos(local_coord(3)) / pi
    ! v = phi / 2 pi + 0.5; runs from 0 to 1
    surface_coord(2) = atan2(local_coord(2),local_coord(1)) / 2 / pi + 0.5

  end function get_surface_coord

end module rays_mod
