# pathtracer
This path-tracer is intended for use in the domain of X-ray astrophysics,
specifically for calculation of observables from neutron star emissions.
It includes geodesic paths for Schwarzschild spacetime.

For viewable images, the `.ppm` format is used.
For scientific data, the portable `.dat` extension is used and
a file management driver (for serializing and deserializing the data) 
provided as part of the program.

## Build
The binary is developed and built using [fpm](https://github.com/fortran-lang/fpm)
(the fortran package manager by 
[fortran-lang](https://github.com/fortran-lang) project).
