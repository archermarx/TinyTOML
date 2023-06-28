# TinyTOML

A small TOML-parsing library written in Modern Fortran. I wrote this instead of using [TOML-f](https://github.com/toml-f/toml-f) because
I needed TOML parsing in my research code and on an HPC cluster and didn't want to take on a large external library.
You should probably use that library instead of this one if you can.

Consists of a single c. 1000 line file.

Does not currently support all TOML features. Missing features include:
 - Nested arrays
 - Nested tables (i.e. [tab1.tab2])
 - Dates and times
 - Binary, Hexadecimal, and octal literals

## Installation

To install, just download the files from this repository and add the two
source files to your fortran source directory. Compile using your build system of choice.

A sample Makefile is provided for building the example code. Clone the repository and run `make`.
To run the example program, type
`./toml.exe test/example.toml`

## Compatibility

Tested with both gfortran and ifort on Linux.

## Usage

Let's say we have a TOML file saved as `example.toml`. Its contents are:

```toml
[options]
string_option = "a string"
array_option = [1.0, 0.1]
float_option = 0.1
bool_option = false
integer_option = 2

[[fruits]]
type="apple"
properties = {mass_g=83.798, color = "red", in_stock = 1}

[[fruits]]
type="orange"
properties = {mass_g=131.293, color = "orange", in_stock = 3}
```

To read this file in our program, we must first define a `toml_object` object.
This is the root note of the parsed TOML file. We can then read toml data from a
file using the `parse_file` function

```fortran
use TinyTOML

type(toml_object):: toml_content

toml_content = parse_file("example.toml)
```

To get a sub-object, such as the `[options]` table, we use the `get` function.
Passing a string retrieves the corresponding key, while passing an integer gives the value at that index.

```fortran
type(toml_object):: options

! Since "options" is the first thing in the file,
! these are all equivalent
options = toml_content%get("options")
options = toml_content%get(1)
options = get(toml_content, "options")
options = get(toml_content, 1)
```

In order to read the value of a TOML object into a Fortran data-type, we use the `read_value` subroutine.
This supports integers, logicals, reals, and arrays thereof, in addition to strings. When reading arrays,
you are expected to pass an `allocatable` variable.

```fortran
real(f64):: float_option
real(f64), allocatable:: array_option(:)

call read_value(float_option, options%get("float_option"))
call read_value(array_option, options%get("array_option"))
```

To dump the contents of a `toml_object` into a string, call the `stringify` function, i.e.

```fortran
print*, options%stringify()
print*, stringify(options) ! this is equivalent
```

A more complete example of usage is available in the `test` directory.

## Contribution

This library is a work-in-progress. Please feel free to open issues and pull requests to improve this repo.


