program test
    use TinyTOML

    implicit none

    integer(i32):: status, i
    character(len = 32):: filename

    ! Values to try and fill
    type(toml_object):: input

    call get_command_argument(1, filename, status = status)

    if (status /= 0) then
        write(stderr, *) "Must supply filename as first argument."
        call exit(2)
    endif

    input = parse_file(filename)

    do i = 1, num_children(input)
        print*, stringify(input%get(i))
    end do

    block
        type(toml_object):: options, fruits, fruit, properties
        character(len = :), allocatable:: string_option, fruit_name, fruit_color
        integer(i32):: num_array_elements, in_stock
        real(f64), allocatable:: array_option(:)
        real(f64):: float_option, mass_g
        integer(i32):: integer_option
        logical:: bool_option

        options = input%get("options")

        call read_value(string_option, options%get("string_option"))
        call read_value(float_option, options%get("float_option"))
        call read_value(integer_option, options%get("integer_option"))
        call read_value(array_option, options%get("array_option"))
        call read_value(bool_option, options%get("bool_option"))

        num_array_elements = size(array_option)

        print*, "String option:", string_option
        print*, "Float option: ", float_option
        print*, "Integer option: ", integer_option
        print*, "Length of array: ", num_array_elements
        print*, "Array option: ", array_option
        print*, "Bool option: ", bool_option

        fruits = input%get("fruits")
        do i = 1, num_children(fruits)
            fruit = get(input%get("fruits"), i)
            properties = fruit%get("properties")
            call read_value(fruit_name, fruit%get("type"))
            call read_value(in_stock, properties%get("in_stock"))
            call read_value(fruit_color, properties%get("color"))
            call read_value(mass_g, properties%get("mass_g"))
            print*, ""
            print*, "Fruit: ", i
            print*, "Name: ", fruit_name
            print*, "Mass (g): ", mass_g
            print*, "Color: ", fruit_color
            print*, "In stock: ", in_stock
        end do

    end block
end program