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
        type(toml_object):: options, more_options, fruits, fruit, properties
        character(len = :), allocatable:: string_option, fruit_name, fruit_color
        integer(i32):: num_array_elements, in_stock
        real(f64), allocatable:: array_option(:), not_present_option(:)
        real(f64):: float_option, mass_g
        integer(i32):: integer_option, another_option
        logical:: bool_option

        options = input%get("options")

        call read_value(options%get("string_option", error = .false.), string_option)
        call read_value(options%get("float_option", error = .false.), float_option)
        call read_value(options%get("integer_option", error = .false.), integer_option)
        call read_value(options%get("array_option", error = .false.), array_option)
        call read_value(options%get("bool_option", error = .false.), bool_option)
        call read_value(&
            options%get("not_present_option", error = .false.), &
            not_present_option, &
            default = (/1.0_f64, 2.0_f64, 3.0_f64/)&
        )

        more_options = input%get("more-options")
        call read_value(more_options%get("another_option"), another_option)

        num_array_elements = size(array_option)

        print*, "String option:", string_option
        print*, "Float option: ", float_option
        print*, "Integer option: ", integer_option
        print*, "Length of array: ", num_array_elements
        print*, "Array option: ", array_option
        print*, "Bool option: ", bool_option
        print*, "Not present option: ", not_present_option

        print*, "Another option: ", another_option

        fruits = input%get("fruits")
        do i = 1, num_children(fruits)
            fruit = get(input%get("fruits"), i)
            properties = fruit%get("properties")
            call read_value(fruit%get("type"), fruit_name)
            call read_value(properties%get("in_stock"), in_stock)
            call read_value(properties%get("color"), fruit_color)
            call read_value(properties%get("mass_g"), mass_g)
            print*, ""
            print*, "Fruit: ", i
            print*, "Name: ", fruit_name
            print*, "Mass (g): ", mass_g
            print*, "Color: ", fruit_color
            print*, "In stock: ", in_stock
        end do

    end block
end program
