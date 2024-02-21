module TinyTOML
    ! TinyTOML v0.2.0
    ! https://github.com/archermarx/TinyTOML
    ! Copyright 2023, Thomas A. Marks, licensed under the MIT license.
    ! The full text of this license can be found in LICENSE.md in the root directory of this repository.
    ! This repository provides utilities for parsing TOML files for input reading
    ! When possible, try upgrading to TOML-f
    ! which is sure to be more robust, but I was unable to get it to install on the cluster
    ! https://github.com/toml-f/toml-f
    !
    ! ================NOT CURRENTLY SUPPORTED =====================
    ! Nested arrays
    ! Nested tables (i.e. tab1.tab2)
    ! Dates and times
    ! Binary, Hexadecimal, and octal literals
    ! Multi-line strings
    use, intrinsic:: iso_fortran_env, only : &
        stderr => error_unit, &
        stdin => input_unit, &
        stdout => output_unit, &
        f32 => real32, &
        f64 => real64, &
        f128 => real128, &
        i8 => int8, &
        i16 => int16, &
        i32 => int32, &
        i64 => int64

    implicit none

    integer(i32), parameter:: ERROR_LENGTH = 64

    ! List of error messages
    character(len = *), dimension(1), parameter:: ERROR_MESSAGES(17) = [character(len = ERROR_LENGTH):: &
        "General error", &
        "No equals sign for key-value pair", &
        "No value provided for key", &
        "No closing brace for inline table", &
        "No closing bracket for array", &
        "No closing bracket for table", &
        "No closing bracket for table array", &
        "Number may only have one decimal point",   &
        "Invalid character in number ", &
        "Invalid underscore placement in number", &
        "Invalid + or - in number", &
        "Invalid entry", &
        "Missing closing double quote in string", &
        "Missing closing single quote in string", &
        "File not found", &
        "Invalid read", &
        "Key not found" &
    ]

    ! List of corresponding error codes
    enum, bind(c)
        enumerator:: SUCCESS = 0
        enumerator:: DEFAULT_ERROR = 1
        enumerator:: NO_EQUALS_SIGN_IN_KEY = 2
        enumerator:: EMPTY_VALUE = 3
        enumerator:: NO_CLOSING_BRACE_INLINE_TABLE = 4
        enumerator:: NO_CLOSING_BRACKET_ARRAY = 5
        enumerator:: NO_CLOSING_BRACKET_TABLE = 6
        enumerator:: NO_CLOSING_BRACKET_TABLE_ARRAY = 7
        enumerator:: ONLY_ONE_DECIMAL_POINT = 8
        enumerator:: INVALID_CHAR_IN_NUMBER = 9
        enumerator:: INVALID_UNDERSCORE_IN_NUMBER = 10
        enumerator:: INVALID_PLUS_OR_MINUS_IN_NUMBER = 11
        enumerator:: INVALID_ENTRY = 12
        enumerator:: MISSING_CLOSING_DOUBLE_QUOTE = 13
        enumerator:: MISSING_CLOSING_SINGLE_QUOTE = 14
        enumerator:: FILE_NOT_FOUND = 15
        enumerator:: INVALID_READ = 16
        enumerator:: KEY_NOT_FOUND = 17
    end enum

    type, abstract:: toml_abstract_object
        character(len = :), allocatable:: key
    end type

    ! Define nodes of parse tree
    ! These are generally key-value pairs, but can
    ! also be things without values (table headers)
    ! or without keys (numbers in an array)
    type, extends(toml_abstract_object):: toml_object
        character(len = :), allocatable:: value
        character(len = :), allocatable:: type
        integer(i32):: line_num
        integer(i32):: error_code
        class(toml_abstract_object), allocatable:: children(:)
    contains
        procedure, public:: stringify
        procedure, public:: get_key
        procedure, public:: get_ind
        generic, public:: get => get_key, get_ind
    end type

    ! Interface for various routines for reading TOML data back into fortran data-types
    interface read_value
        module procedure read_value_bool
        module procedure read_value_string
        module procedure read_value_f32
        module procedure read_value_f64
        module procedure read_value_i32
        module procedure read_value_i64
        module procedure read_array_bool
        module procedure read_array_f32
        module procedure read_array_f64
        module procedure read_array_i32
        module procedure read_array_i64
    end interface

    interface get
        module procedure get_key
        module procedure get_ind
    end interface

    contains

!=================String manipulation functions===============================
    pure logical function isdigit(d)
        character(len = 1), intent(in):: d
        isdigit = d >= "0" .and. d <= "9"
    end function

    pure integer(i32) function findfirst(needle, haystack)
        character(len = *), intent(in):: haystack
        character(len = 1), intent(in):: needle
        integer(i32):: i

        do i = 1, len(haystack)
            if (haystack(i:i) == needle) then
                findfirst = i
                return
            elseif (i == len(haystack)) then
                findfirst = 0
            endif
        end do
    end function

    pure function strip(str) result(s)
        ! Strip leading and trailing whitespace from input string str
        character(len = *), intent(in):: str
        character(len = :), allocatable:: s
        s = trim(adjustl(str))
    end function

    pure integer(i32) function count(str, ch)
        ! Count occurances of character ch in string str
        character(len = *), intent(in):: str
        character(len = 1), intent(in):: ch
        integer(i32):: i

        count = 0
        do i = 1, len(str)
            if (str(i:i) == ch) count = count + 1
        end do

    end function

    function split(str, dlm) result(split_str)
        ! Split input string str into several substrings on occurances of
        ! a comma. Padded spaces are also removed.
        character(len = *), intent(in):: str
        character(len = 1), intent(in):: dlm
        character(len = 64), allocatable, dimension(1):: split_str(:)
        integer(i32):: length, num_substrings, i, ind, dlm_ind

        ! Count how many times delimiter occurs in string
        num_substrings = count(str, ",") + 1

        ! Get length of string
        length = len(str)

        ! Allocate result array
        allocate(split_str(num_substrings))

        ! Use internal list-directed io to read comma-separated values
        !read(str, *) split_str_tmp
        dlm_ind = 1
        ind = 1
        do i = 1, length
            if (str(i:i) == dlm) then
                split_str(ind) = str(dlm_ind:i-1)
                dlm_ind = i + 1
                ind = ind + 1
            endif
        end do

        if (dlm_ind <= length) then
            split_str(num_substrings) = str(dlm_ind:length)
        else
            split_str(num_substrings) = ""
        endif

    end function

    integer(i32) function num_lines(file)
        character(len = *):: file
        integer(i32):: io, unit
        num_lines = 0
        open (newunit = unit, file = file, iostat = io)
        do
            read(unit,*,iostat=io)
            if (io /= 0) exit
            num_lines = num_lines + 1
        end do
        close (unit)
    end function

!=================End string manipulation functions===========================

!=================Error-handling routines=====================================
    subroutine toml_error(message, code)
        character(len = *), intent(in):: message
        integer(i32), intent(in):: code
        write(stderr, '(A)') "TOML ERROR: " //  message // "."
        call exit(code)
    end subroutine

    subroutine key_not_found_error(target_key, node, context)
        character(len = *), intent(in):: target_key
        type(toml_object), intent(in):: node
        character(len = *), optional:: context
        character(len = :), allocatable:: message
        message = "Key '" // target_key // "' not found in object"
        if (node%key /= "") message = message // " '" // node%key // "'"
        if (present(context)) message = message // " " // context
        call toml_error(message, KEY_NOT_FOUND)
    end subroutine

    subroutine invalid_read_error(type, node)
        character(len = *), intent(in):: type
        type(toml_object), intent(in):: node
        call toml_error("Tried to read '"//node%key//"' of type "//node%type//" as a "// type, INVALID_READ)
    end subroutine

    subroutine parse_error(error_code, line)
        integer(i32), intent(in):: error_code, line
        character(len = 256):: line_str
        write(line_str, '(g0)') line
        call toml_error(strip(ERROR_MESSAGES(error_code)) // " (line " // strip(line_str) // ")", error_code)
    end subroutine
!=================End error-handling routines=================================

!=================Routines for manipulating toml_object objects=================
    pure integer(i32) function num_children(node)
        type(toml_object), intent(in):: node
        num_children = size(node%children)
    end function

    logical function use_default(node, default_present)
        type(toml_object), intent(in):: node
        logical, intent(in):: default_present

        use_default = .false.
        if (node%error_code == KEY_NOT_FOUND) then
            if (default_present) then
                use_default = .true.
            else
                call key_not_found_error(node%value, node, "and no default provided")
            endif
        endif
    end function

    subroutine read_value_bool(node, val, default)
        logical, intent(out):: val
        logical, intent(in), optional:: default
        type(toml_object), intent(in):: node

        if (use_default(node, present(default))) then
            val = default; return
        endif

        select case(node%type)
        case("bool")
            if (node%value == "true") val = .true.
            if (node%value == "false") val = .false.
        case default
            call invalid_read_error("boolean", node)
        end select
    end subroutine

    subroutine read_value_string(node, val, default)
        character(len = :), allocatable, intent(out):: val
        character(len = *), optional, intent(in):: default
        type(toml_object), intent(in):: node

        if (use_default(node, present(default))) then
            val = default; return
        endif

        val = node%value
    end subroutine

    subroutine read_value_f32(node, val, default)
        real(f32), intent(out):: val
        real(f32), intent(in), optional:: default
        type(toml_object), intent(in):: node

        if (use_default(node, present(default))) then
            val = default; return
        endif

        select case(node%type)
        case("int", "float")
            read(node%value, *) val
        case default
            call invalid_read_error("32-bit float", node)
        end select
    end subroutine

    subroutine read_value_f64(node, val, default)
        real(f64), intent(out):: val
        real(f64), intent(in), optional:: default
        type(toml_object), intent(in):: node

        if (use_default(node, present(default))) then
            val = default; return
        endif

        select case(node%type)
        case("int", "float")
            read(node%value, *) val
        case default
            call invalid_read_error("64-bit float", node)
        end select
    end subroutine

    subroutine read_value_i32(node, val, default)
        integer(i32), intent(out):: val
        integer(i32), intent(in), optional:: default
        type(toml_object), intent(in):: node

        if (use_default(node, present(default))) then
            val = default; return
        endif

        select case(node%type)
        case("int")
            read(node%value, *) val
        case default
            call invalid_read_error("32-bit integer", node)
        end select
    end subroutine

    subroutine read_value_i64(node, val, default)
        integer(i64), intent(out):: val
        integer(i64), intent(in), optional:: default
        type(toml_object), intent(in):: node

        if (use_default(node, present(default))) then
            val = default; return
        endif

        select case(node%type)
        case("int")
            read(node%value, *) val
        case default
            call invalid_read_error("64-bit integer", node)
        end select
    end subroutine

    subroutine read_array_bool(node, val, default)
        logical, allocatable, intent(out):: val(:)
        logical, optional, intent(in):: default(:)
        type(toml_object):: node
        integer(i32):: i, n

        if (use_default(node, present(default))) then
            val = default; return
        endif
        n = num_children(node)

        allocate(val(n))
        do i = 1, n
            call read_value(node%get(i), val(i))
        end do
    end subroutine

    subroutine read_array_i32(node, val, default)
        integer(i32), allocatable, intent(out):: val(:)
        integer(i32), optional, intent(in):: default(:)
        type(toml_object):: node
        integer(i32):: i, n

        if (use_default(node, present(default))) then
            val = default; return
        endif
        n = num_children(node)

        allocate(val(n))
        do i = 1, n
            call read_value(node%get(i), val(i))
        end do
    end subroutine

    subroutine read_array_i64(node, val, default)
        integer(i64), allocatable, intent(out):: val(:)
        integer(i64), optional, intent(in):: default(:)
        type(toml_object):: node
        integer(i32):: i, n

        if (use_default(node, present(default))) then
            val = default; return
        endif
        n = num_children(node)

        allocate(val(n))
        do i = 1, n
            call read_value(node%get(i), val(i))
        end do
    end subroutine

    subroutine read_array_f32(node, val, default)
        real(f32), allocatable, intent(out):: val(:)
        real(f32), optional, intent(in):: default(:)
        type(toml_object):: node
        integer(i32):: i, n

        if (use_default(node, present(default))) then
            val = default; return
        endif
        n = num_children(node)

        allocate(val(n))
        do i = 1, n
            call read_value(node%get(i), val(i))
        end do
    end subroutine

    subroutine read_array_f64(node, val, default)
        real(f64), allocatable, intent(out):: val(:)
        real(f64), optional, intent(in):: default(:)
        type(toml_object):: node
        integer(i32):: i, n

        if (use_default(node, present(default))) then
            val = default; return
        endif
        n = num_children(node)

        allocate(val(n))
        do i = 1, n
            call read_value(node%get(i), val(i))
        end do
    end subroutine

    pure function find_key(nodes, key) result(index)
        class(toml_abstract_object), intent(in):: nodes(:)
        character(len = *), intent(in):: key
        integer(i32):: index, i
        index = 0
        do i = 1, size(nodes)
            if (nodes(i)%key == key) then
                index = i
                return
            endif
        end do
    end function

    pure function insert_child(children, new_child) result(new_children)
        class(toml_abstract_object), intent(in), allocatable:: children(:)
        type(toml_object), intent(in):: new_child
        type(toml_object), allocatable:: new_children(:)
        integer(i32):: i, n_children

        if (.not. allocated(children)) then
            n_children = 1
        else
            n_children = size(children) + 1
        endif

        allocate(new_children(n_children))

        do i = 1, n_children - 1
            select type (child => children(i))
            type is (toml_object)
                new_children(i) = child
            end select
        end do

        new_children(n_children) = new_child
    end function

    recursive logical function has_key(node, key) result(result)
        class(toml_object), intent(in):: node
        character(len = *), intent(in):: key
        character(len = :), allocatable:: subkey, remainder
        integer(i32):: index, first_dot_index
        type(toml_object):: gotten

        ! Check for subkeys
        do first_dot_index = 1, len(key)
            if (key(first_dot_index:first_dot_index) == ".") exit
        end do

        subkey = key(1:first_dot_index-1)
        if (subkey == key) then
            remainder = ""
        else
            remainder = key(first_dot_index+1:len(key))
        endif

        index = find_key(node%children, subkey)

        result = .false.

        if (index > 0) then
            result = .true.
            return
        endif

        gotten = node%get(index)

        if (remainder /= "") then
            result = result .or. has_key(gotten, strip(remainder))
        endif
    end function

    recursive function get_key(node, key, error) result(gotten)
        class(toml_object), intent(in):: node
        character(len = *), intent(in):: key
        character(len = :), allocatable:: subkey, remainder
        integer(i32):: index, first_dot_index
        type(toml_object):: gotten
        logical, optional:: error
        logical:: throw_error

        if (present(error)) then
            throw_error = error
        else
            throw_error = .true.
        endif

        ! Check for subkeys
        do first_dot_index = 1, len(key)
            if (key(first_dot_index:first_dot_index) == ".") exit
        end do

        subkey = key(1:first_dot_index-1)
        if (subkey == key) then
            remainder = ""
        else
            remainder = key(first_dot_index+1:len(key))
        endif

        index = find_key(node%children, subkey)

        if (index == 0 .and. throw_error) call key_not_found_error(subkey, node)

        gotten = node%get(index)

        if (gotten%error_code == KEY_NOT_FOUND) then
            gotten%key = node%key
            gotten%value = subkey
        endif

        if (remainder /= "") then
            gotten = gotten%get(strip(remainder))
        endif
    end function

    pure function get_ind(node, index) result(gotten)
        class(toml_object), intent(in):: node
        integer, intent(in):: index
        type(toml_object):: gotten

        if (index <= 0 .or. index > num_children(node)) then
            gotten%key = ""
            gotten%value = ""
            gotten%type = ""
            gotten%error_code = KEY_NOT_FOUND
            gotten%children = no_children()
        else
            select type (found => node%children(index))
                type is (toml_object); gotten = found
            end select
        endif
    end function

    ! Instructions for converting a node into a string
    recursive pure function stringify(node) result(str)
        class(toml_object), intent(in):: node
        character(len = :), allocatable:: str, val_str
        character(len = 1):: left_bracket, right_bracket
        integer(i32):: i, nchildren

        nchildren = size(node%children)

        str = ""

        select case (node%type)
        case("table", "array")
            if (node%key /= "") str = node%key // ": "
            if (node%type == "table") then
                left_bracket = "{"
                right_bracket = "}"
            else
                left_bracket = "["
                right_bracket = "]"
            endif
            str = str // left_bracket
            do i = 1, nchildren
                str = str // stringify(node%get(i))
                if (i < nchildren) str = str // ", "
            end do
            str = str // right_bracket
        case default
            val_str = node%value
            if (node%type == "string") val_str = '"' // val_str // '"'
            if (node%key == "") then
                str = val_str // "::" // node%type
            else
                str = node%key // "::" // node%type // " = " // val_str
            endif

        end select
    end function

    pure function no_children() result(children)
        type(toml_object), allocatable:: children(:)
        allocate(children(0))
    end function
!=================End poutines for manipulating toml_object objects=============

!=================Parsing routines============================================
    function parse_file(file) result(parse_tree)
        character(len = *), intent(in):: file
        type(toml_object):: parse_tree
        logical:: exists

        inquire(file = file, exist = exists)

        if (exists) then
            parse_tree%key = ""
            parse_tree%value = ""
            parse_tree%type = "table"
            parse_tree%children = parse_tokens(tokenize(file))
        else
            write(stdout, *) "ERROR: File "// strip(file) // " not found."
            call exit(2)
        endif
    end function

    function tokenize(file) result(tokens)
        character(len = *), intent(in):: file
        type(toml_object), allocatable:: tokens(:)
        character(len = 256):: ln
        character(len = :), allocatable:: key, val, typ, line
        type(toml_object):: pair

        integer(i32):: io, unit, nlines, i, ind, comment_ind, error_code
        nlines = num_lines(file)

        allocate(tokens(nlines))

        if (nlines == 0) then
            return
        endif

        open (newunit = unit, file = file, iostat = io)

        ind = 0
        do i = 1, nlines
            ! Read a line from the file
            read(unit, '(A)', iostat = io) ln

            ! Find first occurance of a pound sign (comment) in the line
            comment_ind = findfirst("#", ln)
            if (comment_ind == 0) then
                ! Line has no comma. Just trim whitespace from line
                line = strip(ln)
            elseif (comment_ind == 1) then
                ! Line starts with a comma. The entire line is thus a comma and can be skipped
                cycle
            else
                line = strip(ln(1:comment_ind-1))
            endif

            ! Try parsing as key-value pair
            pair = parse_key_value_pair(line)
            error_code = pair%error_code

            if (error_code == SUCCESS) then
                ind = ind + 1
                tokens(ind) = pair
                tokens(ind)%line_num = i

            elseif (error_code == NO_EQUALS_SIGN_IN_KEY) then
                key = line
                val = ""

                ! Check for table header
                if (key(1:1) /= "[") then
                    ! Check for blank line
                    if (len(key) == 0 .and. ind > 0) then
                        ! Don't bother recording multiple blank lines
                        ! Blank lines with comments don't count
                        if (tokens(ind)%type == "blank" .or. comment_ind > 0) cycle
                        typ = "blank"
                    else
                        error_code = INVALID_ENTRY
                    endif
                else
                    if (key(1:2) == "[[") then
                        if (key(len(key)-1:len(key)) /= "]]") then
                            error_code = NO_CLOSING_BRACKET_TABLE_ARRAY
                        else
                            typ = "tablearray"
                            ! Strip brackets
                            key = key(3:len(key)-2)
                        endif
                    else
                        if (key(len(key):len(key)) /= "]") then
                            error_code = NO_CLOSING_BRACKET_TABLE
                        else
                            typ = "table"
                            ! Strip brackets
                            key = key(2:len(key)-1)
                        endif
                    endif
                endif

                if (typ /= "unknown") error_code = SUCCESS

                if (error_code == SUCCESS) then
                    ind = ind + 1
                    tokens(ind)%key = strip(key)
                    tokens(ind)%value = strip(val)
                    tokens(ind)%type = typ
                    tokens(ind)%line_num = i
                endif
            endif

            if (ERROR_CODE /= success) then
                call parse_error(ERROR_CODE, i)
            endif
        end do

        close(unit)

        tokens = tokens(1:ind)

    end function

    recursive function parse_tokens(tokens) result(nodes)
        type(toml_object):: tokens(:)
        type(toml_object), allocatable:: nodes(:)
        type(toml_object):: node
        integer(i32):: num_tokens, i, n_children, ind
        num_tokens = size(tokens, 1)
        allocate(nodes(num_tokens))

        ind = 0
        i = 1
        do while (i <= num_tokens)
            select case(tokens(i)%type)
            case("blank")
                i = i + 1
            case("array")
                ind = ind + 1
                nodes(ind)%key = tokens(i)%key
                nodes(ind)%type = tokens(i)%type
                nodes(ind)%value = tokens(i)%value
                nodes(ind)%children = parse_array_content(tokens(i)%value, tokens(i)%line_num)
                i = i + 1
            case("inline_table")
                ind = ind + 1
                nodes(ind)%key = tokens(i)%key
                nodes(ind)%type = "table"
                nodes(ind)%value = ""
                nodes(ind)%children = parse_tokens(&
                    parse_key_value_pairs(  &
                        tokens(i)%value(2:len(tokens(i)%value)-1), &
                        tokens(i)%line_num  &
                    )   &
                )
                i = i + 1
            case("table", "tablearray")
                ! Token is either a table or a table array
                ! So we need to count how many of the following lines
                ! belong under the heading, and then figure out what to do from there
                node%key = tokens(i)%key
                node%type = tokens(i)%type
                node%value = ""
                n_children = 0
                do
                    ! Blank lines separate tables
                    if (tokens(i + n_children)%type == "blank") exit
                    if (i + n_children == num_tokens) exit
                    n_children = n_children + 1
                end do
                ! Recursively parse tokens of this table
                node%children = parse_tokens(tokens(i+1:i+n_children))

                if (node%type == "tablearray") then
                    ! If we have a table array, we need to figure out if we've seen this
                    ! table array before
                    ! If so, insert the contents in that table array
                    ! If not, create a new array containing only this table
                    node%type = "table"
                    block
                        integer(i32):: tabarray_ind
                        tabarray_ind = find_key(nodes, node%key)

                        if (tabarray_ind > 0) then
                            node%key = ""
                            nodes(tabarray_ind)%children = insert_child(nodes(tabarray_ind)%children, node)
                        else
                            ind = ind + 1
                            nodes(ind)%key = node%key
                            nodes(ind)%type = "array"
                            node%key = ""
                            nodes(ind)%value = ""
                            nodes(ind)%children = insert_child(nodes(ind)%children, node)
                        endif
                    end block
                else
                    ind = ind + 1
                    nodes(ind) = node
                endif
                i = i + n_children + 1
            case default
                ! Token is a plain old key-value pair
                ! with no possible children
                ind = ind + 1
                nodes(ind)%key = tokens(i)%key
                nodes(ind)%value = tokens(i)%value
                nodes(ind)%type = tokens(i)%type
                nodes(ind)%children = no_children()
                i = i + 1
            end select
        end do

        nodes = nodes(1:ind)

    end function

    function parse_key_value_pairs(str, line_num) result(pairs)
        ! Parse comma-separated key-value pairs
        character(len = *), intent(in):: str
        integer(i32), intent(in), optional:: line_num
        type(toml_object), allocatable:: pairs(:)
        type(toml_object), allocatable:: result
        character(len = :), allocatable, dimension(1):: split_str(:)
        integer(i32):: num_pairs, i, location

        if (present(line_num)) then
            location = line_num
        else
            location = -1
        endif

        split_str = split(str, ",")
        num_pairs = size(split_str)
        ! Ignore trailing commas
        if (strip(split_str(num_pairs)) == "") then
            num_pairs = num_pairs - 1
        endif

        allocate(pairs(num_pairs))

        do i = 1, num_pairs
            result = parse_key_value_pair(strip(split_str(i)))
            if (result%error_code == SUCCESS) then
                pairs(i) = result
            else
                call parse_error(result%error_code, location)
            endif
        end do

    end function

    function parse_key_value_pair(str, line_num) result(pair)
        character(len = *), intent(in):: str
        integer(i32), intent(in), optional:: line_num
        integer(i32):: equals_ind

        character(len = :), allocatable:: key, val
        type(toml_object):: pair
        type(toml_object):: parse_result

        ! Find first occurance of an equals sign in the line
        equals_ind = findfirst("=", str)

        if (equals_ind == 0) then
            ! No key-value pair found
            pair%error_code = NO_EQUALS_SIGN_IN_KEY
            pair%key = ""
            pair%value = ""
            pair%type = "unknown"
            return
        endif

        ! This line has a key-value pair
        key = strip(str(1:equals_ind-1))
        val = strip(str(equals_ind+1:len(str)))

        parse_result = parse_value(val)
        pair%error_code = parse_result%error_code
        pair%type = parse_result%type
        pair%key = key
        pair%value = parse_result%value

        if (present(line_num)) then
            pair%line_num = line_num
        else
            pair%line_num = -1
        endif

    end function

    pure function clean_number(str) result(num)
        ! Make a number readable as 64-bit by Fortran by removing underscores and
        ! replacing e or E with d and D
        character(len = *), intent(in):: str
        character(len = :), allocatable:: num
        character(len = 1):: next
        integer(i32):: i

        num = ""
        do i = 1, len(str)
            next = str(i:i)
            if (next == "e" .or. next == "E") then
                num = num // "d"
            elseif (next /= "_" .and. next /= "+") then
                num = num // next
            endif
        end do
    end function

    function parse_value(value) result(parse_result)
        character(len = *), intent(in):: value
        type(toml_object):: parse_result
        character(len = :), allocatable:: typ, val
        integer(i32):: error_code

        val = strip(value)

        typ = "unknown"
        error_code = SUCCESS

        ! Check if value is a bool
        if (val == "") then
            error_code = EMPTY_VALUE
        elseif (val == "true" .or. val == "false") then
            typ = "bool"
        ! Check if value is an inf or nan
        elseif (val == "inf" .or. val == "+inf" .or. val == "-inf" .or. &
                val == "nan" .or. val == "+nan" .or. val == "-nan") then
            typ = "float"
            val = clean_number(val)

        ! Check if value starts with an integer part
        elseif (&
            (len(val) >= 2 .and. (val(1:1) == "+" .or. val(1:1) == "-") .and. isdigit(val(2:2))) .or. &
            (len(val) >= 1 .and. isdigit(val(1:1)))) then

            parse_result = parse_number(val)
            typ = parse_result%type
            val = parse_result%value
            error_code = parse_result%error_code

        ! Check if value is an inline table
        elseif (val(1:1) == "{") then

            if (val(len(val):len(val)) .ne. "}") then
                error_code = NO_CLOSING_BRACE_INLINE_TABLE
            else
                typ = "inline_table"
            endif

        ! Check if value is a double-quoted string
        elseif (val(1:1) == '"') then
            if (val(len(val):len(val)) .ne. '"') then
                error_code = MISSING_CLOSING_DOUBLE_QUOTE
            else
                typ = "string"
                val = val(2:len(val)-1)
            endif
        ! Check if value is a single-quoted string
        elseif (val(1:1) == "'") then
            if (val(len(val):len(val)) .ne. "'") then
                error_code = MISSING_CLOSING_SINGLE_QUOTE
            else
                typ = "string"
                val = val(2:len(val)-1)
            endif
        ! Check if value is an array
        elseif (val(1:1) == "[") then
            if (val(len(val):len(val)) .ne. "]") then
                error_code = NO_CLOSING_BRACKET_ARRAY
            else
                typ = "array"
            endif
        endif

        ! Return our result
        parse_result%key = ""
        parse_result%value = val
        parse_result%type = typ
        parse_result%error_code = error_code

    end function

    pure function parse_number(str) result(parse_result)
        character(len = *), intent(in):: str
        type(toml_object):: parse_result
        character(len = :), allocatable:: typ
        character(len = 1), allocatable:: next
        logical:: decimal_point_found, exponent_found, underscore_allowed, plusminus_allowed
        integer(i32):: l, ind, error_code

        decimal_point_found = .false.
        exponent_found = .false.
        underscore_allowed = .false.
        plusminus_allowed = .true.

        l = len(str)

        typ = "unknown"
        error_code = SUCCESS

        ind = 0
        do while (ind+1 <= l)
            ind = ind + 1
            next = str(ind:ind)
            if (next == ".") then
                if (decimal_point_found) then
                    error_code = ONLY_ONE_DECIMAL_POINT
                    exit
                else
                    decimal_point_found = .true.
                    cycle
                endif
            elseif (next == "e" .or. next == "E") then
                if (exponent_found .or. ind == l) then
                    error_code = INVALID_CHAR_IN_NUMBER
                    exit
                else
                    exponent_found = .true.
                    plusminus_allowed = .true.
                endif
            elseif (next == "_") then
                if (underscore_allowed) then
                    underscore_allowed = .false.
                else
                    error_code = INVALID_UNDERSCORE_IN_NUMBER
                    exit
                endif
            elseif (next == "+" .or. next == "-") then
                if (.not. plusminus_allowed) then
                    error_code = INVALID_PLUS_OR_MINUS_IN_NUMBER
                    exit
                endif
                plusminus_allowed = .false.
            elseif (.not. isdigit(next)) then
                error_code = INVALID_CHAR_IN_NUMBER
                exit
            else
                if (plusminus_allowed) plusminus_allowed = .false.
                if (.not. underscore_allowed) underscore_allowed = .true.
                if (ind == l - 1) underscore_allowed = .false.
            endif
        end do

        parse_result%key = ""
        parse_result%error_code = error_code

        if (error_code == SUCCESS) then
            if  (decimal_point_found .or. exponent_found) then
                typ = "float"
            else
                typ = "int"
            endif
            parse_result%value = clean_number(str)
        else
            parse_result%value = str
        endif

        parse_result%type = typ

    end function

    function parse_array_content(str, line_num) result(nodes)
        character(len = *), intent(in):: str
        integer(i32), intent(in):: line_num
        type(toml_object), allocatable:: nodes(:)
        type(toml_object):: child
        type(toml_object):: result
        character(len = :), allocatable:: array_content, element
        character(len = :), allocatable, dimension(1):: array_elements(:)
        integer(i32):: num_elements, i

        array_content = str(2:len(str)-1)
        array_elements = split(array_content, ",")

        num_elements = count(array_content, ",") + 1
        allocate(nodes(num_elements))

        do i = 1, num_elements
            element = strip(array_elements(i))
            result = parse_value(element)
            if (result%error_code == SUCCESS) then
                child%key = ""
                child%value = result%value
                child%type = result%type
                child%children = no_children()
                nodes(i) = child
            else
                call parse_error(result%error_code, line_num)
            endif
        end do

    end function
!=================End parsing routines=========================================
end module
