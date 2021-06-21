! MIT License
!
! Copyright (c) 2021 Georg Schnabel
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.

program dtwdiff
implicit none
    integer*2 :: numsecs1, numsecs2, mf, mt
    integer*2, dimension(1000,2) :: mfmt_list1, mfmt_list2
    integer*8, dimension(1000,2) :: linerange1, linerange2
    integer :: i, j, k

    character(len=1024) :: filename1, filename2

    integer, dimension(:,:), allocatable :: movelist
    integer :: movenr

    character(len=11), dimension(:), allocatable :: els1, els2

    if (command_argument_count() /= 2) then
        write(*,*) 'ERROR: Two filenames as command line arguments required.'
        stop
    end if

    call get_command_argument(1, filename1)
    call get_command_argument(2, filename2)

    open(unit = 10, file=filename1, status='old')
    open(unit = 20, file=filename2, status='old')

    call locate_mfmt(10, numsecs1, mfmt_list1, linerange1)
    call locate_mfmt(20, numsecs2, mfmt_list2, linerange2)

    ! walk through the sections
    i = 1; j = 1
    do while (i <= numsecs1 .or. j <= numsecs2)
        if (mfmt_identical(mfmt_list1, mfmt_list2, i, j)) then
            write(*,'(A50)') '##################################################'
            write(*,'(A25,I4,A6,I4,A11)') '#   COMMON SECTION: MF = ', mfmt_list1(i,1), ' MT = ', mfmt_list1(i,2), '         #' 
            write(*,'(A50)') '##################################################'

            ! do the comparison of the section
            mf = mfmt_list1(i,1)
            mt = mfmt_list1(i,2)

            call get_els(10, mf, mt, linerange1(i, 2), els1)
            call get_els(20, mf, mt, linerange2(j, 2), els2)

            if (size(els1) < 100000 .and. size(els2) < 100000) then
                    call DTWDistance(els1, els2, movenr, movelist)
                    call printDifferences(els1, els2, linerange1(i,1), linerange2(j,1), mf, mt, movenr, movelist)
            else
                print *, 'too large section for detailed comparison', &
                    '(size #1: ', size(els1), ' size #2: ', size(els2), ')'
            end if

            deallocate(els1)
            deallocate(els2)

            i = i + 1
            j = j + 1
        else if (mfmt_smaller(mfmt_list1, mfmt_list2, i, j)) then
            write(*,'(A50)') '##################################################'
            write(*,'(A25,I4,A6,I4,A11)') '#   ONLY IN #1:     MF = ', mfmt_list1(i,1), ' MT = ', mfmt_list1(i,2), '         #' 
            write(*,'(A50)') '##################################################'
            i = i + 1
            if (i > numsecs1) then
                do k=j,numsecs2
                    write(*,'(A50)') '##################################################'
                    write(*,'(A25,I4,A6,I4,A11)') '#   ONLY IN #2:     MF = ', mfmt_list2(j,1), &
                        ' MT = ', mfmt_list2(j,2), '         #' 
                    write(*,'(A50)') '##################################################'
                end do
                exit
            end if
        else
            write(*,'(A50)') '##################################################'
            write(*,'(A25,I4,A6,I4,A11)') '#   ONLY IN #2:     MF = ', mfmt_list2(j,1), ' MT = ', mfmt_list2(j,2), '         #' 
            write(*,'(A50)') '##################################################'
            j = j + 1
            if (j > numsecs2) then
                do k=j,numsecs1
                    write(*,'(A50)') '##################################################'
                    write(*,'(A25,I4,A6,I4,A11)') '#   ONLY IN #1:     MF = ', mfmt_list1(i,1), ' MT = ', mfmt_list1(i,2), &
                        '         #' 
                    write(*,'(A50)') '##################################################'
                end do
                exit
            end if
        end if
    end do    


contains

    subroutine get_els(u, mf, mt, numrows, elarray)
        implicit none
        integer, intent(in) :: u
        integer*8 :: i, numrows
        integer*2, intent(in) :: mf, mt
        integer*2 :: cmf, cmt
        character(len=11), dimension(:), intent(out), allocatable :: elarray
        character(len=11), dimension(6) :: curfields
        logical :: is_reading
        allocate(elarray(numrows*6))
        ! read the elements
        is_reading = .false.
        i = 0
        do while (.true.)
            read (u, '(6A11,4X,I2,I3)') curfields, cmf, cmt
            if (cmf == mf .and. cmt == mt) then
                i = i + 1
                if (.not. is_reading) then
                    is_reading = .true.
                end if
                elarray((i-1)*6+1:i*6) = curfields
            end if 
            if ((cmf /= mf .or. cmt /= mt) .and. is_reading) then
                if (i /= numrows) then
                    print *, 'faulty program!'
                end if
                exit
            end if 
        end do
    end subroutine


    function mfmt_smaller(mfmt_list1, mfmt_list2, n1, n2)
        implicit none
        integer, intent(in) :: n1, n2
        integer*2, dimension(1000,2), intent(in) :: mfmt_list1, mfmt_list2
        logical :: mfmt_smaller

        if (mfmt_list1(n1,1) < mfmt_list2(n2,1)) then
            mfmt_smaller = .true.
        else if (mfmt_list1(n1,1) > mfmt_list2(n2,1)) then
            mfmt_smaller = .false.
        else if (mfmt_list1(n1,2) < mfmt_list2(n2,2)) then
            mfmt_smaller = .true.
        else 
            mfmt_smaller = .false.
        end if
    end function


    function mfmt_identical(mfmt_list1, mfmt_list2, n1, n2)
        implicit none
        integer, intent(in) :: n1, n2
        integer*2, dimension(1000,2), intent(in) :: mfmt_list1, mfmt_list2
        logical :: mfmt_identical
        if (mfmt_list1(n1,1) == mfmt_list2(n2,1) .and. &
            mfmt_list1(n1,2) == mfmt_list2(n2,2)) then
            mfmt_identical = .true.
        else
            mfmt_identical = .false.
        end if
    end function


    subroutine locate_mfmt(u, numsecs, mfmt_list, linerange)
        implicit none
        integer, intent(in) :: u
        integer*2, intent(out) :: numsecs
        integer*2, dimension(1000,2), intent(out) :: mfmt_list
        integer*8, dimension(1000,2), intent(out) :: linerange

        character(len=11), dimension(6) :: fields

        integer :: linenr, startlinenr 
        integer*8 :: numrows 
        integer*2 :: az, mf, mt, lastmf, lastmt

        mfmt_list(:,:) = 0
        linerange(:,:) = 0

        lastmf = 0
        lastmt = 0
        numsecs = 0
        linenr = 0
        rewind(u)
        do while (1 == 1)
            read(u, '(6A11,I4,I2,I3)', END=200) fields, az, mf, mt
            numrows = numrows + 1
            linenr = linenr + 1
            if (lastmt /= mt) then
                if (lastmt /= 0) then
                    numsecs = numsecs + int(1,2)
                    mfmt_list(numsecs,1) = lastmf
                    mfmt_list(numsecs,2) = lastmt
                    linerange(numsecs,1) = startlinenr
                    linerange(numsecs,2) = numrows
                end if

                if (mt /= 0) then
                    startlinenr = linenr
                end if
                numrows = 0
            end if
            lastmf = mf
            lastmt = mt
        end do
        200 continue
        rewind(u)
    end subroutine


    function check_if_number(str, val)
        character(*), intent(in) :: str
        real :: val
        logical :: check_if_number
        logical :: seen_nonwhite1, seen_nonwhite2, seen_whitespace
        integer :: errflag, i

        seen_nonwhite1 = .false.
        seen_nonwhite2 = .false.
        seen_whitespace = .false.

        do i=1,len(str)
            if (str(i:i) /= ' ') then
                if (.not. seen_nonwhite1) then
                    seen_nonwhite1 = .true.
                else if (seen_whitespace) then
                    seen_nonwhite2 = .true.
                end if
            else if (seen_nonwhite1) then
                seen_whitespace = .true.
            end if
        end do

        if (seen_whitespace .and. seen_nonwhite2) then
            check_if_number = .false.
        else
            read(str,*,iostat=errflag) val
            check_if_number = (errflag == 0)
        end if

    end function


    function strdist(s1, s2, is_real1, realval1, is_real2, realval2)
        implicit none
        character(*), intent(in):: s1, s2
        logical, intent(in) :: is_real1, is_real2
        real, intent(in) :: realval1, realval2
        real :: reldiff

        real :: strdist

        if (s1 /= s2) then
            if (is_real1 .and. is_real2) then
                if (real(int(realval1+1e-5)) == realval1 .and. &
                    real(int(realval2+1e-5)) == realval2) then
                    ! both are integer
                    if (abs(realval1 - realval2) < 1e-5) then
                        ! and identical
                        strdist = 0
                        !print *, 'check: two integers, identical ', realval1, realval2
                    else 
                        ! not identical
                        strdist = 1
                        !print *, 'check: two integers, not identical  ', realval1, realval2
                    end if 
                else
                    ! they are not integer
                    if (abs(realval1 - realval2) < 1e-5) then
                        ! they are identical
                        strdist = 0
                        !print *, 'check: two reals, identical', realval1, realval2
                    else
                        reldiff = abs(realval1 - realval2) / max(abs(realval1), abs(realval2))
                        strdist = min(reldiff * 3., 1.)
                        !print *, 'check: two reals, relative difference', realval1, realval2
                    end if
                end if
            else
                ! they are just strings and not identical
                strdist = 1
            end if
        else 
            ! they are identical
            strdist = 0
        end if
    end function


    function strdist_slow(s1, s2)
        character(*), intent(in) :: s1, s2
        real :: realval1, realval2
        logical :: is_real1, is_real2
        real :: strdist_slow

        is_real1 = check_if_number(s1, realval1)
        is_real2 = check_if_number(s2, realval2)
        strdist_slow = strdist(s1, s2, is_real1, realval1, is_real2, realval2)
    end function


    subroutine printDifferences(s, t, ofs1, ofs2, mf, mt, movenr, movelist)
        ! variables for printing
        integer, dimension(:,:), intent(in), allocatable :: movelist
        character(len=11), dimension(:), intent(in), allocatable :: s, t
        integer, intent(in) :: movenr
        integer*8, intent(in) :: ofs1, ofs2
        integer*2, intent(in) :: mf, mt

        integer :: m, n, i 
        integer :: last_printed_i, last_printed_j, num_printed1, num_printed2
        logical :: print_first_column

        character(len=11) :: curv1, curv2
        integer :: curi, curj, lasti, lastj, sk
        logical, dimension(:), allocatable :: is_ins, is_del, is_apx1, is_apx2
        logical :: is_line_complete, any_changes

        ! print *, 'warping distance: ', DTW(n+1,m+1)
        n = size(s)
        m = size(t)
        allocate(is_del(n))
        allocate(is_ins(m))
        is_ins(:) = .false.
        is_del(:) = .false.

        allocate(is_apx1(n))
        allocate(is_apx2(m))
        is_apx1(:) = .false.
        is_apx2(:) = .false.

        is_line_complete = .false.

        lasti=0
        lastj=0
        last_printed_i = 0
        last_printed_j = 0

        do i=1, movenr
            curi = movelist(i, 1)
            curj = movelist(i, 2)
            curv1 = s(curi)
            curv2 = t(curj)

            ! output mode
            if (curi == lasti+1 .and. curj == lastj+1) then
                if (curv1 == curv2) then
                    !write(*, '(A2,A11,A2)', advance='no') '  ', curv1, '  '
                else
                    is_del(curi) = .true.
                    is_ins(curj) = .true.

                    if (strdist_slow(curv1, curv2) < 0.1) then
                        is_apx1(curi) = .true.
                        is_apx2(curj) = .true.
                    end if
                    !write(*, '(A2,A11,A2,A2,A11,A2)', advance='no') '{+', curv2, '+}', '[-', curv1, '-]'
                end if
                if (modulo(curj,6) == 0) then
                    is_line_complete = .true.
                    !write(*,'()')
                end if
            else
                if (curi == lasti+1 .and. curj == lastj) then
                    is_del(curi) = .true.
                    !write(*, '(A2,A11,A2)', advance='no') '[-', curv1, '-]'
                else
                    is_ins(curj) = .true.
                    !write(*, '(A2,A11,A2)', advance='no') '{+', curv2, '+}'
                    if (modulo(curj,6) == 0) then
                        is_line_complete = .true.
                    !    write(*,'()')
                    end if
                end if
            end if

            if (is_line_complete) then
                ! check if there are any changes, i.e., insertions and deletions
                ! in the current line
                any_changes = .false.
                do sk=last_printed_i,curi
                    if (is_del(sk)) then
                        any_changes = .true.
                        exit
                    end if
                end do
                if (.not. any_changes) then
                    do sk=last_printed_j,curj
                        if (is_ins(sk)) then
                            any_changes = .true.
                            exit
                        end if
                    end do
                end if

                ! print the line
                is_line_complete = .false.
                num_printed1 = 0
                num_printed2 = 0
                print_first_column = .true.
                do while (last_printed_i < curi .or. last_printed_j < curj &
                         .or. modulo(num_printed1, 6) /= 0 .or. modulo(num_printed2, 6) /= 0)

                    if (print_first_column) then

                        if (modulo(num_printed1,6)==0) then
                            sk = int(last_printed_i / 6 + 1.) + ofs1 - 1
                            if (any_changes) then
                                write(*,'(A2,I6,A3)', advance='no') '!|',sk, '| ' 
                            else
                                write(*,'(A2,I6,A3)', advance='no') ' |',sk, '| ' 
                            end if
                        end if

                        if (last_printed_i == curi) then
                            !write(*, '(A2,A11,A2)', advance='no') '--', '-----------', '--'
                            write(*, '(A11)', advance='no') '-----------'
                        else
                            if (modulo(last_printed_i,6) /= modulo(num_printed1,6)) then
                                write(*, '(A11)', advance='no') '-----------'
                            else
                                last_printed_i = last_printed_i + 1
                                curv1 = s(last_printed_i)
                                if (is_del(last_printed_i)) then
                                    !write(*, '(A2,A11,A2)', advance='no') '[-', curv1, '-]'
                                    if (.not. is_apx1(last_printed_i)) then
                                        write(*, '(A1,A4,A11,A1,A3)', advance='no') achar(27), '[31m', curv1, achar(27), '[0m'
                                    else
                                        write(*, '(A1,A4,A11,A1,A3)', advance='no') achar(27), '[34m', curv1, achar(27), '[0m'
                                    end if
                                else
                                    !write(*, '(A2,A11,A2)', advance='no') '  ', curv1, '  '
                                    write(*, '(A11)', advance='no') curv1
                                end if
                            end if
                        end if

                        num_printed1 = num_printed1 + 1
                        if (modulo(num_printed1, 6) == 0) then
                            print_first_column = .false.
                        end if
                    end if

                    if (.not. print_first_column) then

                        if (modulo(num_printed2,6)==0) then
                            sk = int(last_printed_j / 6 + 1.) + ofs2 - 1
                            write(*,'(A3,I6,A3)', advance='no') '  |',sk, '| '
                        end if

                        if (last_printed_j == curj) then
                            !write(*, '(A2,A11,A2)', advance='no') '--', '-----------', '--'
                            write(*, '(A11)', advance='no') '-----------'
                        else
                            last_printed_j = last_printed_j + 1
                            curv2 = t(last_printed_j)
                            if (is_ins(last_printed_j)) then
                                !write(*, '(A2,A11,A2)', advance='no') '{+', curv2, '+}'
                                if (.not. is_apx2(last_printed_j)) then
                                    write(*, '(A1,A4,A11,A1,A3)', advance='no') achar(27), '[32m', curv2, achar(27), '[0m'
                                else
                                    write(*, '(A1,A4,A11,A1,A3)', advance='no') achar(27), '[34m', curv2, achar(27), '[0m'
                                end if
                            else
                                !write(*, '(A2,A11,A2)', advance='no') '  ', curv2, '  '
                                write(*, '(A11)', advance='no') curv2
                            end if
                        end if
                        num_printed2 = num_printed2 + 1
                        if (modulo(num_printed2, 6) == 0) then
                            print_first_column = .true.
                            write(*,'(A4,I2,A3,I3,A2)') '   | ', mf, ' | ', mt, ' |'
                        end if
                    end if

                end do
            end if

            lasti = curi
            lastj = curj
        end do

        deallocate(is_ins)
        deallocate(is_del)
        deallocate(is_apx1)
        deallocate(is_apx2)
    end subroutine



    subroutine DTWDistance(s, t, movenr, movelist)
        character(len=11), dimension(:), intent(in), allocatable :: s, t
        integer, dimension(:,:), intent(out), allocatable :: movelist
        integer, intent(out) :: movenr

        integer, dimension(:,:), allocatable :: chunkmovelist
        real, dimension(:,:), allocatable :: DTW
        real :: cost, mincost
        integer :: curmovenr
        integer :: i, j, k, iofs, jofs, last_iofs, last_jofs
        integer :: n, m, nmax, mmax
        logical :: should_exit_loop

        ! to speed up checks if numeric value
        logical, dimension(:), allocatable :: is_real1, is_real2
        real, dimension(:), allocatable :: realvals1, realvals2

        integer :: maxlen, minjump
        parameter (maxlen=6000, minjump=2000)

        n = size(s)
        m = size(t)

        allocate(movelist(n+m+1,2))
        movelist(:,:) = 0

        allocate(is_real1(n))
        allocate(is_real2(m))
        allocate(realvals1(n))
        allocate(realvals2(m))

        do i=1,n
            is_real1(i) = check_if_number(s(i), realvals1(i))
        end do
        do j=1,m
            is_real2(j) = check_if_number(t(j), realvals2(j))
        end do

        nmax = min(maxlen, n)
        mmax = min(maxlen, m)
        allocate(DTW(nmax+1,mmax+1))
        allocate(chunkmovelist(nmax+mmax+1,2))

        iofs = 0
        jofs = 0
        movenr = 0

        should_exit_loop = .false.
        do ! break condition at end of loop
            !print *, iofs, jofs
            nmax = min(maxlen, n-iofs)
            mmax = min(maxlen, m-jofs)
            if (n-iofs <= maxlen .and. m-jofs <= maxlen) then
                should_exit_loop = .true.
            end if

            DTW(:,:) = huge(int(1,2))
            DTW(1,1) = 0

            if (nmax > 0 .and. mmax > 0) then

                do i=1,nmax
                    do j=1,mmax
                        cost = strdist(s(i+iofs), t(j+jofs), &
                            is_real1(i+iofs), realvals1(i+iofs), &
                            is_real2(j+jofs), realvals2(j+jofs))

                        DTW(i+1, j+1) = cost + min(DTW(i,j+1), DTW(i+1,j),DTW(i,j))
                    end do
                end do

                last_iofs = iofs
                last_jofs = jofs
                if (should_exit_loop) then
                    iofs = n
                    jofs = m
                else
                    ! partially processed: determine average cost per character
                    mincost = huge(int(1,2))
                    do i=max(1,minjump-mmax),nmax
                        do j=max(1,minjump-i), mmax
                            cost = DTW(i+1,j+1) / real(i+j)
                            if (cost < mincost) then
                                mincost = cost
                                iofs = last_iofs + i
                                jofs = last_jofs + j
                            end if
                        end do
                    end do
                end if

                ! reconstruct warping path 
                i = iofs - last_iofs+1; j = jofs - last_jofs+1
                curmovenr = 0
                do while (i > 1 .and. j > 1)
                    mincost = min(DTW(i-1,j),DTW(i,j-1),DTW(i-1,j-1))
                    if (mincost == DTW(i-1,j-1)) then
                        i = i - 1
                        j = j - 1
                    else if (mincost == DTW(i-1,j)) then
                        i = i - 1
                    else ! if (mincost == DTW(i,j-1)) then
                        j = j - 1
                    end if

                    curmovenr = curmovenr + 1
                    if (i+last_iofs <= n) then
                        chunkmovelist(curmovenr, 1) = i + last_iofs
                    else
                        chunkmovelist(curmovenr, 1) = n
                    end if
                    if (j+last_jofs <= m) then
                        chunkmovelist(curmovenr, 2) = j + last_jofs
                    else
                        chunkmovelist(curmovenr, 2) = m
                    end if

                end do
                ! reverse and insert current movelist chunk
                do k=1, curmovenr
                    movelist(movenr + k, 1) = chunkmovelist(curmovenr-k+1, 1)
                    movelist(movenr + k, 2) = chunkmovelist(curmovenr-k+1, 2)
                end do
                movenr = movenr + curmovenr

            else ! nmax == 0 or mmax == 0
                if (nmax == 0) then
                    do j=jofs+1,m
                        movenr = movenr + 1
                        movelist(movenr, 1) = iofs
                        movelist(movenr, 2) = j
                    end do
                else
                    do i=iofs+1,n
                        movenr = movenr + 1
                        movelist(movenr, 1) = i
                        movelist(movenr, 2) = jofs
                    end do
                end if
                exit
            end if

            if (should_exit_loop) then
                exit
            end if
        end do

        deallocate(DTW)
        deallocate(chunkmovelist)

        deallocate(is_real1)
        deallocate(is_real2)
        deallocate(realvals1)
        deallocate(realvals2)
    end subroutine

end program
