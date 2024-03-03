! Compare the time of two sorting algorithms

PROGRAM sorting
        ! Declares variables
        REAL, DIMENSION(10) :: array
        INTEGER :: arraySize, C, R, M, I, NBR
        INTEGER PRE (8), POST (8), RES (8)
        CHARACTER (LEN = 10) BIG_BEN (3)

        NBR = 100000
        arraySize = 10

        ! creates array
        I = 0
        ! Mesaures time for before sorting
        call measureTime(PRE,BIG_BEN)
                do while(I < NBR)
                        CALL resetArray(array)
                        CALL bubbleSort(array)
                        I = I + 1
                end do
        call measureTime(POST,BIG_BEN)
        call postcalc(POST,PRE,array,NBR)

        I = 0
        call measureTime(PRE,BIG_BEN)
                do while(I < NBR)
                         CALL resetArray(array)
                         CALL insertSort(array)
                         I = I + 1
                end do                         
        call measureTime(POST,BIG_BEN)
        call postcalc(POST,PRE,array,NBR)

END PROGRAM

SUBROUTINE measureTime(T,BIG_BEN)
        CHARACTER (LEN = 10) BIG_BEN (3)
        INTEGER T(8) 
        CALL DATE_AND_TIME (BIG_BEN (1), BIG_BEN (2), BIG_BEN (3), T)
END SUBROUTINE

SUBROUTINE resetArray(array)
        REAL, DIMENSION(10) :: array
        INTEGER :: I, array_size
        I = 1
        array_size = SIZE(array) 
        ! Sets all the values in array to random real numbers between 0 and 1
        CALL RANDOM_NUMBER(HARVEST = array)
        ! Functions like a while statement in java
        do while(I<array_size+1)
                array(I) = array(I)*100
                ! CEILING: Rounds of to nearest smaller integer
                array(I) = CEILING(array(I))
                I=I+1
        end do 
END SUBROUTINE

SUBROUTINE bubbleSort(array)
        REAL, DIMENSION(10) :: array
        INTEGER :: I, swap, array_size, J
        array_size = SIZE(array) 
        I = 1
        do while(I < array_size)
                J = array_size
                do while(J>I)
                        if(array(J)<array(J-1)) then
                                swap = array(J)
                                array(J) = array(J-1)
                                array(J-1) = swap
                        end if
                        J = J - 1
                end do
                I = I+1
        end do
END SUBROUTINE

SUBROUTINE postcalc(POST, PRE, array, NBR)
        INTEGER PRE(8), POST(8), RES(8)
        INTEGER :: NBR
        REAL, DIMENSION(10) :: array
        RES = POST - PRE
        write(*,*) 'Sorted using insert sort ', NBR, ' times' 
        write(*,*) 'Time to sort ', RES 
        write(*,*) array
END SUBROUTINE

SUBROUTINE insertSort(array)
        REAL, DIMENSION(10) :: array
        INTEGER :: i, j, array_size, swap
        i = 2
        array_size = SIZE(array)
        do while(i < array_size+1) 
                j = i
                do while(array(j)<array(j-1)) 
                        swap = array(j)
                        array(j) = array(j-1)
                        array(j-1) = swap
                        j = j - 1
                end do
                i = i + 1
        end do
END SUBROUTINE
