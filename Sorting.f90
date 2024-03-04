! Compare the time of two sorting algorithms

PROGRAM sorting
        ! Declares variables
        INTEGER, PARAMETER :: arraySize = 1000
        INTEGER :: I, NBR = 1000
        REAL :: array(arraySize)
        REAL first, second, resu 
       
        I = 0
        call cpu_time(first) 
                do while(I < NBR)
                        CALL resetArray(array)
                        CALL bubbleSort(array)
                        I = I + 1
                end do
        call cpu_time(second)
        resu = second - first
        write(*,*) 'Time for bubble sort',NBR,'times :',resu,'for arrays with a size of', arraySize,'elements'

        I = 0
        call cpu_time(first)
                do while(I < NBR)
                         CALL resetArray(array)
                         CALL insertSort(array)
                         I = I + 1
                end do                         
        call cpu_time(second)
        resu = second - first
        write(*,*) 'Time for insert sort',NBR,'times :',resu,'for arrays with a size of',arraysize,'elements'
END PROGRAM

SUBROUTINE resetArray(array)
        REAL, DIMENSION(n) :: array
        INTEGER :: I 
        I = 1
        ! Sets all the values in array to random real numbers between 0 and 1
        CALL RANDOM_NUMBER(HARVEST = array)
        ! Functions like a while statement in java
        do while(I<SIZE(array)+1)
                array(I) = array(I)*100
                ! CEILING: Rounds of to nearest smaller integer
                array(I) = CEILING(array(I))
                I=I+1
        end do 
END SUBROUTINE

SUBROUTINE bubbleSort(array)
        REAL, DIMENSION(n) :: array
        INTEGER :: I, swap, J
        I = 1
        do while(I < SIZE(array))
                J = SIZE(array) 
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

SUBROUTINE insertSort(array)
        REAL, DIMENSION(n) :: array
        INTEGER :: I, swap, J
        I = 2
        do while(I < SIZE(array)+1) 
                J = I
                do while(array(J)<array(J-1)) 
                        swap = array(J)
                        array(J) = array(J-1)
                        array(J-1) = swap
                        J = J - 1
                end do
                I = I + 1
        end do
END SUBROUTINE
