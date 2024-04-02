## 选择与分支结构
> 这个程序将不断读取输入，直到遇到文件结束。它会根据给定的最终成绩，输出相应的等级，并计算每个等级的学生人数。最后，程序将输出每个等级的人数总结。

```
PROGRAM GradeReport
    IMPLICIT NONE

    CHARACTER(LEN=20) :: Name
    REAL :: CRM, ExmAvg, Final
    INTEGER :: Firsts, UpSeconds, LowSeconds, Thirds, Fails

    Firsts = 0
    UpSeconds = 0
    LowSeconds = 0
    Thirds = 0
    Fails = 0

    ! Read input until end of file
    DO WHILE ( .TRUE. )
        READ(*,*,END=10) Name, CRM, ExmAvg, Final
        IF (Final >= 75) THEN
            PRINT*, Name, CRM, ExmAvg, Final, '1'
            Firsts = Firsts + 1
        ELSE IF (Final >= 70) THEN
            PRINT*, Name, CRM, ExmAvg, Final, '2+'
            UpSeconds = UpSeconds + 1
        ELSE IF (Final >= 60) THEN
            PRINT*, Name, CRM, ExmAvg, Final, '2--'
            LowSeconds = LowSeconds + 1
        ELSE IF (Final >= 50) THEN
            PRINT*, Name, CRM, ExmAvg, Final, '3'
            Thirds = Thirds + 1
        ELSE
            PRINT*, Name, CRM, ExmAvg, Final, 'F'
            Fails = Fails + 1
        END IF
    END DO

    ! Output summary
    PRINT*, 'Number of Firsts: ', Firsts
    PRINT*, 'Number of Upper Seconds: ', UpSeconds
    PRINT*, 'Number of Lower Seconds: ', LowSeconds
    PRINT*, 'Number of Thirds: ', Thirds
    PRINT*, 'Number of Fails: ', Fails

10  CONTINUE

END PROGRAM GradeReport

```

## 嵌套选择

```
Disc = B * B
4 * A * C
Outer
: IF (A /= 0) THEN
Inner
: IF (Disc < 0) THEN
PRINT*, 'Complex roots'
ELSE Inner
X1 = (
B + SQRT( Disc )) / (2 * A)
X2 = (
B SQRT( Disc )) / (2 * A)
END IF Inner
END IF Outer
```
##循环嵌套Do

> 计算利率

```
PROGRAM LoanPayments
    IMPLICIT NONE

    INTEGER :: I ! counter
    INTEGER :: N = 12 ! number of payments per year
    INTEGER :: K ! repayment period (yrs)
    INTEGER :: TRIPS ! iteration count
    REAL :: A = 1000.0 ! principal
    REAL :: P ! payment
    REAL :: R ! interest rate
    REAL :: R0, R1, RINC ! lowest, highest interest and increment

    write(*,*) "请输入：最低利率（R0）、最高利率（R1）和增量（RINC）"
    READ(*,*) R0, R1, RINC

    TRIPS = INT( (R1 - R0) / RINC + RINC/2 ) + 1
    R = R0

    PRINT*, "Rate  15 yrs   20 yrs   25 yrs"
    PRINT*, "--------------------------------"

    DO I = 1, TRIPS
        WRITE( *, '(F5.2, "%")', ADVANCE = 'NO' ) 100 * R
        DO K = 15, 25, 5
            P = R/N * A*(1 + R/N)**(N*K) / ((1 + R/N)**(N*K) - 1)
            WRITE( *, '(F10.2)', ADVANCE = 'NO' ) P
        END DO
        PRINT* ! get a new line
        R = R + RINC
    END DO

END PROGRAM LoanPayments

```
### 结果测试
![image](https://github.com/ytu2023/fortran/assets/145260038/d36e99ab-204f-4239-9528-c993945c3fed)
![image](https://github.com/ytu2023/fortran/assets/145260038/2201a098-9cf8-4e79-ae50-893d6248fb4c)


## 猜数游戏
>

```
PROGRAM NumberGuess
    IMPLICIT NONE

    real :: FtnNum ! Fortran number to guess
    INTEGER :: MyGuess ! User's guess

    ! Generate a random number between 1 and 100
    CALL RANDOM_NUMBER(FtnNum)
    FtnNum = 1 + INT(FtnNum * 100)

    DO
        WRITE( *, '("Your guess: ")', ADVANCE = 'NO' )
        READ*, MyGuess

        IF (MyGuess > FtnNum) THEN
            PRINT*, 'Too high. Try again'
        ELSE IF (MyGuess < FtnNum) THEN
            PRINT*, 'Too low. Try again'
        ELSE
            PRINT*, 'Well done!'
            EXIT
        END IF

        IF (MyGuess == FtnNum) EXIT
    END DO

END PROGRAM NumberGuess


```

## 结果
![image](https://github.com/ytu2023/fortran/assets/145260038/796fff33-e756-4827-bc37-3bf64b9c04c8)



## 用do从文件中读取数值

> DATA.txt文件

```
PROGRAM CalculateMean
    IMPLICIT NONE

    REAL :: A, SUM
    INTEGER :: N = 0
    INTEGER :: IO = 0

    ! Open file 'DATA' for reading
    OPEN(1, FILE='DATA.txt')

    SUM = 0

    ! Read values from file until end of file is reached
    DO WHILE (IO == 0)
        READ(1, *, IOSTAT=IO) A  ! Read a value from file
        IF (IO == 0) THEN
            SUM = SUM + A   ! Add the value to the sum
            N = N + 1       ! Increment the count of values read
            PRINT*, A       ! Print the value read
        END IF
    END DO

    ! Close the file
    CLOSE(1)

    ! Calculate and print the mean
    PRINT*, "Mean:", SUM / N

END PROGRAM CalculateMean

```
## 结果和数据
![image](https://github.com/ytu2023/fortran/assets/145260038/4743f69b-3d63-4f47-9e90-4acfc210b860)


## 泰勒公式SINX与sin（x）
> 该程序计算正弦函数的泰勒级数近似值，并将其与 Fortran 内置的正弦函数进行比较：

```
PROGRAM Taylor
    ! Computes sine(x) from Taylor series
    REAL, PARAMETER :: Pi = 3.14159278
    INTEGER :: K = 1 ! term counter
    INTEGER :: MaxTerms = 10 ! max number of terms
    REAL :: Err = 1e-6 ! max error allowed
    REAL :: Sine ! sum of series
    REAL :: Term ! general term in series
    REAL :: X ! angle in radians

    PRINT*, 'Angle in degrees?'
    READ*, X
    X = X * Pi / 180 ! convert to radians
    Term = X ! first term in series
    Sine = Term

    DO WHILE ((ABS(Term) > Err) .AND. (K <= MaxTerms))
        Term = Term * (-1)**K * X**((2 * K) + 1) / REAL((2 * K) + 1) ! Calculate next term
        K = K + 1 ! Increment term counter
        Sine = Sine + Term ! Add term to sum
    END DO

    IF (ABS(Term) > Err) THEN ! Check if series converged
        PRINT*, 'Series did not converge'
    ELSE
        PRINT*, 'After', K-1, 'terms Taylor series gives', Sine
        PRINT*, 'Fortran 90 intrinsic function: ', SIN(X)
    END IF

END PROGRAM Taylor

```
### 计算结果
![image](https://github.com/ytu2023/fortran/assets/145260038/d141a813-420d-46ed-8afe-3c113e18643e)


## 内部子函数 牛顿法求F(x）=0 
> 

```
PROGRAM Newton
    ! Solves f(x) = 0 by Newton's method
    IMPLICIT NONE
    INTEGER :: Its = 0 ! iteration counter
    INTEGER :: MaxIts = 20 ! maximum iterations
    LOGICAL :: Converged = .FALSE. ! convergence flag
    REAL :: Eps = 1e-6 ! maximum error
    REAL :: X = 2.0 ! starting guess
    
    DO WHILE (.NOT. Converged .AND. Its < MaxIts)
        X = X - F(X) / DF(X) ! Newton's method
        PRINT*, X, F(X)
        Its = Its + 1
        Converged = ABS(F(X)) <= Eps
    END DO
    
    IF (Converged) THEN
        PRINT*, 'Newton converged after', Its, 'iterations'
    ELSE
        PRINT*, 'Newton diverged after', Its, 'iterations'
    END IF
    
CONTAINS

    FUNCTION F(X)
        ! problem is to solve f(x) = 0
        REAL :: F, X
        F = X ** 3 + X
    END FUNCTION F

    FUNCTION DF(X)
        ! first derivative of f(x)
        REAL :: DF, X
        DF = 3 * X ** 2 + 1
    END FUNCTION DF

END PROGRAM Newton

```

### 结果 
![image](https://github.com/ytu2023/fortran/assets/145260038/5b54d03d-c9ef-4536-9c24-7fe0814ccbcc)


##
>

```

```


##
>

```

```


##
>

```

```


##
>

```

```


##
>

```

```


##
>

```

```



