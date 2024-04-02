## 第二次课

## 第一个程序
> 这是文件读取，在文件的当前目录下，PPT19页
> 
```
program tt
REAL A,B,C
OPEN( 1, FILE = 'DATA1' )
OPEN( 2, FILE = 'DATA2' )
READ(1, 5) A, B, C
WRITE(2,5) A, B, C
5 FORMAT(1X,3F12.5)
END

```

## 文件输入输出2 指定文件位置PPT20

> 
```
program main
implicit none
character(len=100):: fileDir,fileName,fileSuffix
real(kind=8):: a
fileDir = 'C:Users air Desktop \\' ! 比如桌面
fileName = '03013‘,
fileSuffix = '.txt'
open(111,file = ADJUSTL(trim(fileDir))//&&adjustl(trim(fileName))//adjustl(trim(fileSuffix)))
read(111,*) a !读桌面文件 03013.dat 的内容赋值给变量 a
write(*,*) a
end

```
## 用牛顿方法计算一个数的平方
> 让我感受到了算法之美
牛顿法的知识点补充：
https://mp.weixin.qq.com/s/9skfmNa3CADhuoT2blUbGA



```

PROGRAM Newton
! Square rooting with Newton
IMPLICIT NONE
REAL A ! number to be square rooted
INTEGER I ! iteration counter
REAL X ! approximate square root of A
WRITE( *, 10, ADVANCE = 'NO' ) 'Enter number to be square rooted: '
10 FORMAT( A )
READ(*,*) A
X = 1 ! initial guess (why not?)
DO I = 1, 6
X = (X + A / X) / 2
write(*,*) X
ENDDO
Write(*,*)
Write(*,*) 'Fortran 90''s value:', SQRT( A )
END


```


## 期末成绩
## 逻辑
![image](https://github.com/ytu2023/fortran/assets/145260038/3fa5783b-a28b-4325-b1d4-625976516818)


```
PROGRAM Final_Mark
! Final mark for course based on class record and exams
IMPLICIT NONE
REAL CRM ! Class record mark
REAL ExmAvg ! average of two exam papers
REAL Final ! final mark
REAL P1 ! mark for first paper
REAL P2 ! mark for second paper
INTEGER Stu ! student counter
! file wire in
CHARACTER (Len = 15) Name ! Name
OPEN( 1, FILE = 'MARKS.txt' )
write(*,*) ' CRM Exam Avg Final Mark'
write(*,*)
DO Stu = 1, 3
    READ( 1, * ) CRM, P1, P2 READ( 1, * ) Name, CRM, P1, P2
    ExmAvg = (P1 + P2) / 2.0
    IF (ExmAvg > CRM) THEN
    Final = ExmAvg
    ELSE
    Final = (P1 + P2 + CRM) / 3.0
    END IF
    IF (Final >= 50) THEN
    write(*,*) CRM, ExmAvg, Final, 'PASS'
    ELSE
    write(*,*) CRM, ExmAvg, Final, 'FAIL'
END IF
END DO
END

```
## 运行结果
这个需要文件操作，我没运行

## 常量

```
!常量的定义与赋值
REAL, PARAMETER :: Pi = 3.141593
INTEGER, PARAMETER :: Two = 2
REAL, PARAMETER :: OneOver2Pi = 1 / (2 * Pi)
REAL, PARAMETER :: PiSquared = Pi ** Two

CHARACTER (LEN = *), PARAMETER:: Message = 'Press ENTER to continue'
!定义了一个字符串常量

```


## 内在函数
> 到目前为止，我们可以编写一些简单的程序了，但是，最有兴趣的问题是设计到一些特殊的数学函数， FORTRAN 含有许多的内在函数。
> 

```

PROGRAM Projectile
IMPLICIT NONE
REAL, PARAMETER :: g = 9.8 ! acceleration due to gravity
REAL, PARAMETER :: Pi = 3.1415927 ! a well known constant
REAL A ! launch angle in degrees
REAL T ! time of flight
REAL Theta ! direction at time T in degrees
REAL U ! launch velocity
REAL V ! resultant velocity
REAL Vx ! horizontal velocity
REAL Vy ! vertical velocity
REAL X ! horizontal displacement
REAL Y ! vertical displacement

write (*,*)'shu ru  A,T,U'
READ*, A, T, U


A = A * Pi / 180 ! convert angle to radians

X = U * COS( A ) * T
Y = U * SIN( A ) * T-g * T * T / 2.
Vx= U * COS( A)
Vy= U * SIN( A )-g * T
V = SQRT(Vx * Vx + Vy * Vy)
Theta = ATAN(Vy / Vx ) * 180 / Pi
PRINT*, 'x: ', X, 'y: ', Y
PRINT*, 'V: ', V, 'Theta: ', Theta
END


```
## 










