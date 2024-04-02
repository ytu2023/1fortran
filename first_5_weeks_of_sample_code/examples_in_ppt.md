## 致谢
- 1Adobe的PDF阅读功能，让我对PDF的编辑更加方便
- 2、感谢code::block
- 3、感谢guithub
- 4、感谢授课老师
- 5、感谢我自己
## 第一个code::block测试程序

```
program hello
    implicit none
!$omp parallel num_threads(2)
    print *, "Hello World!"
!$omp end parallel
    write (*,*)"string i love china"

end program
```
## 运行结果


##第二个程序
> 功能说明
> 计算公式：
$A(t)=174.6(t-1981.2)^3$

```
PROGRAM AIDS
! Calculates number of accumulated AIDS cases in USA
INTEGER T        ! year
REAL A           ! number of cases
write(*,*)'please input T,end on enter'
READ(*,*) T
A = 174.6 * (T-1981.2) ** 3
write(*,*) 'Accumulated AIDS in US by year', T, ':', A
END PROGRAM AIDS


```
## 第三个 计算利息
> 主要是为了讲解 老板的fortarn语法与现在的对比，现在的语法更加灵活。
```
PROGRAM MONEY
! Calculates balance after interest compounde
REAL BALANCE, INTEREST, RATE
BALANCE = 1000
RATE = 0.09
INTEREST = RATE * BALANCE
BALANCE = BALANCE + INTEREST
write(*,*) 'New balance:'， BALANCE
END PROGRAM MONEY
```
## 第四个 数据类型与精度的定义
> 程序源代码
```
Program test_kinds
implicit none
integer, parameter :: long=selected_real_kind ( 9,199)
real(kind=long) :: a=2.8_long
write(*,*) KIND(a)
write(*,*) long
write(*,*) precision(a)
write(*,*) range(a)
write(*,*) huge(a)
end


```
程序的运行结果
```
           8
           8
          15
         307
   1.7976931348623157E+308
```
## 重力场下的垂直运动

```
PROGRAM Vertical
! Vertical motion under gravity
IMPLICIT NONE
REAL ， PARAMETER :: G = 9.8 ! 有名常量，在计算中固定不变
REAL S ! displacement (m)
REAL T ! time
REAL U ! initial speed(m/s)
Write(*,*) 'Time              Displacement'
U = 60
T = 6
S = U * T - G / 2 * T ** 2
Write(*,*) T, S
END PROGRAM Vertical
```
输出结果
```
 Time              Displacement
   6.00000000       183.599991

Process returned 0 (0x0)   execution time : 0.015 s
Press any key to continue.

```

## 测试read的用法
> 就是想告诉你，read输入多了，会舍弃，输入的少了则会出错
```
PROGRAM test
! read的用法，输入的多了不太行
IMPLICIT NONE
real A,B,C
write(*,*)'please input:'
READ (*,*) A,B
READ (*,*) C
WRITE(*,*) A
WRITE (*,*) B,C
write (*,*) A,B,C
END PROGRAM test
```
> input outcome
> 
 ```
2 4 6
5
```

> output outcome
> 
 ```
 please input:
2 4 6
5
   2.00000000
   4.00000000       5.00000000
   2.00000000       4.00000000       5.00000000

Process returned 0 (0x0)   execution time : 7.325 s
Press any key to continue.

```
