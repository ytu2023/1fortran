
	DIMENSION A(5,4)
	DOUBLE PRECISION A
	DATA A/1.0,5.0,9.0,13.0,17.0,2.0,6.0,10.0,14.0,18.0,
     *         3.0,7.0,11.0,15.0,19.0,4.0,8.0,12.0,16.0,20.0/
	CALL BRANK(A,5,4,K)
	WRITE(*,10) K
10	FORMAT(1X,'RANK=',I3)
	END

