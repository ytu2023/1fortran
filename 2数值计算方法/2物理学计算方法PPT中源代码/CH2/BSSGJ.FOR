
	SUBROUTINE BSSGJ(A,N,L,B)
	DIMENSION A(N,N),B(N)
	DOUBLE PRECISION A,B,W,G
	L=1
	DO 100 K=1,N
	  M=N-K+1
	  W=A(1,1)
	  IF (W+1.0.EQ.1.0) THEN
	    L=0
	    WRITE(*,10)
	    RETURN
	  END IF
10	  FORMAT(1X,'FAIL')
	  DO 80 I=2,N
	    G=A(I,1)
	    B(I)=G/W
	    IF (I.LE.M) B(I)=-B(I)
	    DO 70 J=2,I
70	    A(I-1,J-1)=A(I,J)+G*B(J)
80	  CONTINUE
	  A(N,N)=1.0/W
	  DO 90 I=2,N
90	  A(N,I-1)=B(I)
100	CONTINUE
	DO 110 I=1,N-1
	DO 110 J=I+1,N
110	A(I,J)=A(J,I)
	RETURN
	END

