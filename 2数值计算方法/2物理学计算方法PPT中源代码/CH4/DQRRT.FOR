
	SUBROUTINE DQRRT(A,N,XR,XI,EPS,B,L)
	DIMENSION B(N,N),XR(N),XI(N),A(N)
	DOUBLE PRECISION B,XR,XI,A
	DO 10 J=1,N
10	B(1,J)=-A(J)
	DO 20 I=2,N
	DO 20 J=1,N
20	B(I,J)=0.0
	DO 30 I=1,N-1
30	B(I+1,I)=1.0
	CALL CHHQR(B,N,XR,XI,EPS,L)
	RETURN
	END

