
	EXTERNAL F
	DIMENSION Y(2),D(2),Z(2,11),P(2),F1(2),F2(2),F3(2),G(2)
	DOUBLE PRECISION Y,D,Z,A,H,T,P,F1,F2,F3,G
	DATA Y/1.0,0.0/
	A=0.0
	H=0.1
	N=11
	M=2
	EPS=0.000001
	CALL GGJFQ(A,H,N,Y,M,Z,F,D,EPS,P,F1,F2,F3,G)
	WRITE(*,*)
	DO 10 I=1,N
	  T=(I-1)*H
	  WRITE(*,20) T,Z(1,I),Z(2,I)
	  WRITE(*,*)
10	CONTINUE
20	FORMAT(1X,'T=',F6.3,5X,'Y1=',D13.6,5X,'Y2=',D13.6)
	END

	SUBROUTINE F(T,Y,M,D)
	DIMENSION Y(M),D(M)
	DOUBLE PRECISION Y,D,T
	D(1)=-Y(2)
	D(2)=Y(1)
	RETURN
	END

