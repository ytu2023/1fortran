
	DIMENSION X(11),Y(21),Z(11,21),A(6,5),V(20,21)
	INTEGER P,Q
	DOUBLE PRECISION X,Y,Z,A,DT1,DT2,DT3,V
	N=11
	M=21
	P=6
	Q=5
	DO 10 I=1,N
10	X(I)=0.2*(I-1)
	DO 20 I=1,M
20	Y(I)=0.1*(I-1)
	DO 40 I=1,N
	DO 40 J=1,M
40	Z(I,J)=EXP(X(I)*X(I)-Y(J)*Y(J))
	CALL HPIR2(X,Y,Z,N,M,A,P,Q,DT1,DT2,DT3,V)
	WRITE(*,*)
	WRITE(*,50)
50	FORMAT(1X,27X,'A(I,J)')
	WRITE(*,*)
	WRITE(*,60) ((A(I,J),J=1,Q),I=1,P)
60	FORMAT(1X,5D13.6)
	WRITE(*,*)
	WRITE(*,70) DT1,DT2,DT3
70	FORMAT(1X,'DT1=',D13.6,3X,'DT2=',D13.6,3X,'DT3=',D13.6)
	END

