
	DIMENSION X(20),Y(20),A(6)
	DOUBLE PRECISION X,Y,A,DT1,DT2,DT3,B
	B=0.0
	DO 10 I=1,20
	  X(I)=B+(I-1)*0.1
	  Y(I)=X(I)-EXP(-X(I))
10	CONTINUE
	N=20
	M=6
	CALL HPIR1(X,Y,A,N,M,DT1,DT2,DT3)
	WRITE(*,*)
	WRITE(*,20) (I,A(I),I=1,M)
20	FORMAT(1X,'A(',I2,' )=',D15.6)
	WRITE(*,*)
	WRITE(*,30) DT1,DT2,DT3
30	FORMAT(1X,'DT1=',D12.6,5X,'DT2=',D12.6,5X,'DT3=',D12.6)
	END

