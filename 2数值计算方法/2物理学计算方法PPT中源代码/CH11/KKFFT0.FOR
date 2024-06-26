

	DIMENSION PR(64),PI(64),FR(64),FI(64)
	DOUBLE PRECISION PR,PI,FR,FI
	N=64
	K=6
	DO 10 I=1,N
	  PR(I)=EXP(-0.1*(I-0.5))
	  PI(I)=0.0
10	CONTINUE
	WRITE(*,*)
	WRITE(*,20) (PR(I),I=1,N)
20	FORMAT(1X,4D15.6)
	WRITE(*,30)
30	FORMAT(2X,'  ')
	CALL KKFFT(PR,PI,N,K,FR,FI,0,1)
	WRITE(*,20) (FR(I),I=1,N)
	WRITE(*,30)
	WRITE(*,20) (FI(I),I=1,N)
	WRITE(*,30)
	WRITE(*,20) (PR(I),I=1,N)
	WRITE(*,30)
	WRITE(*,20) (PI(I),I=1,N)
	WRITE(*,30)
	CALL KKFFT(FR,FI,N,K,PR,PI,1,1)
	WRITE(*,20) (FR(I),I=1,N)
	WRITE(*,*)
	END

