
	EXTERNAL F
	DIMENSION Y(2),D(2),B(10,2),U(2),V(2),W(2)
	DOUBLE PRECISION Y,D,T,H,B,U,V,W
	T=0.0
	H=0.1
	M=2
	EPS=0.000001
	Y(1)=1.0
	Y(2)=0.0
	WRITE(*,*)
	WRITE(*,20) T,Y(1),Y(2)
	WRITE(*,*)
	DO 10 I=1,10
	  CALL GPBS1(T,H,Y,M,F,EPS,D,B,U,V,W)
	  T=T+H
	  WRITE(*,20) T,Y(1),Y(2)
	  WRITE(*,*)
10	CONTINUE
20	FORMAT(1X,'T=',F7.3,5X,'Y1=',D13.6,5X,'Y2=',D13.6)
	END

	SUBROUTINE F(T,Y,M,D)
	DIMENSION Y(M),D(M)
	DOUBLE PRECISION Y,D,T
	D(1)=-Y(2)
	D(2)=Y(1)
	RETURN
	END

