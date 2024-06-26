
	EXTERNAL FS,F
	DIMENSION JS(3),X(3)
	DOUBLE PRECISION F,S,X
	DATA JS/4,4,4/
	N=3
	CALL FGAUS(N,JS,X,FS,F,S)
	WRITE(*,10) S
10	FORMAT(1X,'S=',D13.6)
	END

	SUBROUTINE FS(J,N,X,DN,UP)
	DIMENSION X(N)
	DOUBLE PRECISION X,DN,UP,Q
	IF (J.EQ.1) THEN
	  DN=0.0
	  UP=1.0
	ELSE IF (J.EQ.2) THEN
	  DN=0.0
	  UP=SQRT(1.0-X(1)*X(1))
	ELSE IF (J.EQ.3) THEN
	  Q=X(1)*X(1)+X(2)*X(2)
	  DN=SQRT(Q)
	  UP=SQRT(2.0-Q)
	END IF
	RETURN
	END

	FUNCTION F(N,X)
	DIMENSION X(N)
	DOUBLE PRECISION F,X
	F=X(3)*X(3)
	RETURN
	END

