
	SUBROUTINE LRSRH(A,N,MM,NN,X,K)
	DIMENSION A(N)
	K=MM
10	IF (K.LE.NN) THEN
	  IF (A(K).NE.X) THEN
	    K=K+1
	    GOTO 10
	  END IF
	END IF
	IF (K.GT.NN) K=0
	RETURN
	END

