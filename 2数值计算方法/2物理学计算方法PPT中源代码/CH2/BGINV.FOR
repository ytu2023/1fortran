
	SUBROUTINE BGINV(M,N,A,AA,L,EPS,U,V,KA,S,E,WORK)
	DIMENSION A(M,N),U(M,M),V(N,N),AA(N,M)
	DIMENSION S(KA),E(KA),WORK(KA)
	DOUBLE PRECISION A,U,V,AA,S,E,WORK
	CALL BMUAV(A,M,N,U,V,L,EPS,KA,S,E,WORK)
	IF (L.EQ.0) THEN
	  K=1
10	  IF (A(K,K).NE.0.0) THEN
	    K=K+1
	    IF (K.LE.MIN(M,N)) GOTO 10
	  END IF
	  K=K-1
	  IF (K.NE.0) THEN
	    DO 40 I=1,N
	    DO 40 J=1,M
	      AA(I,J)=0.0
	      DO 30 II=1,K
30	      AA(I,J)=AA(I,J)+V(II,I)*U(J,II)/A(II,II)
40	    CONTINUE
	  END IF
	END IF
	RETURN
	END

