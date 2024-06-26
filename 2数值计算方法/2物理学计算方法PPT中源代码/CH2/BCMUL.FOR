
	SUBROUTINE BCMUL(AR,AI,BR,BI,M,N,K,CR,CI)
	DIMENSION AR(M,N),AI(M,N),BR(N,K),BI(N,K)
	DIMENSION CR(M,K),CI(M,K)
	DOUBLE PRECISION AR,AI,BR,BI,CI,CR,P,Q,S
	DO 100 I=1,M
	DO 100 J=1,K
	  CR(I,J)=0.0
	  CI(I,J)=0.0
	  DO 50 L=1,N
	    P=AR(I,L)*BR(L,J)
	    Q=AI(I,L)*BI(L,J)
	    S=(AR(I,L)+AI(I,L))*(BR(L,J)+BI(L,J))
	    CR(I,J)=CR(I,J)+P-Q
	    CI(I,J)=CI(I,J)+S-P-Q
50	  CONTINUE
100	CONTINUE
	RETURN
	END


