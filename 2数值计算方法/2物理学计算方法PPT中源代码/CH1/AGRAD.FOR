
	SUBROUTINE AGRAD(A,N,B,EPS,X,P,R,S,Q)
	DIMENSION A(N,N),B(N),X(N),P(N),R(N),S(N),Q(N)
	DOUBLE PRECISION A,B,X,P,R,S,Q,ALPHA,BETA,D,E
	DO 10 I=1,N
	  X(I)=0.0
	  P(I)=B(I)
	  R(I)=B(I)
10	CONTINUE
	I=1
20	CALL BRMUL(A,P,N,N,1,S)
	D=0.0
	E=0.0
	DO 30 K=1,N
	  D=D+P(K)*B(K)
	  E=E+P(K)*S(K)
30	CONTINUE
	ALPHA=D/E
	DO 40 K=1,N
40	X(K)=X(K)+ALPHA*P(K)
	CALL BRMUL(A,X,N,N,1,Q)
	D=0.0
	DO 50 K=1,N
	  R(K)=B(K)-Q(K)
	  D=D+R(K)*S(K)
50	CONTINUE
	BETA=D/E
	D=0.0
	DO 55 K=1,N
55	D=D+R(K)*R(K)
	D=SQRT(D)
	IF (D.LT.EPS) RETURN
	DO 60 K=1,N
60	P(K)=R(K)-BETA*P(K)
	I=I+1
	IF (I.LE.N) GOTO 20
	RETURN
	END






