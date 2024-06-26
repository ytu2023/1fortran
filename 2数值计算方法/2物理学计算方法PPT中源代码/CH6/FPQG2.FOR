
	SUBROUTINE FPQG2(A,B,FS,F,EPS,S)
	EXTERNAL FS,F
	DIMENSION BB(10),H(10)
	DOUBLE PRECISION A,B,F,S,BB,H,S1,S2,TS1,TS2,S0,X,HH,G
	M=1
	N=1
	H(1)=B-A
	HH=B-A
	CALL PQG1(A,FS,F,EPS,S1)
	CALL PQG1(B,FS,F,EPS,S2)
	TS1=HH*(S1+S2)/2.0
	S0=TS1
	BB(1)=TS1
10	TS2=0.5*TS1
	DO 20 K=0,N-1
	  X=A+(K+0.5)*HH
	  CALL PQG1(X,FS,F,EPS,S1)
	  TS2=TS2+0.5*S1*HH
20	CONTINUE
	M=M+1
	H(M)=H(M-1)/2.0
	G=TS2
	DO 30 J=2,M
	  S=G-BB(J-1)
	  IF (ABS(S)+1.0.EQ.1.0) THEN
	    G=SIGN(1.0D+35,S)
	    G=G*SIGN(1.0D0,H(M)-H(J-1))
	  ELSE
	    G=(H(M)-H(J-1))/S
	  END IF
30	CONTINUE
	BB(M)=G
	S=BB(M)
	DO 40 J=M,2,-1
	  IF (ABS(S)+1.0.EQ.1.0) THEN
	    S=SIGN(1.0D+35,S)
	    S=S*SIGN(1.0D0,H(J-1))
	    S=BB(J-1)-S
	  ELSE
	    S=BB(J-1)-H(J-1)/S
	  END IF
40	CONTINUE
	IF ((ABS(S-S0).GE.EPS*(ABS(S)+1.0)).AND.(M.LE.9)) THEN
	  N=N+N
	  TS1=TS2
	  S0=S
	  HH=HH/2.0
	  GOTO 10
	END IF
	RETURN
	END

	SUBROUTINE PQG1(X,FS,F,EPS,S)
	DIMENSION B(10),H(10)
	DOUBLE PRECISION X,F,S,B,H,Y1,Y2,HH,TS1,TS2,S0,Y,G
	M=1
	N=1
	CALL FS(X,Y1,Y2)
	H(1)=Y2-Y1
	HH=Y2-Y1
	TS1=0.5*HH*(F(X,Y1)+F(X,Y2))
	S0=TS1
	B(1)=TS1
10	TS2=0.5*TS1
	DO 20 K=0,N-1
	  Y=Y1+(K+0.5)*HH
	  TS2=TS2+0.5*HH*F(X,Y)
20	CONTINUE
	M=M+1
	H(M)=H(M-1)/2.0
	G=TS2
	DO 30 J=2,M
	  S=G-B(J-1)
	  IF (ABS(S)+1.0.EQ.1.0) THEN
	    S=SIGN(1.0D+35,S)
	    G=S*SIGN(1.0D0,H(M)-H(J-1))
	  ELSE
	    G=(H(M)-H(J-1))/S
	  END IF
30	CONTINUE
	B(M)=G
	S=B(M)
	DO 40 J=M,2,-1
	  IF (ABS(S)+1.0.EQ.1.0) THEN
	    S=SIGN(1.0D+35,S)
	    S=S*SIGN(1.0D0,H(J-1))
	    S=B(J-1)-S
	  ELSE
	    S=B(J-1)-H(J-1)/S
	  END IF
40	CONTINUE
	IF ((ABS(S-S0).GE.EPS*(ABS(S)+1.0)).AND.(M.LE.9)) THEN
	  N=N+N
	  TS1=TS2
	  S0=S
	  HH=0.5*HH
	  GOTO 10
	END IF
	RETURN
	END


