      SUBROUTINE CJCBI(A,N,EPS,V,L)
      IMPLICIT NONE
      DIMENSION A(N,N),V(N,N)
      DOUBLE PRECISION A,V,FM,CN,SN,OMEGA,X,Y,EPS
      INTEGER P,Q,L,I,J,N
      L=1
      DO 20 I=1,N
        V(I,I)=1.0D0
        DO 10 J=1,N
          IF (I.NE.J) V(I,J)=0.0D0
10        CONTINUE
20      CONTINUE
25      FM=0.0D0
      DO 30 I=2,N
      DO 30 J=1,I-1
        IF (DABS(A(I,J)).GT.FM) THEN
          FM=DABS(A(I,J))
          P=I
          Q=J
        END IF
30      CONTINUE
      IF (FM.LT.EPS) THEN
        L=1
        RETURN
      END IF
      IF (L.GT.1000) THEN
        L=0
        RETURN
      END IF
      L=L+1
      X=-A(P,Q)
      Y=(A(Q,Q)-A(P,P))/2.0D0
      OMEGA=X/DSQRT(X*X+Y*Y)
      IF (Y.LT.0.0) OMEGA=-OMEGA
      SN=1.0D0+DSQRT(1.0D0-OMEGA*OMEGA)
      SN=OMEGA/DSQRT(2.0D0*SN)
      CN=DSQRT(1.0-SN*SN)
      FM=A(P,P)
      A(P,P)=FM*CN*CN+A(Q,Q)*SN*SN+A(P,Q)*OMEGA
      A(Q,Q)=FM*SN*SN+A(Q,Q)*CN*CN-A(P,Q)*OMEGA
      A(P,Q)=0.0D0
      A(Q,P)=0.0D0
      DO 60 J=1,N
        IF ((J.NE.P).AND.(J.NE.Q)) THEN
          FM=A(P,J)
          A(P,J)=FM*CN+A(Q,J)*SN
          A(Q,J)=-FM*SN+A(Q,J)*CN
        END IF
60      CONTINUE

      DO 70 I=1,N
        IF ((I.NE.P).AND.(I.NE.Q)) THEN
          FM=A(I,P)
          A(I,P)=FM*CN+A(I,Q)*SN
          A(I,Q)=-FM*SN+A(I,Q)*CN
        END IF
70      CONTINUE
      DO 80 I=1,N
        FM=V(I,P)
        V(I,P)=FM*CN+V(I,Q)*SN
        V(I,Q)=-FM*SN+V(I,Q)*CN
80      CONTINUE
      GOTO 25
      END
