## 本地编辑完上传到github

## 主程序

```
program gaosi
    implicit none

      DIMENSION A(4,4),B(4),X(4),JS(4)
      DOUBLE PRECISION:: A,B,X
      integer:: JS,N,L,I

      DATA A/0.2368,0.1968,0.1582,1.1161,0.2471,0.2071,1.1675,0.1254, &
           0.2568,1.2168,0.1768,0.1397,1.2671,0.2271,0.1871,0.1490/
      DATA B/1.8471,1.7471,1.6471,1.5471/

      N=4

      CALL AGAUS(A,B,N,X,L,JS)

      IF (L.NE.0) THEN
        WRITE(*,10) (I,X(I),I=1,4)
    else
        write(*,*) 'this fc no value'
      END IF

10      FORMAT(1X,'X(',I2,' )=',D15.6)
      END program gaosi
```

### **子程序 SUBROUTINE**

```
      SUBROUTINE AGAUS(A,B,N,X,L,JS)
      DIMENSION A(N,N),X(N),B(N),JS(N)
      DOUBLE PRECISION A,B,X,T
      L=1
      DO 50 K=1,N-1
        D=0.0
        DO 210 I=K,N
        DO 210 J=K,N
          IF (ABS(A(I,J)).GT.D) THEN
            D=ABS(A(I,J))
            JS(K)=J
            IS=I
          END IF
210        CONTINUE
        IF (D+1.0.EQ.1.0) THEN
          L=0
        ELSE
          IF (JS(K).NE.K) THEN
            DO 220 I=1,N
              T=A(I,K)
              A(I,K)=A(I,JS(K))
              A(I,JS(K))=T
220            CONTINUE
          END IF
          IF (IS.NE.K) THEN
            DO 230 J=K,N
              T=A(K,J)
              A(K,J)=A(IS,J)
              A(IS,J)=T
230            CONTINUE
            T=B(K)
            B(K)=B(IS)
            B(IS)=T
          END IF
        END IF
        IF (L.EQ.0) THEN
          WRITE(*,100)
          RETURN
        END IF
        DO 10 J=K+1,N
          A(K,J)=A(K,J)/A(K,K)
10        CONTINUE
        B(K)=B(K)/A(K,K)
        DO 30 I=K+1,N
          DO 20 J=K+1,N
            A(I,J)=A(I,J)-A(I,K)*A(K,J)
20          CONTINUE
          B(I)=B(I)-A(I,K)*B(K)
30        CONTINUE
50      CONTINUE
      IF (ABS(A(N,N))+1.0.EQ.1.0) THEN
        L=0
        WRITE(*,100)
        RETURN
      END IF
      X(N)=B(N)/A(N,N)
      DO 70 I=N-1,1,-1
        T=0.0
        DO 60 J=I+1,N
          T=T+A(I,J)*X(J)
60        CONTINUE
        X(I)=B(I)-T
70      CONTINUE
100      FORMAT(1X,' FAIL ')
      JS(N)=N
      DO 150 K=N,1,-1
        IF (JS(K).NE.K) THEN
          T=X(K)
          X(K)=X(JS(K))
          X(JS(K))=T
        END IF
150      CONTINUE
      RETURN
      END
```
