
	FUNCTION MGAM1(X)
	DOUBLE PRECISION MGAM1,X
	DOUBLE PRECISION Y,T,S,U,A(11)
	DATA A/0.0000677106,-0.0003442342,0.0015397681,
     *         -0.0024467480,0.0109736958,-0.0002109075,
     *         0.0742379071,0.0815782188,0.4118402518,
     *         0.4227843370,1.0/
	IF (X.LE.0.) THEN
	  WRITE(*,*) '  ERR** X<0!'
	  MGAM1=-1.0
	  RETURN
	END IF
	Y=X
	IF (Y.LE.1.0) THEN
	  T=1.0/(Y*(Y+1.0))
	  Y=Y+2.0
	ELSE IF (Y.LE.2.0) THEN
	  T=1.0/Y
	  Y=Y+1.0
	ELSE IF (Y.LE.3.0) THEN
	  T=1.0
	ELSE
	  T=1.0
10	  IF (Y.GT.3.0) THEN
	    Y=Y-1.0
	    T=T*Y
	    GOTO 10
	  END IF
	END IF
	S=A(1)
	U=Y-2.0
	DO 20 I=1,10
20	S=S*U+A(I+1)
	S=S*T
	MGAM1=S
	RETURN
	END

