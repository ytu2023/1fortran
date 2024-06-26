
	EXTERNAL FS
	DIMENSION X(3),Y(3)
	DOUBLE PRECISION X,Y
	DATA X/1.5,6.5,-5.0/
	EPS=1.0E-06
	CALL DSNSE(3,EPS,X,Y,FS,L)
	IF (L.GT.0) THEN
	  DO 10 I=1,3
10	  WRITE(*,20) I,X(I)
	END IF
20	FORMAT(1X,'X(',I2,')=',D15.6)
	END

	SUBROUTINE FS(X,N,F,Y)
	DIMENSION X(N),Y(N)
	DOUBLE PRECISION X,Y,F,F1,F2,F3,DF1,DF2,DF3
	F1=X(1)-5*X(2)*X(2)+7*X(3)*X(3)+12.0
	F2=3*X(1)*X(2)+X(1)*X(3)-11.0*X(1)
	F3=2.0*X(2)*X(3)+40.0*X(1)
	F=F1*F1+F2*F2+F3*F3
	DF1=1.0
	DF2=3.0*X(2)+X(3)-11.0
	DF3=40.0
	Y(1)=2.0*(F1*DF1+F2*DF2+F3*DF3)
	DF1=10.0*X(2)
	DF2=3.0*X(1)
	DF3=2.0*X(3)
	Y(2)=2.0*(F1*DF1+F2*DF2+F3*DF3)
	DF1=14.0*X(3)
	DF2=X(1)
	DF3=2.0*X(2)
	Y(3)=2.0*(F1*DF1+F2*DF2+F3*DF3)
	RETURN
	END

