
	DIMENSION X(5),Y(5)
	DOUBLE PRECISION X,Y,T,Z
	DATA X/1.615,1.634,1.702,1.828,1.921/
	DATA Y/2.41450,2.46459,2.65271,3.03035,3.34066/
	EPS=0.000001
	T=1.682
	CALL ENATK(X,Y,5,T,EPS,Z)
	WRITE(*,10) T,Z
10	FORMAT(1X,'X=',F7.3,10X,'F(X)=',D15.6)
	T=1.813
	CALL ENATK(X,Y,5,T,EPS,Z)
	WRITE(*,10) T,Z
	END

