
	DIMENSION Y(10)
	DOUBLE PRECISION X,H,Y,T,Z
	DATA Y/0.904837,0.818731,0.740818,0.670320,0.606531,
     *         0.548812,0.496585,0.449329,0.406570,0.367879/
	X=0.1
	H=0.1
	EPS=0.000001
	T=0.15
	CALL EEATK(X,H,10,Y,T,EPS,Z)
	WRITE(*,10) T,Z
10	FORMAT(1X,'X=',F7.3,10X,'F(X)=',D15.6)
	T=0.55
	CALL EEATK(X,H,10,Y,T,EPS,Z)
	WRITE(*,10) T,Z
	END

