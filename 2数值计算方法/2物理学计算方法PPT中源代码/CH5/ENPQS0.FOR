
	DIMENSION X(10),Y(10)
	DOUBLE PRECISION X,Y,T,Z
	DATA X/-1.0,-0.8,-0.65,-0.4,-0.3,0.0,0.2,0.45,0.8,1.0/
	DATA Y/0.0384615,0.0588236,0.0864865,0.2,0.307692,
     *         1.0,0.5,0.164948,0.0588236,0.0384615/
	T=-0.85
	CALL ENPQS(X,Y,10,T,Z)
	WRITE(*,20) T,Z
20	FORMAT(1X,'X=',F7.3,10X,'F(X)=',D15.6)
	T=0.25
	CALL ENPQS(X,Y,10,T,Z)
	WRITE(*,20) T,Z
	END

