
	SUBROUTINE FHMGS(F,G)
	DIMENSION T(5),C(5)
	DOUBLE PRECISION F,G,T,C,X
	DATA C/1.181469599,0.9865791417,0.9453089237,
     *         0.9865791417,1.181469599/
	DATA T/-2.02018200,-0.95857190,0.0,
     *         0.95857190,2.02018200/

	G=0.0D0
	DO 10 I=1,5
	  X=T(I)
	  G=G+F(X)*C(I)
10	CONTINUE
	END


