
	DOUBLE PRECISION X,Y
	X=1.0
	Y=4.0
	CALL PCSIN(X,Y,X,Y)
	WRITE(*,10) X,Y
10	FORMAT(1X,D13.6,'+j',D13.6)
	END

