
	FUNCTION MCHII(X,N)
	DOUBLE PRECISION MCHII,X,MGAM2
	IF (X.LT.0.0) X=-X
	MCHII=MGAM2(N/2.0D0,X/2.0)
	RETURN
	END

