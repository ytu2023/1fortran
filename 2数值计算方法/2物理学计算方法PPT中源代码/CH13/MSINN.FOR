

	FUNCTION MSINN(X)
	DOUBLE PRECISION MSINN,X,FLRGS
	EXTERNAL SI
	DOUBLE PRECISION SI
	IF (X.LE.0.0) THEN
	  MSINN=0.0
	  RETURN
	END IF
	MSINN=FLRGS(0.0D0,X,SI,1.0E-07)
	RETURN
	END

	FUNCTION SI(X)
	DOUBLE PRECISION SI,X
	SI=SIN(X)/X
	RETURN
	END



