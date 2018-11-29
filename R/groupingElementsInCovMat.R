#extract different types of elements in a covariant matrix.
#the diag is variance.
#the diag ele +1 in the odd row is cov in a group
#remain elements are cov values between two groups

m=3
vars <- NULL
covInG <-NULL
covBetG<-NULL

for( i in 1:((2*m)**2)){
	row = (i-1)%/%(2*m) +1 
	col = (i-1) %%(2*m) + 1
	if(col >= row){
		if(col==row){
			vars<-append(vars, i)
		}else if(col==row +1 && row %% 2==1){
			covInG <-append(covInG, i)
		} else {
			covBetG <- append(covBetG, i)
		}
		
	}
}

vars+1
covInG+1
covBetG+1