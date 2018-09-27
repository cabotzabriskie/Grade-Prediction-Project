GPP R Analysis---------------------------------------------

Run "Weekly Data Splitter.R"
	-This will split the entire gradebook into weekly datasets
	-Creates columns for:
		FinalCourseGradeAB_Rest
		Lab[i]Missed - Indicator of if a particular lab was missed
		FHwk - codes writen homeworks to friday homeworks
		Lec[i]Missed
		TakeCSEM - if took the CSEM
		
Run "Registrar Data Manipulator.R"
	-Merges Registrar data with wk1 to remove individuals not in the class dataset
		Registrar data N = 70820
		wk1 N = 1282
		Removing students not in Registrar but in wk1 N = 1281
		Removing all NA's (required for Random Forests) N = 907
		Removing students w/o HSGPA N = 886
		Removing students w/o ACTSATM N = 801
		Size of Analysis dataset = 801
		
	-Generates a dataset with the following variables
		"Username"
		"CURCMP"
		"CURGPA"
		"CURAZcount"
		"CURCred"
		"CUREnroll"
		"P111LastGradeAB_Rest"
		"P111WVUCount"
		"HSGPA"
		"ACTSATM"
		"ACTSATV"
		"Cal1LastGradeAB_Rest"
		"Cal1WVUCount"
		"APCount"
		"APCredit"
		"TransferCount"
		"TransferCredit"
		"IsFirstGen"
		"IsFirstFall"
		"MathEntry"
		"IsWV"
		"P112LastSem"
		"P112FirstAttempt"
		
Run "Registrar Model.R"
	-Generates logistic and Random forest models for registrar data
	
Run "Week 1 Analysis.R" through "Week 8 Analysis.R"
	-Generates logistic and Random forest models for class and combined class & registrar data
	
	
	
	
	
