USE DWH

	SELECT CaseNo 
		 ,Case when CoO_L1  in ('Aleppo' , 'Damascus' , 'Hassakeh') then CoO_L1 else 'Other'  end COO_L1 
		 , Case when CurrentSize BETWEEN 3 AND 5 then  '_3_5' ELSE CASE WHEN CurrentSize>5 then '5' ELSE CAST(CurrentSize AS NVARCHAR(1)) END  end size 
		,CASE when CaseNo in (Select CaseNo from [dbo].[vSPNeedsDetailUniqueByIndividual] A inner join fn_getActiveIndividuals ('2017-02-28') B
							 ON (A.IndividualID = B.IndividualID)) then '1' else '0' end needs , 
case when CoA_Phone is not null then '1' else '0' end phone
from fn_getActiveIndividuals ('2017-02-28') where COA = 'IRQ' and Relationship = 'PA'   AND coo='SYR' AND CurrentSize>0 

ORDER BY COO_L1, size
 

	SELECT CaseNo 
		 ,Case when CoO_L1  in ('Aleppo',	'Damascus',	'Homs',	'Rural Damascus') then CoO_L1 else 'Other'  end COO_L1 
		 , Case when CurrentSize BETWEEN 3 AND 5 then  '_3_5' ELSE CASE WHEN CurrentSize>5 then '5' ELSE CAST(CurrentSize AS NVARCHAR(1)) END  end size 
		,CASE when CaseNo in (Select CaseNo from [dbo].[vSPNeedsDetailUniqueByIndividual] A inner join fn_getActiveIndividuals ('2017-02-28') B
							 ON (A.IndividualID = B.IndividualID)) then '1' else '0' end needs , 
case when CoA_Phone is not null then '1' else '0' end phone
from fn_getActiveIndividuals ('2017-02-28') where COA = 'ARE' and Relationship = 'PA'   AND coo='SYR' AND CurrentSize>0
ORDER BY COO_L1, size
 
	SELECT CaseNo 
		 ,Case when CoO_L1  in ('Aleppo',	'Damascus',	'Idleb','Dara',	'Homs',	'Rural Damascus') then CoO_L1 else 'Other'  end COO_L1 
		 , Case when CurrentSize BETWEEN 3 AND 5 then  '_3_5' ELSE CASE WHEN CurrentSize>5 then '5' ELSE CAST(CurrentSize AS NVARCHAR(1)) END  end size 
		,CASE when CaseNo in (Select CaseNo from [dbo].[vSPNeedsDetailUniqueByIndividual] A inner join fn_getActiveIndividuals ('2017-02-28') B
							 ON (A.IndividualID = B.IndividualID)) then '1' else '0' end needs , 
case when CoA_Phone is not null then '1' else '0' end phone
from fn_getActiveIndividuals ('2017-02-28') where COA = 'JOR' and Relationship = 'PA'   AND coo='SYR' AND CurrentSize>0

ORDER BY COO_L1, size
 
