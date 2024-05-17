#Include 'Protheus.ch'

//---------------------------------------------------------------------------
/*/ {Protheus.doc} PRGA010 - Considera beneficios adicionais(TV1) de acordo 
com o Roteiro posicionado.
@author  Fernando
@since   17/05/2016
/*/
//---------------------------------------------------------------------------
User Function PRGA010(aVRCalc,aVTCalc)
	
	Local cChave 	  := ""
	Local i 		  :=0	
	Local nDiasTrab   := DIASTRAB
	Local _cRot 	  := cRot
	Local nResult	  := 0
	Local nValPagFunc := 0
	Local aBeneficios := {}
	Local lSubTrai	  := .f.

	/*
	------------------------------------------------------------- 
	Estrutura do Array: aVRCalc:
		// VR
		aAdd(aVRCalc ,;
			{cKeyAux:= SR0->(R0_FILIAL+R0_MAT+R0_TPVALE+R0_CODIGO),; 1
			nDiaCal,; 	2
			nValUni,; 	3
			nValVRF,; 	4
			nCustFunc,; 5
			nCustEmp,;  6
			nDiaRef,;   7
			lPropin} )  8
		
		// VT	
	    aAdd(aVTCalc ,;
	     	{cKeyAux := SR0->(R0_FILIAL+R0_MAT+R0_TPVALE+R0_CODIGO),; 1
	     	nDiasVTR,; 	 2
	     	nValUni,;  	 3
	     	nValVTR,;  	 4
	     	nCustFunc,;	 5 
	     	(nValVTR-nCustFunc),; 6
	     	nValUniDif,; 7
	     	nDiasProp,;  8
	     	0 } )	     9
	-------------------------------------------------------------	
	*/

Do Case

	Case _cRot == "VTR"
		
		DbSelectArea("SR0")
		SR0->(DbSetOrder(1))
		
		nTotVTAcum := 0
		nValPagFunc := (SRA->RA_SALARIO) * 0.06
		
		aBeneficios := aClone(aVTCalc)
		
		For i := 1 to Len(aBeneficios)
			
			cChave := aBeneficios[i,1]
			cChave := Right(cChave, TamSx3('R0_TPVALE')[1]+TamSx3('R0_CODIGO')[1])
			cChave := Right(cChave,TamSx3('R0_CODIGO')[1])+Left(cChave, TamSx3('R0_TPVALE')[1]) //inverte ordem, conforme o indice.
			
			cChave := Left(aBeneficios[i,1], TamSx3('R0_FILIAL')[1]+TamSx3('R0_MAT')[1] ) + cChave
			
			If SR0->(DbSeek(cChave))
				
				nAdicionais	:= BenefAdic("1", @lSubtrai)
				
				//_nDias := (SR0->R0_QDIACAL/SR0->R0_QDIAINF)
				//_nDias += nAdicionais				
				
				nDiasVTR 	:= aBeneficios[i,08]
				nDiasVTR 	:= nDiasVTR + nAdicionais

				aBeneficios[i,08] := nDiasVTR 
				aBeneficios[i,02] := nDiasVTR * SR0->R0_QDIAINF
				aBeneficios[i,04] := aBeneficios[i,02] * aBeneficios[i,03]

				nTotVTAcum += aBeneficios[i,04] //total de VT Acumulado
			EndIf			
		Next i


		For i := 1 to len(aBeneficios)
			
			/*
	     	nValVTR,;  4
	     	nCustFunc,;5 
	     	(nValVTR-nCustFunc),; 6
	     	*/
			
			aBeneficios[i,9] := nTotVTAcum
			
			nValPagFunc := (SRA->RA_SALARIO) * 0.06			
			nValPagFunc := nValPagFunc * aBeneficios[i,04]
			nResult 	:= nValPagFunc/aBeneficios[i,09]
	
			//aBeneficios[i,05] := nResult -  custo Funcionario
			
			If nResult < aBeneficios[i,04] 
				aBeneficios[i,05] := nResult
			Else
				aBeneficios[i,05] := aBeneficios[i,04]				
			EndIf
			aBeneficios[i,06] := aBeneficios[i,04] - aBeneficios[i,05]			

		Next i
		
		aVTCalc := aClone(aBeneficios)
		

	Case _cRot == "VRF"
	
		aBeneficios := aClone(aVRCalc)

		For i := 1 to Len(aBeneficios)

			nDiaCal := aBeneficios[i,02]

			DbSelectArea("RCF")
			
			RCF->(DbSetOrder(1)) //RCF_FILIAL, RCF_ANO, RCF_MES, RCF_TNOTRA, RCF_SEMANA
			lSeek:= RCF->(DbSeek(xFilial("RCF")+CANOMES+SRA->RA_TNOTRAB+CSEMANA))
			If !lSeek
				// Posiciona no turno padrão caso não encontre o turno padrão do funcionário nos períodos.
				lSeek:= RCF->(DbSeek(xFilial("RCF")+CANOMES+AvKey("@@@","RCF_TNOTRA")+CSEMANA))
			EndIf

			nDiasTrab := RCF->RCF_DIATRA
			
			nAdicionais := BenefAdic("2", @lSubTrai)

			If (nDiaCal < nDiasTrab) //20% sobre valor a ser pago.

				nDiaCal  := nDiaCal + nAdicionais
				nValUni  := aBeneficios[i,03]
				nTotal	 := Round( nValUni * nDiaCal, 2 )			
				nPercent := (20/100)
				
				nCustFunc:= Round(nTotal * nPercent, 2)			
				
				If lSubtrai // Se houver Sutracao de Benefícios Adicionais, considero 20% do valor a pagar do vale.
					nTotal := Round( (aBeneficios[i,04] * nPercent) * nDiaCal,2)			
				EndIf

			Else //2,5(ou o que estiver informado na tabela) sobre o salário.
				
				nDiaCal := nDiaCal + nAdicionais

				cChave := aBeneficios[i,1]
				cChave := Right(cChave, TamSx3('R0_TPVALE')[1]+TamSx3('R0_CODIGO')[1])

				DbSelectArea("RFO")
				RFO->(DbSetOrder(1)) //RFO_FILIAL, RFO_TPVALE, RFO_CODIGO
				RFO->(DbSeek(xFilial("RFO")+ cChave))
				
				_nSal := Round( SRA->RA_SALARIO, 2 )			
				nCustFunc := Round((_nSal * RFO->RFO_PERC)/100, 2)
				
			EndIf

			//====================================================
			//Calcula o Custo do Vale Refeicao - Especifico TV1
			//****************************************************
			//1-Valor Salario
			//1-Valor Refeicao(Padrao Totvs)
			//****************************************************
			
			//Estrutura Array aRefeicao
			//------------------------------------------------------------------------------------------------------------------------------
			//Aadd(aRefeicao,{ cCodigo ,nValRefe,nNumVale,nPerDesc,nTetoRefe , cDesc, lUseParamVR, Subs(SRX->RX_COD,nEmpDE, nEmpQtos ) })
			//==============================================================================================================================		
			nValUni := aBeneficios[i,03]
			nTotal:= Round( nValUni * nDiaCal, 2 )

			If lSubtrai // Se houver Sutracao de Benefícios Adicionais, considero 20% do valor a pagar do vale.
				nPercent   := (20/100)
				nCustFunc  := (nTotal * nPercent)
			EndIf
			
			If RFO->RFO_TETO > 0 .And. nCustFunc > RFO->RFO_TETO 
				nCustFunc := RFO->RFO_TETO	 
			EndIf		
							
			nCustEmp := nTotal - nCustFunc		

			aBeneficios[i,02] :=  nDiaCal
			aBeneficios[i,03] :=  nValUni
			aBeneficios[i,04] :=  (nDiaCal * nValUni )
			aBeneficios[i,05] :=  nCustFunc
			aBeneficios[i,06] :=  nCustEmp
		Next

		aVRCalc := aClone(aBeneficios)

	//Case _cRot == "VA"
	
EndCase
	
Return(Nil)

//-----------------------------------------------
// Retorna a Quantidade de Benefícios Adicinais 
//para Cálculo de Benefício
//------------------------------------------------
Static Function BenefAdic(cTipo, lSubTrai)
	Local nRet:= 0
	//Calcula a quantidade do Beneficio adicional
	DbSelectArea("Z01")
	Z01->(DbSetORder(1))
	If Z01->(DbSeek(xFilial("Z01")+AvKey(SRA->RA_MAT,"Z01_MAT")+Alltrim(CANOMES)))
		Do While Z01->(!Eof()) .And. Alltrim(SRA->RA_MAT) == Alltrim(Z01->Z01_MAT) .And. Alltrim(CANOMES)==Alltrim(Z01->Z01_ANO)+Alltrim(Z01->Z01_MES)
			If Z01->Z01_TIPO == cTipo
				If Alltrim(Z01->Z01_OPER) == "2"
		   			nRet := Z01->Z01_DIAS * -1
		   			lSubTrai := .T.
		   		Else
		   			nRet += Z01->Z01_DIAS
		   		EndIf	
	  		EndIf
  			Z01->(DbSkip())	
  		EndDo
	EndIf	
Return(nRet)
