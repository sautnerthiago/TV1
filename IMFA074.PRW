#Include "Protheus.ch"
#Include "RwMake.ch"
#Include "TbiConn.ch"

#DEFINE DEF_BOLETO    1
#DEFINE DEF_DATAHORA  2
#DEFINE DEF_USUARIO   3
#DEFINE DEF_AREA      4
#DEFINE DEF_CLIENTE   5
#DEFINE DEF_ORIGEM    6
#DEFINE DEF_DESTINO   7
#DEFINE DEF_MOTIVO    8
#DEFINE DEF_PSP       9
#DEFINE DEF_CCUSTO    10
#DEFINE DEF_VALOR     11
#DEFINE DEF_TAREFA    12

#DEFINE DEF_MAXCOLS  12

//-------------------------------------------------------------------
/*/IMFA074 - Carrega Planilha PontoCom
@aParam
@author		Fernando
@since		20/03/2014
@uso        Especificos TV1
@Obs
/*/
//--------------------------------------------------------------------
User Function IMFA074()

Local aPergs  		:= {}
Local cForn   		:= space(tamsx3("A2_COD")[1])
Local cProd   		:= AvKey("0000000294","B1_COD")
Local aHead   		:= {}
Local aRet	  		:= {}
Local aDet	  		:= {}
Local _cNumPC 		:= ""
Local _lProc  		:= .t.

/*Variaveis reservadas para Visualizacao de Pedido de Compras*/
Private cAlias   	:= "SC7"
Private INCLUI := .t.

//Alert("Importar planilha de Pedido de Compra....")

aAdd(aPergs ,{1,"Fornecedor:" ,cForn ,"@",'.T.',"SA2074",'.T.',tamsx3("A2_COD")[1],.T.})
aAdd(aPergs ,{1,"Produto: 	 " ,cProd ,"@!",'!Empty(mv_par02)','SB1074','.T.',tamsx3("B1_COD")[1]*3,.T.})
aAdd(aPergs	,{6,"Selecione a Pasta?", Space(300), "@S120", "File(MV_PAR03)", "", 100, .t., "Arquivos .CSV |*.CSV", "",GETF_LOCALHARD+GETF_LOCALFLOPPY+GETF_NETWORKDRIVE})

If ParamBox(aPergs ,"Importac�o de Despesas " + Alltrim(Posicione("SB1",1, xFilial("SB1")+cProd, "B1_DESC")) ,aRet)

	If !MsgYesNo("ATEN��O... deseja Processar a Planilha Selecionada?")
		Return
	EndIf
	
	aHead := {MV_PAR01, MV_PAR02}

	Processa( {|| _lProc := .t., sleep(1000), aDet:= A074ReadFile( Alltrim(MV_PAR03) ) },"Aguarde... Formatando Estrutura do Arquivo...")
	Processa( {|| _lProc := .t., _cNumPC:= A074CriaPed(aHead, aDet) },"Aguarde... Gerando Pedido de Compras...")

	If !("ERRO" $ upper(_cNumPC))

		If MsgYesNo("Importa��o realizada com sucesso!"+CRLF+" Deseja Visualizar o Pedido de Compras no ["+_cNumPC+"] ?")
			SC7->(dbSetOrder(1))
			If SC7->(MsSeek(xFilial('SC7')+_cNumPC))

				//Declara todas vari�veis privadas  para visualiza��o do pedido...
				Private nFuncao 		:= 1
				Private nTipoPed  	:= 1
				Private nReg     	:= 0
				Private nOpcX    	:= 2
				Private xFiller  	:= xFilial("SC7")
				Private l120Auto 	:= .F. 
				Private lCopia   	:= .F.
				Private lWhenGet 	:= .F. 
				Private aAutoCab  	:= {}
				PRIVATE aRotina	:= {}
				Private aBackSC7   := {}
				Private aAutoItens := Nil
				Private aRatCTBPC  := {}
				Private aAdtPC     := {}
				Private aRatProj   := {}

				Private nAutoAdt   := 0
				Private bFiltraBrw := {|| Nil }
				Private cCadastro  := "Pedidos de Compra"
				Private lPedido    := .T.
				Private lGatilha   := .T.                          // Para preencher aCols em funcoes chamadas da validacao (X3_VALID)
				Private lVldHead   := GetNewPar( "MV_VLDHEAD",.T. )// O parametro MV_VLDHEAD e' usado para validar ou nao o aCols (uma linha ou todo), a partir das validacoes do aHeader -> VldHead()

				aAdd(aRotina,{"Pesquisar","PesqBrw"   , 0, 1, 0, .F. }) //"Pesquisar"
				aAdd(aRotina,{"Visualizar","A120Pedido", 0, 2, 0, Nil }) //"Visualizar"
				aAdd(aRotina,{"Incluir","A120Pedido", 0, 3, 0, Nil }) //"Incluir"
				aAdd(aRotina,{"Alterar","A120Pedido", 0, 4, 6, Nil }) //"Alterar"
				aAdd(aRotina,{"Excluir","A120Pedido", 0, 5, 7, Nil }) //"Excluir"
				aAdd(aRotina,{"Copia","A120Copia" , 0, 4, 0, Nil }) //"Copia"
				aAdd(aRotina,{"Imprimir","A120Impri" , 0, 2, 0, Nil }) //"Imprimir"
				aAdd(aRotina,{"Legenda","A120Legend", 0, 1, 0, .F. }) //"Legenda"
				aAdd(aRotina,{"Conhecimento","MsDocument", 0, 4, 0, Nil }) //"Conhecimento"					

				MsgRun('Exibindo Pedido de Compras...',, {|| A120Pedido('SC7',SC7->(Recno()),2,xFiller,lCopia,lWhenGet)})

			Else
				Alert("Pedido de Compras n�o encontrado!")
			EndIf
		EndIf
	Else
		Alert("Inconsist�ncias foram encontradas na Planilha:"+CRLF+"Mensagem:"+CRLF+_cNumPC)
	EndIf
Else
	Alert("Processo cancelado!")
EndIf
Return(nil)

//-------------------------------------------------------------------
/*/ a074ReadFile - Leitura do Arquivo Texto delimitado por ;
@aParam
@author		Fernando
@since		20/03/2014
@uso        Especificos TV1

/*/
//--------------------------------------------------------------------
Static Function a074ReadFile(_cFile)
	Local _nLinha		:= 1
	Local _nCols		:= 1
	Local _cLinha 		:= ""
	Local _cFlag 		:= ""
	Local _aArea		:= GetArea()        
	Local _cContent 	:= ""
	Local cDelimiter	:= ";"
	Local nPosDelimiter	:= 0 
	Local aDados  		:= {Array(DEF_MAXCOLS)}

	If FT_FUse(_cFile) == -1
		Help(" ",1,"NOFILEIMPOR")
		RestArea(_aArea)
		Return(.f.)
	EndIf

	//Leitura do Arquivo Excel e armazanagem no Array aTXT
	FT_FGOTOP()
	Do While !FT_FEOF()

		_cLinha:= FT_FREADLN()

		_cFlag := _cLinha
		
		Do While At(cDelimiter,_cFlag) > 0 .and. _nCols <= DEF_MAXCOLS  // Montagem das Colunas
			
			nPosDelimiter := At(cDelimiter,_cFlag)
			If nPosDelimiter == 0
				Exit
			EndIf

			_cContent := Substr(_cFlag,1, nPosDelimiter-1)

			If Empty(_cContent)
				Exit
			Else
				aDados[len(aDados), _nCols] := _cContent
			EndIf

			_cFlag := Substr(_cFlag,nPosDelimiter+1, Len(_cFlag) )
			_nCols ++

		EndDo
		
		If _nCols == DEF_MAXCOLS .And. !Empty(_cFlag) .And. At(cDelimiter,_cFlag) == 0
			_cContent := Alltrim(_cFlag)
			aDados[len(aDados), _nCols] := _cContent
		EndIf
		
		_nLinha := 1
		_nCols := 1
		
		aAdd(aDados, Array(DEF_MAXCOLS))

		If Empty(_cLinha) // Fim da Planilha
			Exit
		EndIf
		FT_FSKIP()
	EndDo
	FT_FUSE()

Return(aDados)

static Function A074CriaPed(aHead, aDet)

	Local _aArea	 	:= GetArea()
	Local _nX		 	:= 0
	Local i				:= 0
	Local _cTarefa 		:= ""
	Local _cDescTar  	:= ""
	Local _cLink     	:= ""
	Local _cNumSc7   	:= ""
	Local _cMsgError 	:= ""
	Local _aCabSC7		:= {}
	Local _aItSC7	    := {}
	Local _lRet 	 	:= .t.
	Local _cTar			:= ""
	Local _cCodSA2		:= AllTrim(aHead[1])
	Local _cLojSA2 		:= '01'
	Local _cPgto 		:= '010'
	Local _cProd		:= _cProd := AvKey(AllTrim(aHead[2]),"B1_COD") 
	Local _aAF8 		:= {}
	Local _cRevisao 	:= ""
	Local _cObsApro 	:= ""
	Local _cObsPadrao	:= "Rotina: "+Alltrim(Funname())+" - Usu�rio: "+cUserName+" - "+ " Em:"+dtoc(dDataBase)+" �s "+Alltrim(Time())
	Local cQuery		:= ""
	Local lSeekSC7		:= .f.
	Local cValidSC7		:= 'Positivo() .And. A120Preco(M->C7_PRECO) .And. MaFisRef("IT_PRCUNI","MT120",M->C7_PRECO)'

	Private lMsErroAuto := .F.
	Private lMsHelpAuto := .T. 	// Vari�vel que define que o help deve ser gravado no arquivo de log e que as informa��es est�o vindo � partir da rotina autom�tica.
	Private lAutoErrNoFile:=.T. // For�a a grava��o das informa��es de erro em array para manipula��o da grava��o ao inv�s de gravar direto no arquivo tempor�rio.

	_cNumSc7 := GetNumSC7(.T.)
	Conout("CriaPed()")

	_nVlrSC7 := 0
	_aCabSC7 := {}
	_aCabSC7 := {;
				{"C7_NUM"	  , _cNumSC7		,NIL},;
				{"C7_EMISSAO" , dDataBase		,NIL},;
				{"C7_FORNECE" , _cCodSA2		,NIL},;
				{"C7_LOJA"	  , _cLojSA2     	,NIL},;
				{"C7_CONTATO" , "Contato"   	,NIL},;
				{"C7_COND"	  , _cPgto       	,NIL},;
				{"C7_FILENT"  , xFilial("SC7")  ,NIL},;
				{"C7_DESC1"   , 0.00,NIL}}

	For i := 2 To Len(aDet)

		_cBoleto  := aDet[i, DEF_BOLETO]

		If _cBoleto <> Nil .Or. !Empty(_cBoleto)

			_cProjeto := Troca(aDet[i, DEF_PSP], "C")

			DbSelectArea("AF8")
			AF8->(DbSetOrder(1))
			If AF8->(DbSeek(xFilial("AF8")+_cProjeto)) .and. !Empty(_cProjeto)
				_cDescPrj := AF8->AF8_DESCRI
				_cRevisao := AF8->AF8_REVISA
			Else
				_cMsgError := "Linha:"+cvaltochar(i)+CRLF+"PSP - "+_cProjeto+" n�o encontrado!"	+CRLF+"Favor ajustar na Planilha e fazer uma nova importa��o!"
				_lRet := .f.
				Exit
			EndIf

			_cCusto   := Troca(aDet[i, DEF_CCUSTO], "C")
			DbSelectArea("CTT")
			CTT->(DbSetOrder(1))
			If !CTT->(DbSeek(xFilial("CTT")+_cCusto)) .And. !Empty(_cCusto)
				_cMsgError := "Linha:"+cvaltochar(i)+CRLF+" Centro de Custo - "+_cCusto+" n�o encontrado!" +CRLF+ "Favor ajustar na Planilha e fazer uma nova importa��o!"
				_lRet := .f.
				Exit			
			EndIf

			_nVlrPC   := Troca(aDet[i,DEF_VALOR], "N")
			If _nVlrPC <=0
				_cMsgError := "Linha:"+cvaltochar(i)+CRLF+ "O Valor do Item [" +cvaltochar(i)+ "] da DESPESA dever� ser maior que zero!"
				_lRet := .f.
				Exit
			EndIf
			
			_cGrpSCR  := space(06)
			_cObsApro := ""
			_cObsApro := "No Boleto:"+Troca(aDet[i,DEF_BOLETO],"C"," - ")+"Origem:"+Troca(Alltrim(aDet[i,DEF_ORIGEM]),"C"," - ")  + "Destino:"+ Troca(aDet[i,DEF_DESTINO],"C"," - ")+ "Motivo:"+Troca(aDet[i,DEF_MOTIVO],"C"," - ")+ "Usuario:"+ Troca(aDet[i,DEF_USUARIO],"C"," - ")+ "Data/Hora:"+ Troca(aDet[i,DEF_DATAHORA],"C"," - ")

			DbSelectArea("SB1")
			SB1->(DbSetOrder(1))
			If !SB1->(DbSeek(xFilial("SB1")+AvKey(_cProd,"B1_COD")))

				_cMsgError := "Linha:"+cvaltochar(i)+CRLF+" Produto n�o encontrado - "+_cProd+" para o PSP - "+_cProjeto+"!"
				_cTar := ""				
				_lRet := .f.

				Exit

			Else
				
				_cTar := Alltrim(aDet[i,DEF_TAREFA])
				
				_cDescTar := Alltrim(Posicione("AF9",1, xFilial("AF9")+AvKey(_cProjeto,"AF8_PROJET")+AvKey(_cRevisao,"AF8_REVISA")+ AvKey(_cTar,"AF9_TAREFA"),"AF9_DESCRI"))
				If Empty(_cDescTar) .Or. Empty(_cTar)
					_cMsgError:= "Linha:"+cvaltochar(i)+CRLF+" Tarefa - "+_cTar+" n�o encontrada para o PSP - "+_cProjeto+"!"
					_cTar := ""				
					_lRet 	  := .f.
					Exit
				EndIf

				_aLinSC7 := {}

				aAdd(_aLinSC7,{"C7_ITEM"	, StrZero(i-1,4)	, NIL})
				aAdd(_aLinSC7,{"C7_PRODUTO"	, SB1->B1_COD		, NIL})
				aAdd(_aLinSC7,{"C7_QUANT"  	, 1					, NIL})
				aAdd(_aLinSC7,{"C7_QUJE"  	, 1					, NIL})
				aAdd(_aLinSC7,{"C7_PRECO"  	, Round(_nVlrPC,2)	,cValidSC7})
				aAdd(_aLinSC7,{"C7_TOTAL"  	, Round(_nVlrPC,2)	, NIL})
				aAdd(_aLinSC7,{"C7_TES"    	, "101"				,NIL})
				aAdd(_aLinSC7,{"C7_TIPO"   	, 1					,NIL})
				
				aAdd(_aLinSC7,{"C7_DESCRI"	, SB1->B1_DESC		,NIL})
				aAdd(_aLinSC7,{"C7_UM"		, SB1->B1_UM		,NIL})
				aADD(_aLinSC7,{"C7_LOCAL"	, SB1->B1_LOCPAD	,NIL})
				aADD(_aLinSC7,{"C7_CC"		, CTT->CTT_CUSTO	,NIL})
				
				aADD(_aLinSC7,{"C7_APROV"	, "TAX099"	,NIL})          //-->> Incluido Claudio 13/05/2024

				aADD(_aLinSC7,{"C7_USER"	, __cUserID	,NIL}) 
				aADD(_aLinSC7,{"C7_CONAPRO", 'L', NIL})
				aADD(_aLinSC7,{"C7_ENCER", 'E', NIL})

				aAdd(_aLinSC7,{"C7_DATPRF" 	 ,dDataBase	,NIL})
				aAdd(_aLinSC7,{"C7_DATAINI"	 ,dDataBase	,NIL})	
				
				aADD(_aLinSC7,{"C7_PROJET" 	, AvKey(_cProjeto,"AF8_PROJET")			,NIL})
				aADD(_aLinSC7,{"C7_CODPSP" 	, AvKey(_cProjeto,"AF8_PROJET")			,NIL})
				aADD(_aLinSC7,{"C7_DESCPRO"	, _cDescPrj			,NIL})
				aADD(_aLinSC7,{"C7_TAREFA" 	, AvKey(_cTar,"AF9_TAREFA"), NIL})
				aADD(_aLinSC7,{"C7_DESCTAR"	, _cDescTar			,NIL})					
				aADD(_aLinSC7,{"C7_MOEDA"	, 1	,NIL})	
				
				aADD(_aLinSC7,{"C7_OBS"	, _cObsPadrao, NIL})

				If SC7->(FieldPos("C7_X_OBDET")) > 0
					aADD(_aLinSC7,{"C7_X_OBDET"	, _cObsApro, NIL})
				EndIf

				If SC7->(FieldPos("C7_X_CMCNT")) > 0
					aADD(_aLinSC7,{"C7_X_CMCNT"	, 'TV1', NIL})
				EndIf

				aADD(_aLinSC7,{"C7_ORIGEM", 'IMFA074', NIL})
			
				aADD(_aItSC7,_aLinSC7)

			EndIf	
		EndIf	
	Next

	If _lRet

		lMsHelpAuto   := .F.
		lMsErroAuto   := .F.

		Begin Transaction

			nOpc := 03
			aArea := GetArea()

			MSExecAuto({|v,x,y,z,w| MATA120(v,x,y,z,w)},1,_aCabSC7,_aItSC7,nOpc,.F.)//Efetua a operacao
			
			RestArea(aArea)		

			If lMsErroAuto
				_lRet := .f.
				_cNumSC7:="ERRO"+Chr(13)+Chr(10)
				_cErrLog := ""
				MostraErro()

				//fun��o que retorna as informa��es de erro ocorridos durante o processo da rotina autom�tica
				aLog	:= {}
				aLog 	:= GetAutoGRLog()
				For _nX := 1 To Len(aLog)
					_cErrLog += Alltrim(aLog[_nX])+CHR(13)+CHR(10)
				Next _nX
				_cNumSC7 +="Erro na gera��o do Pedido de Compras..."+Chr(13)+Chr(10)+_cErrLog + Chr(13)+Chr(10) + "Contate o administrador do sistema!"

				DisarmTransaction()				

			Else
				_lRet := .T.			
			EndIf
		
		End Transaction
	Else
		_cNumSC7 := "ERRO " + _cMsgError
	EndIf
	RestArea(_aArea)

	If !_lRet
		Alert(_cNumSC7)
	// Else
	// 	Aviso( "Pedido de Compras!", " Pedido de Compras "+Alltrim(_cNumSC7)+" gerado com sucesso!", {"Ok"},2,,,"OK" )	
	EndIf

Return(_cNumSC7)

//-------------------------------------------------------------------
/*/
{Protheus.doc} Troca
Funcao para retornar os valores da forma correta
@aParam
@aRetorno
@author
@since
@obs		somente podera ser usado com valores
@uso        GENERICO
/*/
//--------------------------------------------------------------------
Static Function Troca(cCampo, cType, cCompl)
Local aArea		:= GetArea()
Local xRet 		:= 0          
Default cCompl 	:= ""

Do Case
	Case cType == "N" 

		cCampo := StrTran(cCampo,".","")
		cCampo := StrTran(cCampo,",",".")
		xRet := Val(cCampo)

	Case cType == "D" 		
		cCampo := dTos(stod(cCampo))
		xRet := cCampo
		
	Case cType == "C"
		cCampo := StrTran(cCampo," ' ", "  " ) 
		cCampo := Alltrim(cCampo)
		If !Empty(cCompl)
			xRet := cCampo + cCompl
		Else
		 	xRet := cCampo
		EndIf 		
EndCase
RestArea(aArea)
Return(xRet)


USER FUNCTION A74VLDUSR(campo, tipo)
	Local LRET := .T.
	Local xConteudo := &("M->"+campo)
	
	conout("Campo obrigat�rio --> " + campo)
	Do Case
		Case tipo == "C"
			If( Empty(xConteudo) )
				conout("Campo obrigat�rio n�o preenchido! " + campo)
			EndIf
	EndCase

RETURN(LRET)

// User Function POSAVE()

// 	Local _aArea	 	:= GetArea()
// 	Local _nX		 	:= 0
// 	Local i				:= 0
// 	Local _cTarefa 		:= ""
// 	Local _cDescTar  	:= "COMPRAS N�O PREVISTAS"
// 	Local _cLink     	:= ""
// 	Local _cNumSc7   	:= ""
// 	Local _cMsgError 	:= ""
// 	Local _aDadSC7	:= {}
// 	Local _aCabSC7	:= {}
// 	Local _aItSC7	    := {}
// 	Local _lRet 	 	:= .t.
// 	Local _cTar		:= ""
// 	Local _cCodSA2	:= "019552"
// 	Local _cLojSA2 	:= '01'
// 	Local _cPgto 		:= '010'
// 	Local _cProd		:= ""
// 	Local _aAF8 		:= {}
// 	Local _cRevisao 	:= ""
// 	Local _cObsApro 	:= ""
// 	Local _cObsPadrao	:= "Rotina: "+Alltrim(Funname())+" - Usu�rio: "+cUserName+" - "+ " Em:"+dtoc(dDataBase)+" �s "+Alltrim(Time())
// 	Local cQuery		:= ""
// 	Local cAliasTrb	:= getNextAlias()
// 	Local RecSC7		:= 0
// 	Local lSeekSC7	:= .f.
// 	Local cValidSC7	:= 'Positivo() .And. A120Preco(M->C7_PRECO) .And. MaFisRef("IT_PRCUNI","MT120",M->C7_PRECO)'
// 	Local nVlrPed		:= 100
// 	Local lItemOk		:= .t.
// 	Private lMsErroAuto := .F.
// 	Private lMsHelpAuto := .T. 	// Vari�vel que define que o help deve ser gravado no arquivo de log e que as informa��es est�o vindo � partir da rotina autom�tica.
// 	Private lAutoErrNoFile:=.T. // For�a a grava��o das informa��es de erro em array para manipula��o da grava��o ao inv�s de gravar direto no arquivo tempor�rio.
	
// 	public lCop:=.F. // Flag para n�o entrar na tela de aprova��o de pedidos...

//     Default nOpc := 03

// 	_cNumSc7 := GetNumSC7(.T.)	

// 	//u_A009X3() //Edita SX3 - X3_PRECO

// 	_nVlrSC7 := 0
// 	_aCabSC7 := {}
// 	_aCabSC7 := {;
// 				{"C7_NUM"	 	, _cNumSC7		,NIL},;
// 				{"C7_EMISSAO" , dDataBase		,NIL},;
// 				{"C7_FORNECE" , "019552"		,NIL},;
// 				{"C7_LOJA"	 	, "01"          ,NIL},;
// 				{"C7_CONTATO" , "."      	  	,NIL},;
// 				{"C7_COND"	 	, "010"       	,NIL},;
// 				{"C7_FILENT"  , xFilial("SC7") ,NIL},;
// 				{"C7_DESC1"   , 0.00,NIL}}

// 	_cCusto := "090101     " // MIDIA.

// 	_nVlrPC   := 100
// 	_cGrpSCR  := SPACE(06) 	//_cGrpSCR  := "000001"	

// 	_cObsApro := "Pedido de Compra gerado via inclus�o de Pedido de Inser��o: "
			
// 	_cProjeto 	:= "09U0000048"
// 	_cDescPrj	:= "PROJETO 09U0000048"
// 	_cTar		:= Alltrim("2.01")
// 	_nVlrComVeiculo := 0
		
// 	For i :=  1 to 1
		
// 		lItemOk := .t.
		
//     	If !lItemOk
// 			Loop
// 		Else	

// 			_cProd := AvKey("0000000294","B1_COD")
	
// 			DbSelectArea("SB1")
// 			SB1->(DBSETORDER(1))
// 			//if SB1->( DBSEEK(XFILIAL("SB1") +"0000004677     ") )
// 			if SB1->( DBSEEK(XFILIAL("SB1") +_cProd) )
// 				alert('achou produto! STATUS = '+ SB1->B1_MSBLQL)
// 			Else
// 				alert('produto n�o localizado!')
// 			EndIf

// 			_aLinSC7 := {}
		
// 			aAdd(_aLinSC7,{"C7_ITEM"	, "0001"	,NIL})
// 			aAdd(_aLinSC7,{"C7_PRODUTO"	, SB1->B1_COD ,NIL})
// 			aAdd(_aLinSC7,{"C7_QUANT"  	, 1 /*_aColsAFA[i, GdFieldPos("AFA_QUANT",aHeaderSV[1])]*/  ,NIL})
// 			aAdd(_aLinSC7,{"C7_QUJE"  	, 1 /*_aColsAFA[i, GdFieldPos("AFA_QUANT",aHeaderSV[1])]*/	,NIL})
// 			aAdd(_aLinSC7,{"C7_PRECO"  	, Round(nVlrPed,2) /*Round(_aColsAFA[i, GdFieldPos("AFA_CUSTD",aHeaderSV[1])],2)*/,cValidSC7})
// 			aAdd(_aLinSC7,{"C7_TOTAL"  	, Round(nVlrPed,2) 	,NIL})
// 			aAdd(_aLinSC7,{"C7_TES"    	, "101"				,NIL})
// 			aAdd(_aLinSC7,{"C7_TIPO"		, 1						,NIL})//1-Pedido|2-Aut.Entrega
// 			aAdd(_aLinSC7,{"C7_DESCRI"	, SB1->B1_DESC		,NIL})
// 			aAdd(_aLinSC7,{"C7_UM"		, SB1->B1_UM			,NIL})
// 			aADD(_aLinSC7,{"C7_LOCAL"	, SB1->B1_LOCPAD		,NIL})
// 			aADD(_aLinSC7,{"C7_CC"		, _cCusto				, NIL})
		
// 			//aADD(_aLinSC7,{"C7_APROV"	, _cGrpSCR				,NIL})
// 			//aADD(_aLinSC7,{"C7_GRUPCOM"	, _cGrpSCR				,NIL})
		
// 			aADD(_aLinSC7,{"C7_USER"  	, __cUserID			,NIL})
// 			aADD(_aLinSC7,{"C7_CONAPRO"	,'L'					,NIL})
// 			aADD(_aLinSC7,{"C7_ENCER" 	,'E'					,NIL})
// 			aADD(_aLinSC7,{"C7_DATPRF" 	,dDataBase				,NIL})
// 			aADD(_aLinSC7,{"C7_DATAINI"	,dDataBase				,NIL})
// 			aADD(_aLinSC7,{"C7_PROJET" 	, AvKey(_cProjeto,"AF8_PROJET") ,NIL})
// 			aADD(_aLinSC7,{"C7_CODPSP" 	, AvKey(_cProjeto,"AF8_PROJET")	,NIL})
// 			aADD(_aLinSC7,{"C7_DESCPRO"	, _cDescPrj			,NIL})
// 			aADD(_aLinSC7,{"C7_TAREFA" 	, AvKey(_cTar,"AF9_TAREFA") 				,NIL})
// 			aADD(_aLinSC7,{"C7_DESCTAR"	, _cDescTar			,NIL})
// 			aADD(_aLinSC7,{"C7_MOEDA"	, 1             		,NIL})
					
// 			aADD(_aLinSC7,{"C7_OBS"		, _cObsPadrao			,NIL})
		
// 			If SC7->(FieldPos("C7_X_OBDET")) > 0
// 				aADD(_aLinSC7,{"C7_X_OBDET"	, _cObsApro		,NIL})
// 			EndIf

// 			If SC7->(FieldPos("C7_X_CMCNT")) > 0
// 				aADD(_aLinSC7,{"C7_X_CMCNT"	, 'TV1'			,NIL})
// 			EndIf
// 			aAdd(_aItSC7,_aLinSC7)
// 		EndIf
// 	Next i

// 	If _lRet
	
// 		lMsHelpAuto   := .F.
// 		lMsErroAuto   := .F.
	
// 		Begin Transaction

// 			aArea := GetArea()
// 			MSExecAuto({|v,x,y,z,w| MATA120(v,x,y,z,w)},1,_aCabSC7,_aItSC7,nOpc,.F.)//Efetua a operacao
			
// 			RestArea(aArea)		

// 			If lMsErroAuto
// 				_lRet := .f.
// 				_cNumSC7:="ERRO"+Chr(13)+Chr(10)
// 				_cErrLog := ""

// 				//fun��o que retorna as informa��es de erro ocorridos durante o processo da rotina autom�tica
// 				aLog	:= {}
// 				aLog 	:= GetAutoGRLog()
// 				For _nX := 1 To Len(aLog)
// 					_cErrLog += Alltrim(aLog[_nX])+CHR(13)+CHR(10)
// 				Next _nX
// 				_cNumSC7 +="Erro na gera��o do Pedido de Compras..."+Chr(13)+Chr(10)+_cErrLog + Chr(13)+Chr(10) + "Contate o administrador do sistema!"

// 				AUTOGRLOG(TIME())
// 				AUTOGRLOG(_cErrLog)

// 				MostraErro()				

// 				DisarmTransaction()				

// 			Else
// 				_lRet := .T.			
// 			EndIf
	    
// 		End Transaction
// 	Else
// 		_cNumSC7 := "ERRO " + _cMsgError
// 	EndIf
// 	RestArea(_aArea)
	
// 	If lMsErroAuto
// 		Alert(_cNumSC7)
// 	Else
// 		If INCLUI
// 			Aviso( "Pedido de Compras!", " Pedido de Compras "+Alltrim(_cNumSC7)+" gerado com sucesso!", {"Ok"},2,,,"OK" )	
// 		ElseIf ALTERA
// 			If !lSeekSC7
// 				Aviso( "Pedido de Compras!", " Pedido de Compras "+Alltrim(_cNumSC7)+" gerado com sucesso!", {"Ok"},2,,,"OK" )			
// 			Else
// 				Aviso( "Pedido de Compras!", " Pedido de Compras "+alltrim(_cNumSC7)+" Alterado com sucesso!", {"Ok"},2,,,"OK" )
// 			EndIf	
// 		EndIf	
// 	EndIf
// Return(.T.)