//#INCLUDE "transit.ch"
#INCLUDE "Totvs.ch"
//#INCLUDE "FiveWin.ch"
#include "protheus.ch"
 
//-------------------------------------------------------------------
/*/ {Protheus.doc} RDVTVR01
Programa para gravacao dos dados de dias adicionais dos Benefícios
@aParam
@author		Marcos Kato
@since		14/09/2011
@uso        
/*/
//--------------------------------------------------------------------
User Function RDVTVR01(void)

Private cCadastro := OemToAnsi("Beneficios Adicionais") 
Private aRotina := { {"Pesquisar","AxPesqui", 0 , 1,,.F.},;		//Pesquisar
					{ "Visualizar","AxVisual", 0 , 2,,.F.},;		//Visualizar
					{ "Incluir","AxInclui", 0 , 3,,.F.},;		//Incluir
					{ "Alterar","AxAltera", 0 , 4,,.F.},;		//Alterar
					{ "Excluir","AxDeleta", 0 , 5,,.F.}}	    //Excluir
					//{ STR0006,"RDLEGGPE"  ,0,6}}		//Legenda								

If FindFunction("U_IMFA018") 
	u_IMFA018("RDVTVR01","Beneficios Adicionais")		
Endif	

DBSelectArea("Z01")
Z01->(DBSetOrder(1))
Z01->(DBSeek(xFilial("Z01")))
Z01->(mBrowse( 6, 1,22,75,"Z01",,,,,,))

Return(.T.)
