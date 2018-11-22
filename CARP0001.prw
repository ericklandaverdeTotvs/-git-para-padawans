#Include "Protheus.Ch"
#Include "TopConn.Ch"

/*ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Empresa   ³ TOTVS Mexico S.A. de C.V.                                  ³±±
±±³          ³ Privada de Los Industriales # 110 int 106                  ³±±
±±³          ³ Zona Ind. Benito Juarez  -   Queretaro - Qro - Mexico      ³±±
±±³          ³ Fone: +52 442 103-0600   -   Site: www.totvs.com           ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ´±±
±±³Programa  ³ CARP0001  ³ Autor ³ Wellington Santos    ³ Data ³03/09/2010³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descricao ³ Rehace los Acumulados de la Nomina                         ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Sintaxe   ³ CARP0001(void)                                             ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Parametros³                                                            ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Retorno   ³                                                            ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Uso       ³ Gestion de Personal - Protheus 10 - Exclusivo Carso        ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³  Fecha   ³   Analista    ³Alteraciones efetuadas                      ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³  /  /    ³               ³                                            ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß*/
User Function CARP0001
Local cCadastro 	:= "Reconstrucción de Acumulados"
Local lOk 		:= .F.
Local oFont		:= TFont():New("MS Sans Serif",8,8,,.T.,,,,.F.,.F.)

Private oDlg, oProceso, oDescProc, oEmpleado, oNomEmpl, oPD, oDescPD, oPerIni, oPerFin
Private oChkBox01, oChkBox02, oChkBox03, oChkBox04, oChkBox05, oChkBox06, oChkLmp
Private oChkBox07, oChkBox08, oChkBox09, oChkBox10, oChkBox11, oChkBox12, oChkBox13
Private lChk01, lChk02, lChk03, lChk04, lChk05, lChk06, lChkLmp
Private lChk07, lChk08, lChk09, lChk10, lChk11, lChk12, lChk13
Private cProceso  := CriaVar("RG7_PROCES"	,.F.) 				// Proceso
Private cDescProc, cNomEmpl, cDescPD 						// Descripciones
Private cEmpleado	:= CriaVar("RA_MAT"		,.F.) 			// Codigo del Empleado
Private cNomEmpl	:= CriaVar("RA_NOME"		,.F.) 			// Nombre del Empleado
Private cPD 	:= CriaVar("RG7_PD"		,.F.) 			// Concepto
Private cAno 	:= StrZero( Year(dDataBase), 4 )
Private cPerIni 	:= ""
Private cPerFin 	:= ""
Private aPerIni 	:= fCargaPer()
Private aPerFin 	:= fCargaPer()

Define MsDialog oDlg Title cCadastro Of oMainWnd Pixel From 0,0 To 320,500 COLOR 0, 16777215

@ 015,002 To 065,250 Of oDlg Pixel

// Proceso
@ 019,005 Say RetTitle("RG7_PROCES")	Size  50,09 Of oDlg Pixel Font oFont
@ 018,050 MsGet cProceso F3 "RCJ" 		Size  20,09 Of oDlg Pixel Font oFont;
	Valid( ValidaProc( &(ReadVar()) ) )

// Descripcion del Proceso	
@ 019,105 Say RetTitle("RCJ_DESCRI")	Size  50,09 Of oDlg Pixel Font oFont
@ 018,145 Get oDescProc Var cDescProc	Size 100,09 Of oDlg Pixel Font oFont When .F.
	
// Codigo del Empleado
@ 035,005 Say RetTitle("RG7_MAT")		Size  50,09 Of oDlg Pixel Font oFont
@ 034,050 MsGet cEmpleado F3 "SRA" 		Size  20,09 Of oDlg Pixel Font oFont;
	Valid( ValidaEmp( &(ReadVar()) ) )

// Nome del Empleado
@ 035,105 Say RetTitle("RA_NOME")		Size  50,09 Of oDlg Pixel Font oFont
@ 034,145 Get oNomEmpl Var cNomEmpl		Size 100,09 Of oDlg Pixel  Font oFont When .F.

// Concepto
@ 051,005 Say RetTitle('RG7_PD')		Size  50,09 Of oDlg Pixel Font oFont
@ 050,050 MsGet cPD F3 "SRV" 			Size  20,09 Of oDlg Pixel Font oFont;
	Valid( ValidaPD( &(ReadVar()) ) )

// Descripcion del Concepto
@ 051,105 Say RetTitle("RV_DESC")		Size  50,09 Of oDlg Pixel Font oFont
@ 050,145 Get oDescPD Var cDescPD		Size 100,09 Of oDlg Pixel  Font oFont When .F.


@ 069,002 To 095,250 Of oDlg Pixel
@ 071,005 Say "Año"                 	Size  50,09 Of oDlg Pixel Font oFont
@ 080,005 MsGet cAno   Picture "9999"	Size  20,09 Of oDlg Pixel Font oFont;
		Valid( ValidAno( &(ReadVar()) ) )

@ 071,050 Say 'Periodo Inicial' 		Size  50,09 Of oDlg Pixel Font oFont
@ 080,050 MSCOMBOBOX oPerIni VAR cPerIni ITEMS aPerIni SIZE 070,009 Of oDlg Pixel Font oFont;
	On Change ( oPerFin:aItems := fCargaPer(2), oPerFin:nAt := 0 )

@ 071,145 Say 'Periodo Final' 		Size  50,09 Of oDlg Pixel Font oFont
@ 080,145 MSCOMBOBOX oPerFin VAR cPerFin ITEMS aPerFin SIZE 070,009 Of oDlg Pixel Font oFont


@ 098,002 To 155,250 Label "Meses a reconstruir" Of oDlg Pixel
@ 110,005 CHECKBOX oChkBox01 VAR lChk01 PROMPT "Mes 01" 	SIZE 035, 009 OF oDlg Pixel Font oFont
@ 110,045 CHECKBOX oChkBox02 VAR lChk02 PROMPT "Mes 02" 	SIZE 035, 009 OF oDlg Pixel Font oFont
@ 110,085 CHECKBOX oChkBox03 VAR lChk03 PROMPT "Mes 03" 	SIZE 035, 009 OF oDlg Pixel Font oFont
@ 110,125 CHECKBOX oChkBox04 VAR lChk04 PROMPT "Mes 04" 	SIZE 035, 009 OF oDlg Pixel Font oFont
@ 110,165 CHECKBOX oChkBox05 VAR lChk05 PROMPT "Mes 05" 	SIZE 035, 009 OF oDlg Pixel Font oFont
@ 110,205 CHECKBOX oChkBox06 VAR lChk06 PROMPT "Mes 06" 	SIZE 035, 009 OF oDlg Pixel Font oFont

@ 125,005 CHECKBOX oChkBox07 VAR lChk07 PROMPT "Mes 07" 	SIZE 035, 009 OF oDlg Pixel Font oFont
@ 125,045 CHECKBOX oChkBox08 VAR lChk08 PROMPT "Mes 08" 	SIZE 035, 009 OF oDlg Pixel Font oFont
@ 125,085 CHECKBOX oChkBox09 VAR lChk09 PROMPT "Mes 09" 	SIZE 035, 009 OF oDlg Pixel Font oFont
@ 125,125 CHECKBOX oChkBox10 VAR lChk10 PROMPT "Mes 10" 	SIZE 035, 009 OF oDlg Pixel Font oFont
@ 125,165 CHECKBOX oChkBox11 VAR lChk11 PROMPT "Mes 11" 	SIZE 035, 009 OF oDlg Pixel Font oFont
@ 125,205 CHECKBOX oChkBox12 VAR lChk12 PROMPT "Mes 12" 	SIZE 035, 009 OF oDlg Pixel Font oFont

@ 140,005 CHECKBOX oChkBox13  VAR lChk13  PROMPT "Mes 13" 	SIZE 035, 009 OF oDlg Pixel Font oFont
@ 142,090 SAY "Seleccione el mes a procesar o ningun para TODOS" SIZE 150, 009 Of oDlg Pixel Font oFont COLOR CLR_HBLUE
//@ 140,085 CHECKBOX oChkBoxLmp VAR lChkLmp PROMPT "Limpiar Acumulados Antes de Reconstruir" SIZE 150, 009 OF oDlg Pixel Font oFont

bOk	:= {|| lOk := ( Aviso( cCadastro, "Confirma la reconstrucción de los acumulados", {"Confirma","Anula"}, 1, "¿ Confirma reconstrucción ?" ) == 1 ), oDlg:End() }
bCanc	:= {|| oDlg:End() }

Activate MsDialog oDlg Center On Init EnchoiceBar( oDlg, bOk, {||oDlg:End()} )

If lOk
	Processa( {|| fGenAcum()},"Generando acumulados erick landaverde" ,"Aguarde..." )
EndIf

Return


/*ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºFuncion   ³FCARGAPER ºAutor  ³Wellington Santos   ºFecha ³  03/09/2010 º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³                                                            º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß*/
Static Function fCargaPer( nCombo )
Local aArea 	:= GetArea()
Local aPer  	:= {}
Local cQuery  	:= ""

DEFAULT nCombo := 1

cQuery := "SELECT RCH_PER "
cQuery += "FROM " +RetSQLName("RCH")+ " RCH "
cQuery += "WHERE RCH_FILIAL = '" +xFilial("RCH")+ "' "
cQuery += "  AND RCH_DTFECH <> '' "
If !Empty( cAno )
	cQuery += "  AND RCH_ANO = '" +cAno+ "' "
EndIf
If !Empty( cProceso )
	cQuery += "  AND RCH_PROCES = '" +cProceso+ "' "
EndIf
If nCombo <> 1 .and. !Empty(cPerIni)
	cQuery += "  AND RCH_PER >= '" +cPerIni+ "' "
EndIf

cQuery += "  AND RCH.D_E_L_E_T_ = ' ' "
cQuery += "GROUP BY RCH_PER "

If Select( "cQry" ) > 0 	; 	cQry->( dbCloseArea() ) 	; 	EndIf

TCQuery cQuery New Alias "cQry"

Count To nReg
cQry->( dbGoTop() )
	
Do While cQry->( !Eof() )
	aadd( aPer, cQry->RCH_PER )
	
	cQry->( dbSkip() )
EndDo

cQry->( dbCloseArea() )

RestArea( aArea )

Return( aPer )


/*ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºFuncion   ³VALIDAPROCºAutor  ³Wellington Santos   ºFecha ³  03/09/2010 º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³                                                            º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß*/
Static Function ValidaProc( cProceso )
Local lRet := .T.

If !Empty( cProceso )
	If !ExistCpo( "RCJ", cProceso )
		lRet := .F.
	EndIf
	
	If lRet
		cDescProc := RCJ->RCJ_DESCRI
		oDescProc:Refresh()
		
		oPerIni:aItems 	:= fCargaPer()
		oPerIni:nAt 	:= 0
		cPerIni 		:= ""
		cPerFin 		:= ""
	EndIf
EndIf

Return( lRet )


/*ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß*/
Static Function ValidaEmp( cEmpleado )
Local lRet := .T.

If !Empty( cEmpleado )
	If !ExistCpo( "SRA", cEmpleado )
		lRet := .F.
	EndIf
	
	If lRet
		cNomEmpl := Posicione("SRA",1,xFilial("SRA")+cEmpleado,"RA_NOME")
		oNomEmpl:Refresh()
	EndIf
EndIf

Return( lRet )


/*ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß*/
Static Function ValidaPD( cPD )
Local lRet := .T.

If !Empty( cPD ) 
	If !ExistCpo( "SRV", cPD )
		lRet := .F.
	EndIf
	
	If lRet
		cDescPD := Posicione( "SRV", 1, xFilial("SRV") + cPD, "RV_DESC" )
		oDescPD:Refresh()
	EndIf
EndIf

Return( lRet )


/*ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß*/
Static Function ValidAno( cAno )
Local lRet := .T.

If !Empty( cAno ) 
	// Carga Periodos
	oPerIni:aItems 	:= fCargaPer()
	oPerIni:nAt 	:= 0
	cPerIni 		:= ""
	cPerFin 		:= ""
EndIf

Return( lRet )

/*ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºFuncion   ³ fGenAcum ºAutor  ³Wellington Santos   ºFecha ³  03/09/2010 º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³                                                            º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß*/
Static Function fGenAcum
Local aArea 	:= GetArea()
Local cQuery 	:= ""
Local cMeses 	:= ""
Local nRegs 	:= 0
Local nCount 	:= 0
Local cCount 	:= ""
Local cHrs 		:= ""
Local cAcum 	:= ""
Local cVar 		:= ""
Local cRoteir 	:= CriaVar( "RG7_ROTEIR", .F. )
Local i

For i = 1 To 13
	cVar := "lChk" + StrZero(i,2)
	If &(cVar)
		cMeses += Iif( Empty(cMeses),"'" , ",'" ) + StrZero(i,2) + "'"
	EndIf
Next i
alert("Se modifico el fuente?")

cQuery := "SELECT RD_FILIAL, RD_MAT, RD_PROCES, RD_PD, SUM(RD_HORAS) HORAS, SUM(RD_VALOR) VALOR "
cQuery += ", SubString(RD_DATARQ,1,4) AS ANO, RD_MES "
cQuery += "FROM " +RetSQLName("SRD")+ " SRD "
cQuery += "   INNER JOIN " +RetSQLName("SRM")+ " SRM ON (RM_PROCES = RD_PROCES AND RM_CALCULO = RD_ROTEIR "
cQuery += "                                          AND RM_VERBA = RD_PD ) "
cQuery += "WHERE RD_FILIAL = '" +xFilial("SRD")+ "' "
If !Empty( cProceso )
	cQuery += "  AND RD_PROCES  = '" +cProceso+ "' "
EndIf
If !Empty( cPD )
	cQuery += "  AND RD_PD      = '" +cPD+ "' "
EndIf
If !Empty( cEmpleado )
	cQuery += "  AND RD_MAT     = '" +cEmpleado+ "' "
EndIf
If !Empty( cMeses )
	cQuery += "  AND RD_MES     IN (" +cMeses+ ") "
EndIf
If !Empty( cPerIni ) .and. !Empty( cPerFin )
	cQuery += "  AND RD_PERIODO BETWEEN '" +cPerIni+ "' AND '" +cPerFin+ "' "
EndIf
cQuery += "  AND RM_GERAACU = '1' "
cQuery += "  AND SRD.D_E_L_E_T_ = ' ' "
cQuery += "  AND SRM.D_E_L_E_T_ = ' ' "
cQuery += "GROUP BY RD_FILIAL, RD_MAT, RD_PROCES, RD_PD, SubString(RD_DATARQ,1,4), RD_MES "
cQuery += "ORDER BY RD_FILIAL, RD_MAT, RD_PROCES, RD_PD, SubString(RD_DATARQ,1,4), RD_MES "

cQuery := ChangeQuery( cQuery )

TCQuery cQuery New Alias "qSRD"
Count To nRegs
If nRegs > 0
	ProcRegua( nRegs )
	qSRD->( dbGoTop() )
	RG7->( dbSetOrder(1) ) // Filial + Mat + Concepto + Proceso + Roteiro + AnoFim
	nCount := 0
	//Begin TransAction
		Do While qSRD->( !Eof() )
		
			nCount ++
			cCount := AllTrim(Str(nCount)) + "/" + AllTrim(Str(nRegs))
			
			IncProc( "Procesando: " + qSRD->RD_MAT + "... " + cCount )
			
			cHrs 		:= "RG7->RG7_HRS" + qSRD->RD_MES
			cAcum 	:= "RG7->RG7_ACUM" + qSRD->RD_MES
			
			// Busca registro en la RG7
			If RG7->( dbSeek( xFilial("RG7")+qSRD->RD_MAT+qSRD->RD_PD+qSRD->RD_PROCES+cRoteir+qSRD->ANO ) )
				RecLock( "RG7", .F. )
					&(cHrs) 	:= qSRD->HORAS
					&(cAcum) 	:= qSRD->VALOR
					RG7->RG7_DTATUA 	:= DTOC(dDataBase) +" "+  Time() // 01/01/2001 01:00:00
				RG7->( MsUnLock() )
			Else
				RecLock( "RG7", .T. )
					RG7->RG7_FILIAL 	:= xFilial("RG7")
					RG7->RG7_PROCES 	:= qSRD->RD_PROCES
					RG7->RG7_ROTEIR 	:= cRoteir
					RG7->RG7_MAT 	:= qSRD->RD_MAT
					RG7->RG7_PD 	:= qSRD->RD_PD
					RG7->RG7_ANOINI 	:= qSRD->ANO
					RG7->RG7_ANOFIM 	:= qSRD->ANO
					RG7->RG7_CODCRI 	:= "01"
					RG7->RG7_SEQACU 	:= "01"
					&(cHrs) 		:= qSRD->HORAS
					&(cAcum) 		:= qSRD->VALOR
					RG7->RG7_DTATUA 	:= DTOC(dDataBase) +" "+  Time() // 01/01/2001 01:00:00
				RG7->( MsUnLock() )
			EndIf
			
			qSRD->( dbSkip() )
		EndDo
	//End TransAction
	
	MsgInfo( "Atualización concluida !" )

Else
	MsgInfo( "No se encontraron movimentos !" )
EndIf

qSRD->( dbCloseArea() )


RestArea( aArea )

Return