$PROBLEM 2-compartment model
$INPUT ID ARM TIME EVID MDV DV AMT CMT
$DATA dataset.csv IGNORE=I
$SUBROUTINE ADVAN4 TRANS4
$PK
KA = THETA(1)*EXP(ETA(1))
CL = THETA(2)*EXP(ETA(2) + IOV_CL)
V2 = THETA(3)*EXP(ETA(3))
V3 = THETA(4)*EXP(ETA(4))
Q = THETA(5)*EXP(ETA(5))
S2 = V2
$ERROR 
 CP = F
 OBS_CP = CP *(1+EPS(1))
 Y = OBS_CP
$THETA 1     ; KA 
$THETA 5     ; CL 
$THETA 80    ; V2 
$THETA 20    ; V3 
$THETA 4     ; Q
$OMEGA 0.09 FIX ; KA
$OMEGA 0.09 FIX ; CL
$OMEGA 0.09 FIX ; V2
$OMEGA 0.09 FIX ; V3
$OMEGA 0.09 FIX ; Q
$SIGMA 0.09 FIX ; PROP

$SIMULATION (1234) ONLYSIM NSUB=1
$TABLE ID ARM TIME EVID MDV DV AMT CMT CP FILE=output.tab ONEHEADER NOAPPEND NOPRINT

