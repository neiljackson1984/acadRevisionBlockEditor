;***************************************|     edit the values below as desired   |******************************************* 
;***************************************| \/   \/   \/    \/    \/  \/   \/  \/  |*******************************************
(setq newRevisionFields
	(list 
		(cons     ""                     "3"                                       )  ;;this is the revision number
		(cons     "DATE"                 "01/04/2018"                              )
		(cons     "BY"                   "JESUS CHRIST"                            )
		(cons     "DESCRIPTION"          "MADE SOME MINOR CHANGES"                 )
		(cons     "APV"                  "GOD THE FATHER"                          )
	)
)








;*************************************************************************************************************************
; DO NOT MODIFY BELOW 
;=========================================================================================================================


















(defun doit ( /

	)

	(vlax-for layout (vla-get-Layouts (vla-get-ActiveDocument (vlax-get-acad-object))) 
		(if (/= (vla-get-ObjectID (vla-get-Block layout)) (vla-get-ObjectID (vla-get-ModelSpace (vla-get-Document layout))))  ;; if this layout is not the model layout (i.e. is a "paper space" layout.)
			(progn
				(princ "now processing ")(princ (vla-get-Name layout)) (princ "\n")
				(vlax-for entity (vla-get-Block layout)
					(if (= (vla-get-ObjectName entity) "AcDbBlockReference")
						(progn
							(if (= (vla-get-EffectiveName entity) "SHT_TITL_wRev_30X42")
								(progn
									(setq theTitleBlock entity)
									;; find first empty attribute value having tag of the form Rn, where n is a number
									(setq rNumberOfRevision3 nil)
									(setq rNumberOfTargetRevisionValue nil)
									(setq targetRevisionValue (cdr (assoc "" newRevisionFields)))
									(setq rNumber 1)
									(while (and (/= (getAttributeValue theTitleBlock (strcat "R" (itoa rNumber)) ) "") (<= rNumber 12) )
										(if (= (vl-string-trim " \t\n" (getAttributeValue theTitleBlock (strcat "R" (itoa rNumber)) )) "3")
											(progn 
												(setq rNumberOfRevision3 rNumber)
											)
										)
										(if (= (vl-string-trim " \t\n" (getAttributeValue theTitleBlock (strcat "R" (itoa rNumber)) )) (vl-string-trim " \t\n" targetRevisionValue))
											(progn 
												(setq rNumberOfTargetRevisionValue rNumber)
											)
										)
										(setq rNumber (+ rNumber 1))
									)
									(setq firstEmptyRevisionRowNumber rNumber)
									(setq lastNonEmptyRevisionRowNumber (- rNumber 1))	
									(princ "firstEmptyRevisionRowNumber: ")(princ firstEmptyRevisionRowNumber)(princ "\n")
									(princ "lastNonEmptyRevisionRowNumber: ")(princ lastNonEmptyRevisionRowNumber)(princ "\n")
									(princ "rNumberOfRevision3: ")(princ rNumberOfRevision3)(princ "\n")
									(princ "rNumberOfTargetRevisionValue: ")(princ rNumberOfTargetRevisionValue)(princ "\n")
									
									
									(if nil ;;set the fields in the last nonempty row of the revision table to emtpy strings.
										(progn 
											(if rNumberOfRevision3
												(progn
													;;we are setting the row containing revision 3 stuff to have all empty strings as values.
													(mapcar
														'(lambda (x) (setAttributeValue  theTitleBlock  x ""))
														(mapcar 
															'(lambda (x) (strcat "R" (itoa rNumberOfRevision3) x))
															(list "" "DATE" "BY" "DESCRIPTION" "APV")
														)
													)
												)
											)	
										)
									)
									
									
									(if T ;; add the new revision values
										(progn 
											( if rNumberOfTargetRevisionValue ;;if there was  already a row for the revision we wanted to add
												(progn
													(princ "Row ")(princ rNumberOfTargetRevisionValue)(princ " of the revision table already contains the revision number that you want to add, so we will not make any changes. Call Neil for help.  Good bye.")(princ "\n")												)
												(progn
													(princ "now adding your new revision table to the first empty row of the revision table")(princ "\n")
													(foreach nameValuePair newRevisionFields
														(setAttributeValue  theTitleBlock  
															(strcat "R" (itoa firstEmptyRevisionRowNumber) (car nameValuePair))
															(cdr nameValuePair)
														)
													)
												)
											)
										)
									)
								)
							)
						)
					)
				)
			)
			(progn
				(princ "skipping layout ")(princ (vla-get-Name layout))(princ " because it is the \"Model\" layout")
			)
		)
	)
)


(defun setAttributeValue
	(
		theBlockReference
		tag
		newValue
		/
		theBlockReference
		attributeReferences
		theAttributeReference
	)
	(setq attributeReferences
		(gc:VariantToLispData 
			(vla-GetAttributes theBlockReference)
		)
	)
	(foreach x attributeReferences
		(if (= tag (vla-get-TagString x))
			(setq theAttributeReference x)
		)
	)
	(vla-put-TextString theAttributeReference newValue)
	theAttributeReference	
)



(defun getAttributeValue
	(
		theBlockReference
		tag
		/
		theBlockReference
		attributeReferences
		theAttributeReference
	)
	(setq attributeReferences
		(gc:VariantToLispData 
			(vla-GetAttributes theBlockReference)
		)
	)
	(foreach x attributeReferences
		(if (= tag (vla-get-TagString x))
			(setq theAttributeReference x)
		)
	)
	(vla-get-TextString theAttributeReference)
)

;;; copied on 2016/01/17 from http://www.theswamp.org/index.php?topic=31674.5;wap2 

;;;======================== VARIANTS & SAFEARRAYS ========================;;;

;; Variant -> LISP

;; gc:VariantToLispData
;; Converts a variant or a safearray into LISP data (list)
;;
;; Argument: var variant or safearray

(defun gc:VariantToLispData (var)
  (cond
    ((= (type var) 'variant)
     (gc:VariantToLispData (vlax-variant-value var)))
    ((= (type var) 'safearray)
     (mapcar 'gc:VariantToLispData (vlax-safearray->list var))
    )
    (T var)
  )
)

;; gc:2dVariantToPointList
;; Converts a variant of 2D coordinates into a 2d points list
;; LightweightPolyline: OCS coordinates
;;
;; Argument
;; var: a variant (array of doubles) as returned by vla-get-Coordinates

(defun gc:2dVariantToPointList (var / foo)
  (defun foo (lst)
    (if lst
      (cons (list (car lst) (cadr lst)) (foo (cddr lst)))
    )
  )
  (foo (vlax-safearray->list (vlax-variant-value var)))
)

;; gc:3dVariantToPointList
;; Converts a variant of 3D coordinates into a 3d points list
;; 2d Polyline: OCS coordinates (Z = 0)
;; 3DFace, 3DPolyline, Leader, MLine, PolyfaceMesh,
;; PolygonMesh, Solid, Trace: WCS coordinates
;;
;; Argument
;; var: a variant (array of doubles) as returned by vla-get-Coordinates

(defun gc:3dVariantToPointList (var / foo)
  (defun foo (lst)
    (if lst
      (cons (list (car lst) (cadr lst) (caddr lst)) (foo (cdddr lst)))
    )
  )
  (foo (vlax-safearray->list (vlax-variant-value var)))
)

;; gc:VariantsToDxfList
;; Returns an assoc list (DXF list type)
;;
;; Arguments
;; xtyp: variant (array of integers)
;; xval: varinat (array of variants)

(defun gc:VariantsToDxfList (xtyp xval)
  (mapcar 'cons (gc:VariantToLispData xtyp) (gc:VariantToLispData xval))
)

;; gc:GetXdata
;; Returns the object xadta list
;;
;; Arguments
;; obj: (vla-object) the object containing xdata
;; app: (string) the registred application name ("" for all)

(defun gc:GetXdata (obj app / xtyp xval)
  (vla-GetXdata obj app 'xtyp 'xval)
  (gc:VariantsToDxfList xtyp xval)
)

;; gc:GetXrecordData
;; Returns the xrecord object DXF data list
;;
;; Arguments
;; xrec: (vla-object) thet XRECORD object

(defun gc:GetXrecordData (xrec / xtyp xval)
  (vla-GetXrecordData xrec 'xtyp 'xval)
  (gc:VariantsToDxfList xtyp xval)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; LISP -> variant

;; gc:2dPointListToVariant (gile)
;; Return a variant of 2d coordinates
;;
;; Argument: a 2d points list -type (x y)-

(defun gc:2dPointListToVariant (lst)
  (vlax-make-variant
    (vlax-safearray-fill
      (vlax-make-safearray
        vlax-VbDouble
        (cons 0 (1- (* 2 (length lst))))
      )
      (apply 'append lst)
    )
  )
)

;; gc:3dPointListToVariant (gile)
;; Return a variant of 3d coordinates
;;
;; Argument: a 3d points list -type (x y z)-

(defun gc:3dPointListToVariant (lst)
  (vlax-make-variant
    (vlax-safearray-fill
      (vlax-make-safearray
        vlax-VbDouble
        (cons 0 (1- (* 3 (length lst))))
      )
      (apply 'append lst)
    )
  )
)

;; gc:ObjectListToVariant
;; returns a variant (array of objects)
;;
;; Argument
;; lst: a vla-object list

(defun gc:ObjectListToVariant (lst)
  (vlax-make-variant
    (vlax-safearray-fill
      (vlax-make-safearray
        vlax-vbObject
        (cons 0 (1- (length lst)))
      )
      lst
    )
  )
)

;; gc:DxfListToVariants
;; Defines 2 variables and bounds a variant to each
;;
;; Arguments
;; lst: a DXF list
;; typeSymbol: a quoted symbol (other than 'typeSymbol)
;; valueSymbol: a quoted symbol (other than 'valueSymbol)

(defun gc:DxfListToVariants (lst typeSymbol valueSymbol)
  (set typeSymbol
       (vlax-make-variant
         (vlax-safearray-fill
           (vlax-make-safearray
             vlax-vbInteger
             (cons 0 (1- (length lst)))
           )
           (mapcar 'car lst)
         )
       )
  )
  (set valueSymbol
       (vlax-make-variant
         (vlax-safearray-fill
           (vlax-make-safearray
             vlax-vbVariant
             (cons 0 (1- (length lst)))
           )
           (mapcar '(lambda (x)
                      (if (listp (setq x (cdr x)))
                        (vlax-3d-point x)
                        (vlax-make-variant x)
                      )
                    )
                   lst
           )
         )
       )
  )
)


;; gc:SetXdata
;; Set xdatas to an object
;;
;; Arguments
;; obj: (vla-object) the object to set xdatas
;; lst: (liste DXF) the xdatas as:
;; '((1001 . "App_Name") (1002 . "{") (1000 . "string") (1070 . 1) (1002 . "}"))

(defun gc:SetXdata (obj lst / xtyp xval)
  (gc:DxfListToVariants lst 'xtyp 'xval)
  (vla-SetXdata obj xtyp xval)
)

;; gc:SetXrecordData
;; Set datas to an xrecord
;;
;; Arguments
;; xrec: (vla-object) the Xrecord object
;; lst : (liste DXF) the datas as:
;; '((1 . "string") (70 . 1) (10 1.0 2.0 0.0))

(defun gc:SetXrecordData (xrec lst / xtyp xval)
  (gc:DxfListToVariants lst 'xtyp 'xval)
  (vla-SetXrecordData xrec xtyp xval)
)

;; Copied verbatim from http://www.lee-mac.com/dynamicblockfunctions.html 
;;


;; Get Dynamic Block Property Value  -  Lee Mac
;; Returns the value of a Dynamic Block property (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; prp - [str] Dynamic Block property name (case-insensitive)

(defun LM:getdynpropvalue ( blk prp )
    (setq prp (strcase prp))
    (vl-some '(lambda ( x ) (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'value)))
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)












;; Set Dynamic Block Property Value  -  Lee Mac
;; Modifies the value of a Dynamic Block property (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; prp - [str] Dynamic Block property name (case-insensitive)
;; val - [any] New value for property
;; Returns: [any] New value if successful, else nil

(defun LM:setdynpropvalue ( blk prp val )
    (setq prp (strcase prp))
    (vl-some
       '(lambda ( x )
            (if (= prp (strcase (vla-get-propertyname x)))
                (progn
                    (vla-put-value x (vlax-make-variant val (vlax-variant-type (vla-get-value x))))
                    (cond (val) (t))
                )
            )
        )
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)









;; Get Dynamic Block Properties  -  Lee Mac
;; Returns an association list of Dynamic Block properties & values.
;; blk - [vla] VLA Dynamic Block Reference object
;; Returns: [lst] Association list of ((<prop> . <value>) ... )

(defun LM:getdynprops ( blk )
    (mapcar '(lambda ( x ) (cons (vla-get-propertyname x) (vlax-get x 'value)))
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)








;; Set Dynamic Block Properties  -  Lee Mac
;; Modifies values of Dynamic Block properties using a supplied association list.
;; blk - [vla] VLA Dynamic Block Reference object
;; lst - [lst] Association list of ((<Property> . <Value>) ... )
;; Returns: nil

(defun LM:setdynprops ( blk lst / itm )
    (setq lst (mapcar '(lambda ( x ) (cons (strcase (car x)) (cdr x))) lst))
    (foreach x (vlax-invoke blk 'getdynamicblockproperties)
        (if (setq itm (assoc (strcase (vla-get-propertyname x)) lst))
            (vla-put-value x (vlax-make-variant (cdr itm) (vlax-variant-type (vla-get-value x))))
        )
    )
)














;; Get Dynamic Block Property Allowed Values  -  Lee Mac
;; Returns the allowed values for a specific Dynamic Block property.
;; blk - [vla] VLA Dynamic Block Reference object
;; prp - [str] Dynamic Block property name (case-insensitive)
;; Returns: [lst] List of allowed values for property, else nil if no restrictions

(defun LM:getdynpropallowedvalues ( blk prp )
    (setq prp (strcase prp))
    (vl-some '(lambda ( x ) (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'allowedvalues)))
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)















;; Toggle Dynamic Block Flip State  -  Lee Mac
;; Toggles the Flip parameter if present in a supplied Dynamic Block.
;; blk - [vla] VLA Dynamic Block Reference object
;; Return: [int] New Flip Parameter value

(defun LM:toggleflipstate ( blk )
    (vl-some
       '(lambda ( prp / rtn )
            (if (equal '(0 1) (vlax-get prp 'allowedvalues))
                (progn
                    (vla-put-value prp (vlax-make-variant (setq rtn (- 1 (vlax-get prp 'value))) vlax-vbinteger))
                    rtn
                )
            )
        )
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)

















;; Get Visibility Parameter Name  -  Lee Mac
;; Returns the name of the Visibility Parameter of a Dynamic Block (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; Returns: [str] Name of Visibility Parameter, else nil

(defun LM:getvisibilityparametername ( blk / vis )  
    (if
        (and
            (vlax-property-available-p blk 'effectivename)
            (setq blk
                (vla-item
                    (vla-get-blocks (vla-get-document blk))
                    (vla-get-effectivename blk)
                )
            )
            (= :vlax-true (vla-get-isdynamicblock blk))
            (= :vlax-true (vla-get-hasextensiondictionary blk))
            (setq vis
                (vl-some
                   '(lambda ( pair )
                        (if
                            (and
                                (= 360 (car pair))
                                (= "BLOCKVISIBILITYPARAMETER" (cdr (assoc 0 (entget (cdr pair)))))
                            )
                            (cdr pair)
                        )
                    )
                    (dictsearch
                        (vlax-vla-object->ename (vla-getextensiondictionary blk))
                        "ACAD_ENHANCEDBLOCK"
                    )
                )
            )
        )
        (cdr (assoc 301 (entget vis)))
    )
)













;; Get Dynamic Block Visibility State  -  Lee Mac
;; Returns the value of the Visibility Parameter of a Dynamic Block (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; Returns: [str] Value of Visibility Parameter, else nil

(defun LM:getvisibilitystate ( blk )
    (LM:getdynpropvalue blk (LM:getvisibilityparametername blk))
)






















;; Set Dynamic Block Visibility State  -  Lee Mac
;; Sets the Visibility Parameter of a Dynamic Block (if present) to a specific value (if allowed)
;; blk - [vla] VLA Dynamic Block Reference object
;; val - [str] Visibility State Parameter value
;; Returns: [str] New value of Visibility Parameter, else nil

(defun LM:SetVisibilityState ( blk val / vis )
    (if
        (and
            (setq vis (LM:getvisibilityparametername blk))
            (member (strcase val) (mapcar 'strcase (LM:getdynpropallowedvalues blk vis)))
        )
        (LM:setdynpropvalue blk vis val)
    )
)

;; Field Code  -  Lee Mac
;; Returns the field expression associated with an entity

(defun LM:fieldcode ( ent / replacefield replaceobject fieldstring enx )

    (defun replacefield ( str enx / ent fld pos )
        (if (setq pos (vl-string-search "\\_FldIdx" (setq str (replaceobject str enx))))
            (progn
                (setq ent (assoc 360 enx)
                      fld (entget (cdr ent))
                )
                (strcat
                    (substr str 1 pos)
                    (replacefield (fieldstring fld) fld)
                    (replacefield (substr str (1+ (vl-string-search ">%" str pos))) (cdr (member ent enx)))
                )
            )
            str
        )
    )

    (defun replaceobject ( str enx / ent pos )
        (if (setq pos (vl-string-search "ObjIdx" str))
            (strcat
                (substr str 1 (+ pos 5)) " "
                (LM:ObjectID (vlax-ename->vla-object (cdr (setq ent (assoc 331 enx)))))
                (replaceobject (substr str (1+ (vl-string-search ">%" str pos))) (cdr (member ent enx)))
            )
            str
        )
    )

    (defun fieldstring ( enx / itm )
        (if (setq itm (assoc 3 enx))
            (strcat (cdr itm) (fieldstring (cdr (member itm enx))))
            (cond ((cdr (assoc 2 enx))) (""))
        )
    )
    
    (if (and (wcmatch  (cdr (assoc 0 (setq enx (entget ent)))) "TEXT,MTEXT,ATTRIB,MULTILEADER,*DIMENSION")
             (setq enx (cdr (assoc 360 enx)))
             (setq enx (dictsearch enx "ACAD_FIELD"))
             (setq enx (dictsearch (cdr (assoc -1 enx)) "TEXT"))
        )
        (replacefield (fieldstring enx) enx)
    )
)

;; ObjectID  -  Lee Mac
;; Returns a string containing the ObjectID of a supplied VLA-Object
;; Compatible with 32-bit & 64-bit systems

(defun LM:ObjectID ( obj )
    (eval
        (list 'defun 'LM:ObjectID '( obj )
            (if
                (and
                    (vl-string-search "64" (getenv "PROCESSOR_ARCHITECTURE"))
                    (vlax-method-applicable-p (vla-get-utility (LM:acdoc)) 'getobjectidstring)
                )
                (list 'vla-getobjectidstring (vla-get-utility (LM:acdoc)) 'obj ':vlax-false)
               '(itoa (vla-get-objectid obj))
            )
        )
    )
    (LM:ObjectID obj)
)

;; Active Document  -  Lee Mac
;; Returns the VLA Active Document Object

(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)


(defun appendTo 
	(
		theList
		theElementToAppend
		/
		returnValue
	)
	(COND
		(
			(= (type theList) 'SYM)
			(progn
				(if (not (eval theList)) (set theList (list))) ; if theList is undefined, set it to an empty list (I realize that this is a tautology in lisp, since an undefined variabel has value nil, which is the same as an empty list. -- oh well)
				(set theList
					(append
						(eval theList)
						(list theElementToAppend)
					)
				)
				(setq returnValue (eval theList))
			)
		)
		(
			(= (type theList) 'LIST)
			(progn
				(setq returnValue 						
					(append
						(eval theList)
						(list theElementToAppend)
					)
				)
			)
		)
	)
	returnValue
)



(doit)
(princ)
; ;(command-s "qsave")
; (command-s "close")