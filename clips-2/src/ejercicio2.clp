;Sergio Garcia Prado
;Oscar Fernandez Angulo
;13/12/2016

;Ejercicio 2. Modelar el asistente de diagnostico para enfermedades cardiovasculares.


;Definimos las dos plantillas:
(deftemplate oav-u "Plantilla Hechos univaluados"
	(slot objeto (type SYMBOL))
	(slot atributo (type SYMBOL))
	(slot valor)
)

(deftemplate oav-m "Plantilla Hechos multivaluados"
	(slot objeto (type SYMBOL))
	(slot atributo (type SYMBOL))
	(slot valor)
)

;Hechos Marta:
(deffacts hechos_Marta
	(oav-u (objeto marta) (atributo genero) (valor mujer))
	(oav-u (objeto marta) (atributo edad) (valor 12))
	(oav-m (objeto marta) (atributo sintomas) (valor fiebre))
	(oav-m (objeto marta) (atributo observacion) (valor rumor_diastolico))
	(oav-u (objeto marta) (atributo sistolica) (valor 150))
	(oav-u (objeto marta) (atributo diastolica) (valor 60))
)

;Hechos Luis:
(deffacts hechos_Luis
	(oav-u (objeto luis) (atributo genero) (valor hombre))
	(oav-u (objeto luis) (atributo edad) (valor 60))
	(oav-m (objeto luis) (atributo sintomas) (valor dolor_abdominal))
	(oav-m (objeto luis) (atributo observacion) (valor rumor_abdominal))
	(oav-m (objeto luis) (atributo observacion) (valor masa_pulsante_abdomen))
	(oav-u (objeto luis) (atributo sistolica) (valor 130))
	(oav-u (objeto luis) (atributo diastolica) (valor 90))
)

;Hechos enfermedades:
(deffacts hechos_enfermedades
	(oav-m (objeto aneurisma_arteria_abdominal) (atributo afecta) (valor vasos_sanguineos))
	(oav-m (objeto estenosis_arterial) (atributo afecta) (valor vasos_sanguineos))
	(oav-m (objeto arterioesclerosis) (atributo afecta) (valor vasos_sanguineos))
	(oav-m(objeto regurgitacion_aortica) (atributo afecta) (valor corazon))
)

;Garantizamos la semantica univaluada:
(defrule garantizar_semantica
	(declare (salience 10000))
	?f1 <- (oav-u (objeto ?obj) (atributo ?atr) (valor ?val))
	?f2 <- (oav-u (objeto ?obj) (atributo ?atr) (valor ?val))
	(test (neq ?f1 ?f2))
	=>
	(retract ?f2)
)

;Reglas para el diagnostico:
(defrule R1 "calcula presion pulso"
	(oav-u (objeto ?pac) (atributo sistolica) (valor ?sis))
	(oav-u (objeto ?pac) (atributo diastolica) (valor ?dia))
	=>
	(bind ?pul (- ?sis ?dia))
	(assert (oav-u (objeto ?pac) (atributo pulso) (valor ?pul)))
)

(defrule R2a "paciente de riesgo"
	(oav-u(objeto ?pac) (atributo peso) (valor obeso))
	=>
	(assert (oav-m(objeto ?pac) (atributo condicion) (valor paciente_riesgo)))
)

(defrule R2b "paciente de riesgo"
	(oav-u (objeto ?pac) (atributo fuma) (valor ?y &:(> ?y 15)))
	=>
	(assert (oav-m (objeto ?pac) (atributo condicion) (valor paciente_riesgo)))
)

(defrule R2c "paciente de riesgo"
	(oav-u (objeto ?pac) (atributo edad) (valor ?y &:(> ?y 60)))
	=>
	(assert (oav-m (objeto ?pac) (atributo condicion) (valor paciente_riesgo)))
)

(defrule R3 "aneurisma en la arteria abdominal"
	(oav-m(objeto ?x) (atributo observacion) (valor rumor_abdominal))
	(oav-m(objeto ?x) (atributo observacion) (valor masa_pulsante_abdomen))
	=>
	(assert (oav-m (objeto ?x) (atributo diagnostico) (valor aneurisma_arteria_abdominal)))
)

(defrule R4 "regurgitacion aortica"
	(oav-u (objeto ?x) (atributo sistolica) (valor ?y &:(> ?y 140)))
	(oav-u (objeto ?x) (atributo pulso) (valor ?z&:(> ?z 50)))
	(or 
		(oav-m (objeto ?x) (atributo observacion) (valor rumor_sistolico))
		(oav-m (objeto ?x) (atributo observacion) (valor dilatacion_corazon))
	)
	=>
	(assert (oav-m (objeto ?x) (atributo diagnostico) (valor regurgitacion_aortica)))
)

(defrule R5 "estenosis en la arteria de la pierna"
	(oav-m(objeto ?x) (atributo sintomas) (valor calambres_pierna_andar))
	=>
	(assert (oav-m(objeto ?x) (atributo diagnostico) (valor estenosis_arteria_pierna)))
)

(defrule R6 "arterioesclerosis"
	(oav-m(objeto ?x) (atributo diagnostico) (valor estenosis_arteria_pierna))
	(oav-m(objeto ?x) (atributo condicion) (valor paciente_riesgo))
	=>
	(assert (oav-m(objeto ?x) (atributo diagnostico) (valor arterioesclerosis)))
)

(defrule R7a "enfermedad cardiovascular"
	(oav-m (objeto ?x) (atributo afecta) (valor vasos_sanguineos))
	=>
	(assert (oav-m (objeto ?x) (atributo tipo) (valor cardio_vascular)))
)

(defrule R7b "enfermedad cardiovascular"
	(oav-m (objeto ?x) (atributo afecta) (valor corazon))
	=>
	(assert (oav-m (objeto ?x) (atributo tipo) (valor cardio_vascular)))
)

;Imprimir el diagnostico
(defrule imprimir_diagnostico
	(declare (salience -10000))
	(oav-m(objeto ?pac) (atributo diagnostico) (valor ?diag))
	(oav-m(objeto ?diag) (atributo afecta) (valor ?afec))
	(oav-m(objeto ?diag) (atributo tipo) (valor ?tipo))
	=>
	(printout t ?pac " padece " ?diag ", de tipo "?tipo " que afecta a " ?afec crlf)
)
