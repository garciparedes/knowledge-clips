;Sergio Garcia Prado
;Oscar Fernandez Angulo
;03/01/2017

;Ejercicio 3.
;
;Modelar el asistente de diagnostico para enfermedades
;cardiovasculares basado en probabilidad.



;
;
; Definicion de plantillas para el formalismo O-A-V-C:
; ******************************************************************************

(deftemplate oavc-u "Plantilla para hechos univaluados"
	(slot objeto (type SYMBOL))
	(slot atributo(type SYMBOL))
	(slot valor)
	(slot factor (type FLOAT) (range -1.0 +1.0))
)


(deftemplate oavc-m "Plantilla para hechos multivaluados"
	(slot objeto (type SYMBOL))
	(slot atributo(type SYMBOL))
	(slot valor)
	(slot factor (type FLOAT) (range -1.0 +1.0))
)

(defrule garantizar-univaluados
	(declare (salience 9000))
	?f1 <- (oavc-u (objeto ?o1) (atributo ?a1))
	?f2 <- (oavc-u (objeto ?o1) (atributo ?a1))
	(test (neq ?f1 ?f2))
=>
	(retract ?f2)
)

(defrule duplicar-hechos
	(declare (salience 10000))
=>
	(set-fact-duplication TRUE)
)



(defrule acumula-positivos-univaluados
	(declare (salience 10000))
	?fact1 <- (oavc-u (objeto ?o) (atributo ?a) (valor ?v) (factor ?f1&:(>= ?f1 0)&:(<= ?f1 1)) )
	?fact2 <- (oavc-u (objeto ?o) (atributo ?a) (valor ?v) (factor ?f2&:(>= ?f2 0)&:(<= ?f2 1)) )
	(test (neq ?fact1 ?fact2))
=>
	(retract ?fact1)
	(bind ?f3 (+ ?f1 (* ?f2 (- 1 ?f1))))
	(modify ?fact2 (factor ?f3))
)

(defrule acumula-positivos-multivaluados
	(declare (salience 10000))
	?fact1 <- (oavc-m (objeto ?o) (atributo ?a) (valor ?v) (factor ?f1&:(>= ?f1 0)&:(<= ?f1 1)) )
	?fact2 <- (oavc-m (objeto ?o) (atributo ?a) (valor ?v) (factor ?f2&:(>= ?f2 0)&:(<= ?f2 1)) )
	(test (neq ?fact1 ?fact2))
=>
	(retract ?fact1)
	(bind ?f3 (+ ?f1 (* ?f2 (- 1 ?f1))))
	(modify ?fact2 (factor ?f3))
)


;
;
; Hechos
; ******************************************************************************


;
;Hechos Marta:
(deffacts hechos_Marta
	(oavc-u (objeto marta) (atributo genero) (valor mujer) (factor 1.0))
	(oavc-u (objeto marta) (atributo edad) (valor 12) (factor 1.0))
	(oavc-u (objeto marta) (atributo peso) (valor obeso) (factor 1.0))
	(oavc-m (objeto marta) (atributo sintomas) (valor fiebre) (factor 0.6))
	(oavc-m (objeto marta) (atributo observacion) (valor rumor_sistolico) (factor 0.8))
	(oavc-u (objeto marta) (atributo sistolica) (valor 150) (factor 1.0))
	(oavc-u (objeto marta) (atributo diastolica) (valor 60) (factor 1.0))
)


;
;Hechos Luis:
(deffacts hechos_Luis
	(oavc-u (objeto luis) (atributo genero) (valor hombre) (factor 1.0))
	(oavc-u (objeto luis) (atributo edad) (valor 49) (factor 1.0))
	(oavc-u (objeto luis) (atributo peso) (valor obeso) (factor 0.5))
	(oavc-m (objeto luis) (atributo sintomas) (valor dolor_abdominal) (factor 0.7))
	(oavc-m (objeto luis) (atributo sintomas) (valor calambres_pierna_andar) (factor 0.6))
	(oavc-m (objeto luis) (atributo observacion) (valor rumor_abdominal) (factor 0.6))
	(oavc-m (objeto luis) (atributo observacion) (valor masa_pulsante_abdomen) (factor 0.8))
	(oavc-u (objeto luis) (atributo sistolica) (valor 130) (factor 1.0))
	(oavc-u (objeto luis) (atributo diastolica) (valor 90) (factor 1.0))
)


;
;Hechos Andres:
(deffacts hechos_andres
	(oavc-u (objeto andres) (atributo genero) (valor hombre) (factor 1.0))
	(oavc-u (objeto andres) (atributo edad) (valor 52) (factor 1.0))
	(oavc-u (objeto andres) (atributo peso) (valor obeso) (factor 0.7))
	(oavc-u (objeto andres) (atributo fuma) (valor 18) (factor 1.0))
	(oavc-u (objeto andres) (atributo sistolica) (valor 125) (factor 1.0))
	(oavc-u (objeto andres) (atributo diastolica) (valor 85) (factor 1.0))
)


;
;
;Reglas para el diagnostico:
; ******************************************************************************

(defrule calcular_pulso
        (oavc-u (objeto ?paciente) (atributo sistolica) (valor ?sis) (factor ?f1))
        (oavc-u (objeto ?paciente) (atributo diastolica) (valor ?dia) (factor ?f2))
        =>
        (bind ?pul (- ?sis ?dia))
        (assert (oavc-u (objeto ?paciente) (atributo pulso) (valor ?pul) (factor 1.0)))
)

(defrule R1
	(oavc-m (objeto ?paciente) (atributo sintomas) (valor dolor_abdominal) (factor ?f1))
	(oavc-m (objeto ?paciente) (atributo observacion) (valor rumor_abdominal) (factor ?f2))
	(oavc-m (objeto ?paciente) (atributo observacion) (valor masa_pulsante_abdomen) (factor ?f3))
	(test (> (min ?f1 ?f2 ?f3) 0.2))
	=>
        (bind ?f (* (min ?f1 ?f2 ?f3) 0.8))
        (assert (oavc-m  (objeto ?paciente) (atributo diagnostico) (valor aneurisma_arteria_abdominal) (factor ?f)))
)

(defrule R2a "regurgitacion aortica (dos ciertas)"
 	(oavc-u (objeto ?paciente) (atributo sistolica) (valor ?x & :(> ?x 140)) (factor ?f1))
 	(oavc-u (objeto ?paciente) (atributo pulso) (valor ?y & :(> ?y 50)) (factor ?f2))
 	(oavc-m (objeto ?paciente) (atributo observacion) (valor rumor_sistolico) (factor ?f3))
 	(oavc-m (objeto ?paciente) (atributo observacion) (valor dilatacion_corazon) (factor ?f4))
 	(test (> (min ?f1 ?f2 (max ?f3 ?f4)) 0.2))
 	=>
        (bind ?f (* (min ?f1 ?f2 (max ?f3 ?f4)) 0.7))
 	(assert (oavc-m (objeto ?paciente) (atributo diagnostico) (valor regurgitacion_aortica) (factor ?f)))
)
 
(defrule R2b "regurgitacion aortica (segunda cierta)"
 	(oavc-u (objeto ?paciente) (atributo sistolica) (valor ?x & :(> ?x 140)) (factor ?f1))
 	(oavc-u (objeto ?paciente) (atributo pulso) (valor ?y & :(> ?y 50)) (factor ?f2))
 	(not (oavc-m (objeto ?paciente) (atributo observacion) (valor rumor_sistolico) (factor ?f3)))
 	(oavc-m (objeto ?paciente) (atributo observacion) (valor dilatacion_corazon) (factor ?f4))
 	(test (> (min ?f1 ?f2 ?f4) 0.2))
 	=>
        (bind ?f (* (min ?f1 ?f2 ?f4) 0.7))
 	(assert (oavc-m (objeto ?paciente) (atributo diagnostico) (valor regurgitacion_aortica) (factor ?f)))
)
 
(defrule R2c "regurgitacion aortica (primera cierta)"
 	(oavc-u (objeto ?paciente) (atributo sistolica) (valor ?x & :(> ?x 140)) (factor ?f1))
 	(oavc-u (objeto ?paciente) (atributo pulso) (valor ?y & :(> ?y 50)) (factor ?f2))
 	(oavc-m (objeto ?paciente) (atributo observacion) (valor rumor_sistolico) (factor ?f3))
 	(not (oavc-m (objeto ?paciente) (atributo observacion) (valor dilatacion_corazon) (factor ?f4)))
 	(test (> (min ?f1 ?f2 ?f3) 0.2))
 	=>
        (bind ?f (* (min ?f1 ?f2 ?f3) 0.7))
 	(assert (oavc-m (objeto ?paciente) (atributo diagnostico) (valor regurgitacion_aortica) (factor ?f)))
)

(defrule R3a "estenosis (calambre pierna)"
 	(oavc-m (objeto ?paciente) (atributo sintomas) (valor calambre_pierna_andar) (factor ?f1))
 	(test (> ?f1 0.2))
 	=>
        (bind ?f (* ?f1 0.9))
 	(assert (oavc-m (objeto ?paciente) (atributo diagnostico) (valor estenosis) (factor ?f)))
)

(defrule R3b "estenosis (arteriosclerosis)"
 	(oavc-m (objeto ?paciente) (atributo sintomas) (valor arteriosclerosis) (factor ?f1))
 	(test (> ?f1 0.2))
 	=>
        (bind ?f (* ?f1 0.8))
 	(assert (oavc-m (objeto ?paciente) (atributo diagnostico) (valor estenosis) (factor ?f)))
)

(defrule R3c "estenosis (obeso)"
 	(oavc-u (objeto ?paciente) (atributo peso) (valor obeso) (factor ?f1))
 	(test (> ?f1 0.2))
 	=>
        (bind ?riesgo 1)
	(bind ?f (* ?riesgo 0.8)) 
 	(assert (oavc-m (objeto ?paciente) (atributo diagnostico) (valor estenosis) (factor ?f)))
)

(defrule R3d "estenosis (fumador durante >18 anos)"
 	(oavc-u (objeto ?paciente) (atributo fuma) (valor ?anos) (factor ?f1))
 	(test (> ?anos 18))
 	=>
        (bind ?riesgo 1)
	(bind ?f (* ?riesgo 0.8)) 
 	(assert (oavc-m (objeto ?paciente) (atributo diagnostico) (valor estenosis) (factor ?f1)))
)

(defrule R3e "estenosis (fumador entre 12 y 18 anos)"
 	(oavc-u (objeto ?paciente) (atributo fuma) (valor ?anos) (factor ?f1))
 	(test (<= ?anos 18))
	(test (> ?anos 12))
 	=>
	(bind ?riesgo (/ (- ?anos 12) 6))
	(bind ?f (* ?riesgo 0.8)) 
 	(assert (oavc-m (objeto ?paciente) (atributo diagnostico) (valor estenosis) (factor ?f)))
)

(defrule R3f "estenosis (edad >55 anos)"
 	(oavc-u (objeto ?paciente) (atributo edad) (valor ?anos) (factor ?f1))
 	(test (> ?anos 55))
 	=>
        (bind ?riesgo 1)
	(bind ?f (* ?riesgo 0.6))
 	(assert (oavc-m (objeto ?paciente) (atributo diagnostico) (valor estenosis) (factor ?f)))
)

(defrule R3g "estenosis (edad entre 45 y 55 anos)"
 	(oavc-u (objeto ?paciente) (atributo edad) (valor ?anos) (factor ?f1))
 	(test (<= ?anos 55))
 	(test (> ?anos 45))
 	=>
	(bind ?riesgo (/ (- ?anos 45) 10))
	(bind ?f (* ?riesgo 0.6)) 
 	(assert (oavc-m (objeto ?paciente) (atributo diagnostico) (valor estenosis) (factor ?f)))
)

;
;
; Imprimir el diagnostico
; ******************************************************************************

(defrule imprimir_diagnostico
	(declare (salience -10000))
	(oavc-m (objeto ?paciente) (atributo diagnostico) (valor ?diag) (factor ?f))
	=>
	(printout t ?paciente " padece " ?diag " con un factor de certeza de " ?f crlf)
)
