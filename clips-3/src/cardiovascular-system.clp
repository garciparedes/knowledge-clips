;Sergio Garcia Prado
;Oscar Fernandez Angulo
;03/01/2017

;Ejercicio 3.
;
;Modelar el asistente de diagnostico para enfermedades
;cardiovasculares basado en probabilidad.



;Definicion de plantillas para el formalismo O-A-V-C:



(deftemplate oavc-u
	"Plantilla para hechos univaluados"
	(slot objeto (type SYMBOL))
	(slot atributo(type SYMBOL))
	(slot valor)
	(slot factor (type FLOAT) (range -1.0 +1.0))
)


(deftemplate oavc-m
	"Plantilla para hechos multivaluados"
	(slot objeto (type SYMBOL))
	(slot atributo(type SYMBOL))
	(slot valor)
	(slot factor (type FLOAT) (range -1.0 +1.0))
)



(defrule duplicar-hechos
	(declare (salience 10000))
=>
	(set-fact-duplication TRUE)
)



(defrule acumula-positivos-univaluados
	(declare (salience 10000))
	?fact1 <- (oavc-u
				(objeto ?o)
				(atributo ?a)
				(valor ?v)
				(factor ?f1&:(>= ?f1 0)&:(< ?f1 1))
	)
	?fact2 <- (oavc-u (objeto ?o)
				(atributo ?a)
				(valor ?v)
				(factor ?f2&:(>= ?f2 0)&:(< ?f2 1))
	)
	(test (neq ?fact1 ?fact2))
=>
	(retract ?fact1)
	(bind ?f3 (+ ?f1 (* ?f2 (- 1 ?f1))))
	(modify ?fact2 (factor ?f3))
)



(defrule acumula-positivos-multivaluados
	(declare (salience 10000))
    ?fact1 <- (oavc-m (objeto ?o)
				(atributo ?a)
				(valor ?v)
				(factor ?f1&:(>= ?f1 0)&:(< ?f1 1))
	)
	?fact2 <- (oavc-m (objeto ?o)
				(atributo ?a)
				(valor ?v)
				(factor ?f2&:(>= ?f2 0)&:(< ?f2 1))
	)
	(test (neq ?fact1 ?fact2))
=>
	(retract ?fact1)
	(bind ?f3 (+ ?f1 (* ?f2 (- 1 ?f1))))
	(modify ?fact2 (factor ?f3))
)
