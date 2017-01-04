;Sergio Garcia Prado
;Oscar Fernandez Angulo
;13/12/2016

;Ejericio 4. Modelar el asistente al diagnostico propuesto por Pool y Mackworth.

;Plantilla objeto-atributo-valor
(deftemplate oav
	(slot objeto)
	(slot atributo)
	(slot valor)
)

(deffacts hechosPoolMackworth
	(oav 
		(objeto outside)
		(atributo corriente)
		(valor true)
	)
	(oav
		(objeto w5)
		(atributo conectadoA)
		(valor outside)
	)
;Bombillas
	;l1
	(oav
		(objeto l1)
		(atributo bombilla)
		(valor true)
	)
	(oav
		(objeto l1)
		(atributo ok)
		(valor true)
	)
	(oav
		(objeto l1)
		(atributo conectadoA)
		(valor w0)
	)	
	;l2
	(oav
		(objeto l2)
		(atributo bombilla)
		(valor true)
	)
	(oav
		(objeto l2)
		(atributo ok)
		(valor true)
	)
	(oav
		(objeto l2)
		(atributo conectadoA)
		(valor w4)
	)	
;Fusibles
	;cb1
	(oav
		(objeto cb1)
		(atributo ok)
		(valor true)
	)	
	;cb2
	(oav
		(objeto cb2)
		(atributo ok)
		(valor true)
	)	
;Enchufes	
	;p1
	(oav
		(objeto p1)
		(atributo conectadoA)
		(valor w3)
	)	
	(oav
		(objeto p1)
		(atributo ok)
		(valor true)
	)
	;p2	
	(oav
		(objeto p2)
		(atributo conectadoA)
		(valor w6)
	)	
	(oav
		(objeto p2)
		(atributo ok)
		(valor true)
	)
;Conmutadores
	;s1
	(oav
		(objeto s1)
		(atributo posicion)
		(valor abajo)
	)
	(oav
		(objeto s1)
		(atributo ok)
		(valor true)
	)
	;s2
	(oav
		(objeto s2)
		(atributo posicion)
		(valor arriba)
	)
	(oav
		(objeto s2)
		(atributo ok)
		(valor true)
	)
	;s3
	(oav
		(objeto s3)
		(atributo posicion)
		(valor arriba)
	)
	(oav
		(objeto s3)
		(atributo ok)
		(valor true)
	)
)

;Reglas de conexiones:
(defrule conexion_w0-w1
	(oav (objeto s2) (atributo ok) (valor true))
	(oav (objeto s2) (atributo posicion) (valor arriba))
	=>
	(assert (oav (objeto w0) (atributo conectadoA) (valor w1)) )
)
(defrule conexion_w0-w2
	(oav (objeto s2) (atributo ok) (valor true))
	(oav (objeto s2) (atributo posicion) (valor abajo))
	=>
	(assert (oav (objeto w0) (atributo conectadoA) (valor w2)) )
)
(defrule conexion_w1-w3
	(oav (objeto s1) (atributo ok) (valor true))
	(oav (objeto s1) (atributo posicion) (valor arriba))
	=>
	(assert (oav (objeto w1) (atributo conectadoA) (valor w3)) )
)
(defrule conexion_w2-w3
	(oav (objeto s1) (atributo ok) (valor true))
	(oav (objeto s1) (atributo posicion) (valor abajo))
	=>
	(assert (oav (objeto w2) (atributo conectadoA) (valor w3)) )
)

(defrule conexion_w4-w3
	(oav (objeto s3) (atributo ok) (valor true))
	(oav (objeto s3) (atributo posicion) (valor arriba))
	=>
	(assert (oav (objeto w4) (atributo conectadoA) (valor w3)) )
)

(defrule conexion_w3-w5
	(oav (objeto cb1) (atributo ok) (valor true))
	=>
	(assert (oav (objeto w3) (atributo conectadoA) (valor w5)) )
)
(defrule conexion_w6-w5
	(oav (objeto cb2) (atributo ok) (valor true))
	=>
	(assert (oav (objeto w6) (atributo conectadoA) (valor w5)) )
)

;Regla flujo de corriente por conexiones:
(defrule corriente
	(oav (objeto ?destino) (atributo conectadoA) (valor ?origen))
	(oav (objeto ?origen) (atributo corriente) (valor true))
	=>
	(assert (oav (objeto ?destino) (atributo corriente) (valor true)))
)

;Regla de bombillas que lucen:
(defrule luce
	(oav (objeto ?bombilla) (atributo bombilla) (valor true))
	(oav (objeto ?bombilla) (atributo ok) (valor true))
	(oav (objeto ?bombilla) (atributo corriente) (valor true))
	=>
	(assert (oav (objeto ?bombilla) (atributo luce) (valor true)))
)

;Garantizar la semantica univaluada:
(defrule garantizar-semantica
        (declare (salience 10000))
        ?f1 <- (oav (objeto ?objeto) (atributo ?atributo))
        ?f2 <- (oav (objeto ?objeto) (atributo ?atributo))
        (test (neq ?f1 ?f2))
        =>
        (retract ?f2)
)

