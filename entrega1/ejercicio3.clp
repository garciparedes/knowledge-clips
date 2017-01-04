;Sergio Garcia Prado
;Oscar Fernandez Angulo
;13/12/2016

;Ejercicio 3. Modelar el asistente de diagnostico para automóbiles.


;Plantilla objeto-atributo-valor
(deftemplate oav
        (slot objeto)
        (slot atributo)
        (slot valor)
)

;Escenarios (descomentar uno de los dos escenarios):

;(deffacts escenario1
;        (oav
;                (objeto motor)
;                (atributo arranca)
;                (valor false)
;        )
;
;        (oav
;                (objeto bateria)
;                (atributo indicador)
;                (valor cero)
;        )
;)

;(deffacts escenario2
;        (oav
;                (objeto motor)
;                (atributo sePara)
;                (valor true)
;        )
;
;        (oav
;                (objeto combustible)
;                (atributo indicador)
;                (valor cero)
;        )
;)

(defrule arranca1 "El coche no arranca por falta de electricidad"
        (oav (objeto motor) (atributo arranca) (valor false))
        =>
        (assert (oav (objeto potencia) (atributo conectada) (valor false)))
)

(defrule arranca2 "El coche no arranca por falta de combustible"
        (oav (objeto motor) (atributo arranca) (valor false))
        =>
        (assert (oav (objeto motor) (atributo combustible) (valor false)))
)

(defrule sePara "El coche se para"
        (oav (objeto coche)(atributo sePara)(valor true))
        =>
        (assert (oav (objeto motor) (atributo combustible) (valor false)))
)

(defrule fusibleFundido "El fusible esta roto"
        (oav (objeto potencia) (atributo conectada) (valor false))
        (oav (objeto fusible) (atributo inspeccion) (valor roto))
        =>
        (assert (oav (objeto fusible) (atributo estado) (valor fundido)))
)

(defrule bateriaBaja "La bateria esta agotada"
        (oav (objeto potencia) (atributo conectada) (valor false))
        (oav (objeto bateria) (atributo indicador) (valor cero))
        =>
        (assert (oav (objeto bateria) (atributo estado) (valor baja)))
)

(defrule depositoCombustible "El deposito esta agotado"
        (oav (objeto motor) (atributo combustible) (valor false))
        (oav (objeto combustible) (atributo indicador) (valor cero))
        =>
        (assert (oav (objeto deposito) (atributo estado) (valor vacio)))
)
