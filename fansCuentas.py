'''
                                                   ^ ^
 Creado por el Área de Data Science    <(°)      =(°-°)=         
 Fernando Dorantes Nieto                 ( >)"    (   )S  
                                          /|       w w

Este archivo tiene como objetivo descargar --
--el número de fans que tuvieron las cuentas --
--de Facebook de SEAT y Volkswagen Financial Services

'''


import facebook
import pandas as pd
import numpy as np
import datetime
import itertools
import time
from itertools import chain
from datetime import timedelta, date

##Variables globales
token = "tu token"
api = facebook.GraphAPI(token)

idSeat = "113144262054871"
idVwfs = "282547551846127"


##Fechas
numeros = []
for i in range(1, 13):
    for i in [1, 5, 10, 15, 20, 25]:
        i = str(i)
        j= str(j)
        numero= "-".join(["2016", i, j])
        numeros.append(numero)

for i in range(1, 4):
    for j in [1, 5, 10, 15, 20, 25]:
        i = str(i)
        j = str(j)
        numero = "-".join(["2017", i, j])
        numeros.append(numero)
numeros.append("2017-5-1")
ids = [idSeat, idVWfs]


cadena = "/insights/page_fans?since="
it = iter(numeros)
it1 = iter(numeros[1:])

objetos =[]
for i in it:
    objeto = "".join([cadena, i, "&until=", next(it)])
    objetos.append(objeto)

for i in it1:
    objeto = "".join([cadena, i, "&until=", next(it)])
    objetos.append(objeto)

Objetos = []

for idCuenta in ids:
    for i in objetos:
        objetotemp = idCuenta + i
        Objetos.append(objetotemp)


##Fans
fans = []
for i in Objetos:
    conv = api.get_object(i)
    conv = conv["data"]
    fans.append(conv)

fan = list(chain.from_iterable(fans))
dictio = []
for i in range(0, len(fan)):
    temp = dict(idPosteo = fan[i]["id"])
    temp.update(valores = fan[i]["values"])
    dictio.append(temp)

fanDF = pd.DataFrame.from_dict(dictio)
test = [fanDF, pd.DataFrame(fanDF["valores"].tolist()).iloc[:, :5]]
fanDF1 = pd.concat(test, axis=1).drop("valores", axis=1)
fanDF1.to_csv("datos/fans.csv", header=True, index=False)










