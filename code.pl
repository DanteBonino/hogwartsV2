/*
Harry es sangre mestiza, y se caracteriza por ser corajudo, amistoso, orgulloso e inteligente. Odiaría que el sombrero lo mande a Slytherin.
Draco es sangre pura, y se caracteriza por ser inteligente y orgulloso, pero no es corajudo ni amistoso. Odiaría que el sombrero lo mande a Hufflepuff.
Hermione es sangre impura, y se caracteriza por ser inteligente, orgullosa y responsable. No hay ninguna casa a la que odiaría ir.

Para Gryffindor, lo más importante es tener coraje.
Para Slytherin, lo más importante es el orgullo y la inteligencia.
Para Ravenclaw, lo más importante es la inteligencia y la responsabilidad.
Para Hufflepuff, lo más importante es ser amistoso.
*/
%Base de conocimiento:
casa(gryffindor).
casa(slytherin).
casa(ravenclaw).
casa(hufflepuff).

sangre(harry, mestiza).
sangre(draco, pura).
sangre(hermione, impura).

mago(Persona):-
    sangre(Persona,_).

%Punto 1
permiteEntrar(Casa, Mago):-
    casa(Casa),
    Casa \= slytherin,
    mago(Mago).

permiteEntrar(slytherin, Mago):-
    noEsSangreImpura(Mago).

noEsSangreImpura(Mago):-
    mago(Mago),
    not(esSangreImpura(Mago)).

esSangreImpura(Mago):-
    sangre(Mago, impura).  


%Punto 2:
caracterApropiadoPara(Mago,Casa):-
    caracteristicaQueBusca(Casa,_),
    caracteristica(Mago,_),
    forall(caracteristicaQueBusca(Casa, Caracteristica), caracteristica(Mago,Caracteristica)).

caracteristicaQueBusca(gryffindor,coraje).
caracteristicaQueBusca(slytherin,orgullo).
caracteristicaQueBusca(slytherin,inteligencia).
caracteristicaQueBusca(ravenclaw,inteligencia).
caracteristicaQueBusca(ravenclaw,responsabilidad).
caracteristicaQueBusca(hufflepuff,amistad).

caracteristica(harry, coraje).
caracteristica(harry, amistad).
caracteristica(harry, orgullo).
caracteristica(harry, inteligencia).
caracteristica(draco, orgullo).
caracteristica(draco, inteligencia).
caracteristica(hermione, orgullo).
caracteristica(hermione, inteligencia).
caracteristica(hermione, responsabilidad).

%Punto 3:
podriaQuedarSeleccionadoEn(Mago,Casa):-
    caracterApropiadoPara(Mago,Casa),
    permiteEntrar(Casa, Mago),
    not(odiriaQuedarEn(Mago,Casa)).
podriaQuedarSeleccionadoEn(hermione,gryffindor).

odiriaQuedarEn(harry, slytherin).
odiriaQuedarEn(draco, hufflepuff).

%Punto 4:
cadenaDeAmistades(Magos):-
    forall(consecutivos(Mago1, Mago2, Magos), sonAmistososYPuedenIrALAMismaCasa(Mago1, Mago2)).

sonAmistososYPuedenIrALAMismaCasa(Mago, Mago2):-
    esAmistoso(Mago),
    puedeEstarEnLaMismaCasa(Mago, Mago2).

esAmistoso(Mago):-
    caracteristica(Mago, amistad).

puedeEstarEnLaMismaCasa(Mago, Mago2):-
    podriaQuedarSeleccionadoEn(Mago2, Casa),
    podriaQuedarSeleccionadoEn(Mago,Casa).

consecutivos(Anterior,Siguiente,Magos):-
    nth0(Posicion, Magos, Anterior),
    PosicionSiguiente is Posicion + 1,
    nth0(PosicionSiguiente, Magos, Siguiente).

%Parte 2:
esDe(hermione, gryffindor).
esDe(ron, gryffindor).
esDe(harry, gryffindor).
esDe(draco, slytherin).
esDe(luna, ravenclaw).

hizo(harry,irA(bosque)).
hizo(harry, irA(tercerPiso)).
hizo(harry, fueraDeLaCama).
hizo(draco, irA(mazmorras)).
hizo(hermione, irA(tercerPiso)).
hizo(hermione, irA(seccionRestringida)).
hizo(ron, buenaAccion(ganarPartidaDeAjederez, 50)).
hizo(hermione, buenaAccion(usarIntelecto, 50)).
hizo(harry, buenaAccion(derrotarALordVoldemort, 70)).
hizo(hermione, responderPregunta(dondeSeEncuentraUnBezoar,20, snape)).
hizo(hermione, responderPregunta(comoHacerLevitarUnaPluma,25, flitwick)).

%Punto 1
%a
buenAlumno(Mago):-
    hizo(Mago,_),
    not(hizoMalaAccion(Mago)).

hizoMalaAccion(Mago):-
    hizo(Mago, Accion),
    puntajeAccion(Accion, Puntaje),
    Puntaje < 0.

puntajeAccion(fueraDeLaCama, -50).
puntajeAccion(irA(Lugar), Puntaje):-
    puntajeSegunLugar(Lugar, Puntaje).
puntajeAccion(buenaAccion(_,Puntaje), Puntaje).
puntajeAccion(responderPregunta(_,Dificultad, Profesor), Puntaje):-
    puntajeSegunDificultadYProfesor(Dificultad, Profesor, Puntaje).

puntajeSegunDificultadYProfesor(Dificultad, Profesor, Dificultad):-
    Profesor \= snape.
puntajeSegunDificultadYProfesor(Dificultad, snape, Puntaje):-
    Puntaje is Dificultad / 2.

puntajeSegunLugar(bosque,-50).
puntajeSegunLugar(seccionRestringida,-10).
puntajeSegunLugar(tercerPiso,-75).

%b
esRecurrente(Accion):-
    hizo(Mago, Accion),
    hizo(OtroMago, Accion),
    Mago \= OtroMago.



%Punto 2:
puntajeTotalCasa(Casa, Puntaje):-
    casa(Casa),
    findall(PuntajeAccion, puntajeAccionDeMiembro(Casa, PuntajeAccion), Puntajes),
    sum_list(Puntajes, Puntaje).

puntajeAccionDeMiembro(Casa, Puntaje):-
    esDe(Mago, Casa),
    hizo(Mago, Accion),
    puntajeAccion(Accion, Puntaje).

%Punto 3:
ganadoraDeLaCopa(Casa):-
    puntajeTotalCasa(Casa,Puntaje),
    forall(puntajeTotalCasa(_,PuntajeDeOtraCasa), Puntaje >= PuntajeDeOtraCasa).

%Punto 4:
