flight(istanbul,rize).
flight(istanbul,van).
flight(istanbul,ankara).
flight(istanbul,gaziantep).
flight(istanbul,antalya).
flight(istanbul,izmir).
flight(rize,van).
flight(van,ankara).
flight(ankara,konya).
flight(gaziantep,antalya).
flight(konya,antalya).
flight(izmir,ısparta).
flight(ısparta,burdur).
flight(edirne,edremit).
flight(edremit,erzincan).



distance(istanbul,rize,967).
distance(istanbul,van,1262).
distance(istanbul,ankara,351).
distance(istanbul,gaziantep,847).
distance(istanbul,antalya,482).
distance(istanbul,izmir,328).
distance(rize,van,373).
distance(van,ankara,920).
distance(ankara,konya,227).
distance(gaziantep,antalya,592).
distance(konya,antalya,192).
distance(izmir,ısparta,308).
distance(ısparta,burdur,24).
distance(edirne,edremit,914).
distance(edremit,erzincan,736).


route(X,Y):-flight(X,Y);flight(Y,X).
route(X,Y):-flight(X,N),
    route(N,Y),
    X\=Y.



sroute(X,Y,Z):-((distance(X,Y,Z);distance(Y,X,Z))
               ->true
               ;write('There is no fligth')
               ).
