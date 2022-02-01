when(102,10).
when(108,12).
when(341,14).
when(455,16).
when(452,17).

where(102,z23).
where(108,z11).
where(341,z06).
where(455,207).
where(452,207).

enroll(a,102).
enroll(a,108).
enroll(b,102).
enroll(c,108).
enroll(d,341).
enroll(e,455).


schedule(S,P,T):-
    enroll(S,N),
    where(N,P),
    when(N,T).

usage(P,T):-
    where(X,P),
    when(X,T).

time_conf(X,Y):-  %this function check time conflict.
    (   schedule(X,_,T1), %for student
        schedule(Y,_,T2),
        T1=T2)    ;
    (   when(X,T3),  %for class
        when(Y,T4),
        T3=T4).

room_conf(X,Y):- %this function check classroom conflict.
   (   enroll(X,C1),   %for student
       where(C1,R1),
       enroll(Y,C2),
       where(C2,R2),
       R1=R2)     ;
   (   where(X,R3),     %for clas
       where(Y,R4),
       R3=R4).

conflict(X,Y):-
   (time_conf(X,Y);
    room_conf(X,Y)),
    X\=Y.


meet(X,Y):-
    enroll(X,P1),
    enroll(Y,P2),
    X\=Y,
    P1=P2.
