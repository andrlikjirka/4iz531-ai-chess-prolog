barva(bila).
barva(cerna).

figurka(b_kral,bila,'\u2654').
figurka(b_dama,bila,'\u2655').
figurka(b_vez,bila,'\u2656').
figurka(b_jezdec,bila,'\u2658').
figurka(b_strelec,bila,'\u2657').
figurka(b_pesec,bila,'\u2659').


figurka(c_kral,cerna,'\u265a').
figurka(c_dama,cerna,'\u265b').
figurka(c_vez,cerna,'\u265c').
figurka(c_jezdec,cerna,'\u265e').
figurka(c_strelec,cerna,'\u265d').
figurka(c_pesec,cerna,'\u265f').


pozice(a1,1,1).
pozice(a2,1,2).
pozice(a3,1,3).
pozice(a4,1,4).
pozice(a5,1,5).
pozice(a6,1,6).
pozice(a7,1,7).
pozice(a8,1,8).

pozice(b1,2,1).
pozice(b2,2,2).
pozice(b3,2,3).
pozice(b4,2,4).
pozice(b5,2,5).
pozice(b6,2,6).
pozice(b7,2,7).
pozice(b8,2,8).

pozice(c1,3,1).
pozice(c2,3,2).
pozice(c3,3,3).
pozice(c4,3,4).
pozice(c5,3,5).
pozice(c6,3,6).
pozice(c7,3,7).
pozice(c8,3,8).

pozice(d1,4,1).
pozice(d2,4,2).
pozice(d3,4,3).
pozice(d4,4,4).
pozice(d5,4,5).
pozice(d6,4,6).
pozice(d7,4,7).
pozice(d8,4,8).

pozice(e1,5,1).
pozice(e2,5,2).
pozice(e3,5,3).
pozice(e4,5,4).
pozice(e5,5,5).
pozice(e6,5,6).
pozice(e7,5,7).
pozice(e8,5,8).

pozice(f1,6,1).
pozice(f2,6,2).
pozice(f3,6,3).
pozice(f4,6,4).
pozice(f5,6,5).
pozice(f6,6,6).
pozice(f7,6,7).
pozice(f8,6,8).

pozice(g1,7,1).
pozice(g2,7,2).
pozice(g3,7,3).
pozice(g4,7,4).
pozice(g5,7,5).
pozice(g6,7,6).
pozice(g7,7,7).
pozice(g8,7,8).

pozice(h1,8,1).
pozice(h2,8,2).
pozice(h3,8,3).
pozice(h4,8,4).
pozice(h5,8,5).
pozice(h6,8,6).
pozice(h7,8,7).
pozice(h8,8,8).

start_sachovnice(Sachovnice) :-
    Sachovnice = [
        [b2,b_pesec],[c3,b_pesec],[f3,b_pesec],[g4,c_vez],
        [c7,c_pesec],[d6,c_pesec],[g6,c_pesec],[h5,b_vez]
    ].


vypis_sachovnice(Sachovnice) :-
    vypis_hranice,
    vypis_radky(Sachovnice, 8),
    vypis_popisku,nl,!.


vypis_hranice :-
  write(' +----+----+----+----+----+----+----+----+'),nl.


vypis_popisku :-
  write('   A    B    C    D    E    F    G    H'),nl.


vypis_radky(Sachovnice,N) :-
    N > 0,
    write(N),write('|'),vypis_radek_policka(Sachovnice,1,N),nl,
    vypis_hranice,
    N1 is N-1,
    vypis_radky(Sachovnice,N1).
vypis_radky(_,0).


vypis_radek_policka(_,9,_).
vypis_radek_policka(Sachovnice,X,Y) :-
    X =< 8,
    pozice(P,X,Y),
    (member([P,F],Sachovnice) -> (
        figurka(F,_,Znak),
        write(' '),write(Znak),write('  '),write('|')
    )
    ;
    write(' '),write(' '),write('  '),write('|')),
    X1 is X + 1,
    vypis_radek_policka(Sachovnice,X1,Y).


% zadávání tahů
vstup(Odkud, Kam) :-
    write('Zadej tah: '),
    current_input(P),
    read_line_to_string(P, X),
    (
        X = "konec" ->
        writeln('Terminating program...'),
        halt 
        ;
        split_string(X, ",\s\t\n", "\s\t\n", [X1, X2]),
        atom_string(Odkud, X1),
        atom_string(Kam, X2)
    ).


kontrola_vstup_pozice(Odkud,Kam) :-
    pozice(Odkud,_,_),
    pozice(Kam,_,_).
    
kontrola_vstup_odkud_figurka(Odkud,Sachovnice,BarvaHrac) :- 
    pozice(Odkud,_,_),
    member([Odkud,Figurka],Sachovnice),
    figurka(Figurka,BarvaHrac,_). % barva figurky na počáteční pozici = barva hráče

kontrola_vstup_kam_figurka(Kam,Sachovnice,BarvaHrac) :-
    pozice(Kam,_,_),
    (
        not(member([Kam,Figurka],Sachovnice)); % na cílové pozici není žádná figurka
        (
        member([Kam,Figurka],Sachovnice), % na cílové pozici je figurka
        not(figurka(Figurka,BarvaHrac,_)) % ale je opačné barvy
        )
    ).


vstup_tah(Odkud,Kam,Sachovnice,Barva) :-
    repeat,
    (
        vstup(Odkud,Kam) -> 
        (
            kontrola_vstup_pozice(Odkud,Kam) -> 
            (
                kontrola_vstup_odkud_figurka(Odkud,Sachovnice,Barva) -> 
                (
                    kontrola_vstup_kam_figurka(Kam,Sachovnice,Barva) -> 
                    (
                        kontrola_vstup_tah(Odkud,Kam,Sachovnice,Barva) -> !
                        ;
                        chyba_kontrola_tah(_),
                        fail
                    )
                    ;
                    chyba_vstup_kam_figurka(_),
                    fail
                )
                ;
                chyba_vstup_odkud_figurka(_),
                fail
            )
            ;
            chyba_vstup_pozice(_),
            fail
        )
        ;
        chyba_vstup(_),
        fail
    ).


% kontrola tahů
kontrola_vstup_tah(Odkud,Kam,Sachovnice,BarvaHrac) :-
    pozice(Odkud,OdkudX,OdkudY),
    pozice(Kam,KamX,KamY),
    member([Odkud,Figurka],Sachovnice),
    kontrola_vstup_tah_(Figurka,OdkudX,OdkudY,KamX,KamY,Sachovnice).
 
 % u kontroly tahu pro krále doplnit pravidlo, že král se svým tahem nesmí dostat do šachu
 % doplnit obecný predikát zjištění, zda je král v šachu (možná zvlášť pro bílý a černý)
kontrola_vstup_tah_(b_kral,OdkudX,OdkudY,KamX,KamY,_) :-
    (KamX =:= OdkudX+1, KamY =:= OdkudY); % o 1 pole doprava
    (KamX =:= OdkudX+1, KamY =:= OdkudY-1); % o 1 doprava, o 1 dolů
    (KamX =:= OdkudX, KamY =:= OdkudY-1); % o 1 dolů
    (KamX =:= OdkudX-1, KamY =:= OdkudY-1); % o 1 doleva, o 1 dolů
    (KamX =:= OdkudX-1, KamY =:= OdkudY); % o 1 doleva
    (KamX =:= OdkudX-1, KamY =:= OdkudY+1); % o 1 doleva, o 1 nahoru
    (KamX =:= OdkudX, KamY =:= OdkudY+1); % o 1 nahoru
    (KamX =:= OdkudX+1, KamY =:= OdkudY+1). % o 1 doprava, o 1 nahoru

kontrola_vstup_tah_(c_kral,OdkudX,OdkudY,KamX,KamY,_) :- 
    (KamX =:= OdkudX+1, KamY =:= OdkudY); % o 1 pole doprava
    (KamX =:= OdkudX+1, KamY =:= OdkudY-1); % o 1 doprava, o 1 dolů
    (KamX =:= OdkudX, KamY =:= OdkudY-1); % o 1 dolů
    (KamX =:= OdkudX-1, KamY =:= OdkudY-1); % o 1 doleva, o 1 dolů
    (KamX =:= OdkudX-1, KamY =:= OdkudY); % o 1 doleva
    (KamX =:= OdkudX-1, KamY =:= OdkudY+1); % o 1 doleva, o 1 nahoru
    (KamX =:= OdkudX, KamY =:= OdkudY+1); % o 1 nahoru
    (KamX =:= OdkudX+1, KamY =:= OdkudY+1). % o 1 doprava, o 1 nahoru

kontrola_vstup_tah_(b_dama,OdkudX,OdkudY,KamX,KamY,Sachovnice) :- 
    (KamX =:= OdkudX, KamY =\= OdkudY, neobsazeno_svisle(OdkudX,OdkudY,KamX,KamY,Sachovnice)); % svisle o libovolný počet polí, 
    (KamX =\= OdkudX, KamY =:= OdkudY, neobsazeno_vodorovne(OdkudX,OdkudY,KamX,KamY,Sachovnice)); % vodorovně o libovolný počet polí
    (abs(KamX-OdkudX) =:= abs(KamY-OdkudY),neobsazeno_diagonalne(OdkudX,OdkudY,KamX,KamY,Sachovnice)). % diagonálně o libovolný počet polí
     
kontrola_vstup_tah_(c_dama,OdkudX,OdkudY,KamX,KamY,Sachovnice) :-
    (KamX =:= OdkudX, KamY =\= OdkudY, neobsazeno_svisle(OdkudX,OdkudY,KamX,KamY,Sachovnice)); % svisle o libovolný počet polí, 
    (KamX =\= OdkudX, KamY =:= OdkudY, neobsazeno_vodorovne(OdkudX,OdkudY,KamX,KamY,Sachovnice)); % vodorovně o libovolný počet polí
    (abs(KamX-OdkudX) =:= abs(KamY-OdkudY),neobsazeno_diagonalne(OdkudX,OdkudY,KamX,KamY,Sachovnice)). % diagonálně o libovolný počet polí

kontrola_vstup_tah_(b_vez,OdkudX,OdkudY,KamX,KamY,Sachovnice) :-
    (KamX =:= OdkudX, KamY =\= OdkudY, neobsazeno_svisle(OdkudX,OdkudY,KamX,KamY,Sachovnice)); % svisle o libovolný počet polí, 
    (KamX =\= OdkudX, KamY =:= OdkudY, neobsazeno_vodorovne(OdkudX,OdkudY,KamX,KamY,Sachovnice)). % vodorovně o libovolný počet polí

kontrola_vstup_tah_(c_vez,OdkudX,OdkudY,KamX,KamY,Sachovnice) :-
    (KamX =:= OdkudX, KamY =\= OdkudY, neobsazeno_svisle(OdkudX,OdkudY,KamX,KamY,Sachovnice)); % svisle o libovolný počet polí, 
    (KamX =\= OdkudX, KamY =:= OdkudY, neobsazeno_vodorovne(OdkudX,OdkudY,KamX,KamY,Sachovnice)). % vodorovně o libovolný počet polí

kontrola_vstup_tah_(b_strelec,OdkudX,OdkudY,KamX,KamY,Sachovnice) :-
    abs(KamX-OdkudX) =:= abs(KamY-OdkudY),
    neobsazeno_diagonalne(OdkudX,OdkudY,KamX,KamY,Sachovnice).

kontrola_vstup_tah_(c_strelec,OdkudX,OdkudY,KamX,KamY,Sachovnice) :-
    abs(KamX-OdkudX) =:= abs(KamY-OdkudY),
    neobsazeno_diagonalne(OdkudX,OdkudY,KamX,KamY,Sachovnice).

kontrola_vstup_tah_(b_jezdec,OdkudX,OdkudY,KamX,KamY,Sachovnice) :-
    RozdilX is abs(OdkudX-KamX),
    RozdilY is abs(OdkudY-KamY),
    ((RozdilX=:=2,RozdilY=:=1);(RozdilX=:=1,RozdilY=:=2)).

kontrola_vstup_tah_(c_jezdec,OdkudX,OdkudY,KamX,KamY,Sachovnice) :-
    RozdilX is abs(OdkudX-KamX),
    RozdilY is abs(OdkudY-KamY),
    ((RozdilX=:=2,RozdilY=:=1);(RozdilX=:=1,RozdilY=:=2)).

kontrola_vstup_tah_(b_pesec,OdkudX,OdkudY,KamX,KamY,Sachovnice) :-
    ( % výchozí pozice, pohyb dovolen o 1 nebo 2 pole dopředu, pokud cílové není obsazené (nesmí přeskakovat)
        OdkudY=:=2,
        OdkudX=:=KamX,
        RozdilY is KamY-OdkudY,
        (RozdilY=:=1;RozdilY=:=2),
        pozice(P,KamX,KamY),
        not(member([P,_],Sachovnice)),
        neobsazeno_svisle(OdkudX,OdkudY,KamX,KamY,Sachovnice)
    ); 
    ( % nevyýchozí pozice, pohyb dovolen o 1 pole dopředu, pokud cílové není obsazené
        OdkudY=\=2,
        OdkudX=:=KamX,
        KamY-OdkudY=:=1,
        pozice(P,KamX,KamY),
        not(member([P,_],Sachovnice))
    );
    ( % pohyb o 1 diagonálně, pokud je na cílové pozici figurka protihráče => bude probíhat braní
        abs(KamX-OdkudX)=:=1, %vodorovně o 1
        KamY-OdkudY=:=1, %svisle jen dopředu o 1 (u b_pesce pohyb jen nahoru po sachovnici)
        pozice(P,KamX,KamY),
        member([P,_],Sachovnice) %kontrola, zda je na cílové pozici figurka (barva figurky na cílové pozici byla již kontrolována v kontrola_vstup_kam_figurka)
    ).

kontrola_vstup_tah_(c_pesec,OdkudX,OdkudY,KamX,KamY,Sachovnice) :-
    ( % výchozí pozice, pohyb dovolen o 1 nebo 2 pole dopředu, pokud cílové není obsazené (nesmí přeskakovat)
        OdkudY=:=7,
        OdkudX=:=KamX,
        RozdilY is OdkudY-KamY,
        (RozdilY=:=1;RozdilY=:=2),
        pozice(P,KamX,KamY),
        not(member([P,_],Sachovnice)),
        neobsazeno_svisle(OdkudX,OdkudY,KamX,KamY,Sachovnice)
    );
    ( % nevyýchozí pozice, pohyb dovolen o 1 pole dopředu, pokud cílové není obsazené
        OdkudY=\=7,
        OdkudX=:=KamX,
        OdkudY-KamY=:=1,
        pozice(P,KamX,KamY),
        not(member([P,_],Sachovnice))
    );
    ( % pohyb o 1 diagonálně, pokud je na cílové pozici figurka protihráče => bude probíhat braní
        abs(KamX-OdkudX)=:=1, %vodorovně o 1
        OdkudY-KamY=:=1, %svisle o 1 dopředu (u c_pesce pohyb jen dolů po šachovnici)
        pozice(P,KamX,KamY),
        member([P,_],Sachovnice)
    ).


% kontrola neobsazenosti polí na cestě

neobsazeno_vodorovne(OdkudX,OdkudY,KamX,KamY,Sachovnice) :-
    OdkudX =\= KamX,
    X1 is min(OdkudX,KamX)+1,
    X2 is max(OdkudX,KamX),
    findall(P,(pozice(P,X,OdkudY),X>=X1,X<X2),KontrolPozice),
    findall(Poz,(member(Poz,KontrolPozice),member([Poz,_],Sachovnice)),ObsazenePozice),
    length(ObsazenePozice,0).

neobsazeno_svisle(OdkudX,OdkudY,KamX,KamY,Sachovnice) :- 
    OdkudY =\= KamY,
    Y1 is min(OdkudY,KamY)+1,
    Y2 is max(OdkudY,KamY),
    findall(P,(pozice(P,OdkudX,Y),Y>=Y1,Y<Y2),KontrolPozice),
    findall(Poz,(member(Poz,KontrolPozice),member([Poz,_],Sachovnice)),ObsazenePozice),
    length(ObsazenePozice,0).


neobsazeno_diagonalne(OdkudX,OdkudY,KamX,KamY,Sachovnice) :- 
    neobsazeno_diagonalne1(OdkudX,OdkudY,KamX,KamY,Sachovnice);
    neobsazeno_diagonalne2(OdkudX,OdkudY,KamX,KamY,Sachovnice).

neobsazeno_diagonalne1(OdkudX,OdkudY,KamX,KamY,Sachovnice) :- % kontrola neobsazenosti v případě tahu šikmo alias rostoucí fce
    ( 
        (KamX>OdkudX, KamY>OdkudY); 
        (KamX<OdkudX, KamY<OdkudY)
    ),
    X1 is min(OdkudX,KamX)+1,
    X2 is max(OdkudX,KamX),
    Y1 is min(OdkudY,KamY)+1,
    Y2 is max(OdkudY,KamY),
    findall(P,(pozice(P,X,Y),X>=X1,X<X2,Y>=Y1,Y<Y2,(abs(X1-X)=:=abs(Y1-Y))),KontrolPozice),
    findall(Poz,(member(Poz,KontrolPozice),member([Poz,_],Sachovnice)),ObsazenePozice),
    length(ObsazenePozice,0),!.

neobsazeno_diagonalne2(OdkudX,OdkudY,KamX,KamY,Sachovnice) :- % kontrola neobsazenosti v případě tahu šikmo alias klesající fce
    (
        (KamX>OdkudX, KamY<OdkudY);
        (KamX<OdkudX, KamY>OdkudY)
    ),
    X1 is min(OdkudX,KamX)+1,
    X2 is max(OdkudX,KamX),
    Y1 is max(OdkudY,KamY)-1,
    Y2 is min(OdkudY,KamY),
    findall(P,(pozice(P,X,Y),X>=X1,X<X2,Y=<Y1,Y>Y2,(abs(X1-X)=:=abs(Y1-Y))),KontrolPozice),
    findall(Poz,(member(Poz,KontrolPozice),member([Poz,_],Sachovnice)),ObsazenePozice),
    length(ObsazenePozice,0),!.



pohyb(Odkud,Kam,Sachovnice,SachovniceNova) :-
    pozice(Odkud,_,_), pozice(Kam,_,_),
    member([Odkud,Figurka],Sachovnice),
    delete(Sachovnice,[Odkud,Figurka],Sachovnice2),
    append(Sachovnice2,[[Kam,Figurka]],SachovniceNova),
    !.


% herni smycka
sachy_krok_hrac(S,Sn,BarvaHrac) :- 
    vypis_sachovnice(S),
    vstup_tah(Odkud,Kam,S,BarvaHrac),
    %cílová pozice obsazena? true - brani(),pohyb() | false - pohyb()
    pohyb(Odkud,Kam,S,Sn).

sachy_krok(S,bila) :-
    write('Bílý je na tahu.'),nl,
    sachy_krok_hrac(S,Sn,bila),
    sachy_krok(Sn,cerna).


sachy_krok(S,cerna) :-
    write('Černý je na tahu.'),nl,
    sachy_krok_hrac(S,Sn,cerna),
    sachy_krok(Sn,bila).


sachy :- 
    start_sachovnice(S),
    sachy_krok(S,bila).


% chybové hlášky
chyba_vstup(_) :- write('Neplatný vstup.'),nl.
chyba_vstup_pozice(_) :- write('Zadána neplatná pozice na šachovnici.'),nl.
chyba_vstup_odkud_figurka(_) :- write('Hraj laskavě se svými figurkami.'),nl.
chyba_vstup_kam_figurka(_) :- write('Na cílové pozici se nachází figurka tvojí barvy.'),nl.
chyba_kontrola_tah(_) :- write('Zadaný tah nesplňuje pravidla.'),nl.