barva(bila).
barva(cerna).

figurka(b_kral,bila,'\u2654').
figurka(b_pesec,bila,'\u2659').
figurka(b_jezdec,bila,'\u2658').

figurka(c_kral,cerna,'\u265a').
figurka(c_pesec,cerna,'\u265f').
figurka(c_jezdec,cerna,'\u265e').

pozice(a1,1,1).
pozice(a2,1,2).
pozice(a3,1,3).

pozice(b1,2,1).
pozice(b2,2,2).
pozice(b3,2,3).

pozice(c1,3,1).
pozice(c2,3,2).
pozice(c3,3,3).

start_sachovnice(Sachovnice) :-
    Sachovnice = [
        [a1,b_kral],[b1,b_pesec],[c1,b_jezdec],
        [a3,c_kral],[b3,c_pesec],[c3,c_jezdec]
    ].


vypis_sachovnice(Sachovnice) :-
    vypis_hranice,
    vypis_radky(Sachovnice, 3),
    vypis_popisku,nl,!.


vypis_hranice :-
  write(' +----+----+----+'),nl.


vypis_popisku :-
  write('   A    B    C   '),nl.


vypis_radky(Sachovnice,N) :-
    N > 0,
    write(N),write('|'),vypis_radek_policka(Sachovnice,1,N),nl,
    vypis_hranice,
    N1 is N-1,
    vypis_radky(Sachovnice,N1).
vypis_radky(_,0).


vypis_radek_policka(_,4,_).
vypis_radek_policka(Sachovnice,X,Y) :-
    X =< 3,
    pozice(P,X,Y),
    (member([P,F],Sachovnice) -> (
        figurka(F,_,Znak),
        write(' '),write(Znak),write('  '),write('|')
    )
    ;
    write(' '),write(' '),write('  '),write('|')),
    X1 is X + 1,
    vypis_radek_policka(Sachovnice,X1,Y).


% vyřešit problém při pohybu na obsazenou pozici, je potřeba cílovou figurku také odstranit 
%(ale odstranění figurky, která na pozici není vyhodí false => zjistit obsazení cílové pozice)
pohyb(Odkud,Kam,Sachovnice,SachovniceNova) :-
    pozice(Odkud,_,_), pozice(Kam,_,_),
    member([Odkud,Figurka],Sachovnice),
    delete(Sachovnice,[Odkud,Figurka],Sachovnice2),
    append(Sachovnice2,[[Kam,Figurka]],SachovniceNova),!.

% predikát(y) kontrolující správnosti tahů (implementovat nejspíš jako podmínku, která bude použitelná v predikátu pohyb)


% zadávání tahů
vstup(Odkud,Kam) :-
    write('Zadej tah: '),
    current_input(P),
    read_line_to_string(P,X),
    split_string(X, ",\s\t\n" ,"\s\t\n" ,[X1,X2]),
    atom_string(Odkud,X1),
    atom_string(Kam,X2 ).


kontrola_vstup_pozice(Odkud,Kam) :-
    pozice(Odkud,_,_),
    pozice(Kam,_,_).
    

kontrola_vstup_figurka(Odkud,Sachovnice,Barva) :- 
    pozice(Odkud,_,_),
    member([Odkud,Figurka],Sachovnice),
    figurka(Figurka,Barva,_).


vstup_tah(Odkud,Kam,Sachovnice,Barva) :-
    repeat,
    (
        vstup(Odkud,Kam) -> 
        (
            kontrola_vstup_pozice(Odkud,Kam) -> 
            (
                kontrola_vstup_figurka(Odkud,Sachovnice,Barva) -> !
                ;
                chyba_vstup_figurka(_),
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


% herni smycka
sachy_krok_hrac(S,Sn,Barva) :- 
    vypis_sachovnice(S),
    vstup_tah(Odkud,Kam,S,Barva),
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
chyba_vstup_figurka(_) :- write('Hraj laskavě se svými figurkami.'),nl.

