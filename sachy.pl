:- use_module(barvy).
:- use_module(figurky).
:- use_module(pozice).

:- public(sachy/0).

start_sachovnice(Sachovnice) :-
    Sachovnice = [
        [a8,c_vez],[b8,c_jezdec],[c8,c_strelec],[d8,c_dama],[e8,c_kral],[f8,c_strelec],[g8,c_jezdec],[h8,c_vez],
        [a7,c_pesec],[b7,c_pesec],[c7,c_pesec],[d7,c_pesec],[e7,c_pesec],[f7,c_pesec],[g7,c_pesec],[h7,c_pesec],

        [a2,b_pesec],[b2,b_pesec],[c2,b_pesec],[d2,b_pesec],[e2,b_pesec],[f2,b_pesec],[g2,b_pesec],[h2,b_pesec],
        [a1,b_vez],[b1,b_jezdec],[c1,b_strelec],[d1,b_dama],[e1,b_kral],[f1,b_strelec],[g1,b_jezdec],[h1,b_vez]
    ].

start_brane_figurky(BraneFigurky) :-
    BraneFigurky = [].

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
        figurka(F,_,Znak,_),
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
        writeln('Ukončuji šachovou partii...'),
        halt 
        ;
        string_lower(X,Xmale), % zadaný string se převede na malá písmena (vstup může být i velkými)
        split_string(Xmale, ",\s\t\n", "\s\t\n", [X1, X2]),
        atom_string(Odkud, X1),
        atom_string(Kam, X2)
    ).


kontrola_vstup_pozice(Odkud,Kam) :-
    pozice(Odkud,_,_),
    pozice(Kam,_,_).
    
kontrola_vstup_odkud_figurka(Odkud,Sachovnice,BarvaHrac) :- 
    pozice(Odkud,_,_),
    member([Odkud,Figurka],Sachovnice),
    figurka(Figurka,BarvaHrac,_,_). % barva figurky na počáteční pozici = barva hráče

kontrola_vstup_kam_figurka(Kam,Sachovnice,BarvaHrac) :-
    pozice(Kam,_,_),
    (
        not(member([Kam,_],Sachovnice)); % na cílové pozici není žádná figurka
        (
        member([Kam,Figurka],Sachovnice), % na cílové pozici je figurka
        not(figurka(Figurka,BarvaHrac,_,_)) % ale je opačné barvy
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
                        kontrola_tah(Odkud,Kam,Sachovnice,Barva) -> true
                        ;
                        chyba_kontrola_tah(),
                        fail
                    )
                    ;
                    chyba_vstup_kam_figurka(),
                    fail
                )
                ;
                chyba_vstup_odkud_figurka(),
                fail
            )
            ;
            chyba_vstup_pozice(),
            fail
        )
        ;
        chyba_vstup(),
        fail
    ).


% kontrola tahů
kontrola_tah(Odkud,Kam,Sachovnice,_) :-
    pozice(Odkud,OdkudX,OdkudY),
    pozice(Kam,KamX,KamY),
    member([Odkud,Figurka],Sachovnice),
    kontrola_tah_(Figurka,OdkudX,OdkudY,KamX,KamY,Sachovnice).

kontrola_tah_kral_(OdkudX,OdkudY,KamX,KamY) :-  % kontrola jen souřadnic pohybu krale
    (OdkudX\==KamX;OdkudY\==KamY), 
    DeltaX is abs(KamX-OdkudX), 
    DeltaY is abs(KamY-OdkudY), 
    DeltaX =< 1, 
    DeltaY =< 1.

kontrola_tah_(b_kral,OdkudX,OdkudY,KamX,KamY,Sachovnice) :-
    kontrola_tah_kral_(OdkudX,OdkudY,KamX,KamY),
    ( % král nesmí udělat pohyb na pozici, na kterou by se mohl platným pohybem dostat opačný král
        member([PoziceCKral,c_kral],Sachovnice),
        pozice(PoziceCKral,CKralX,CKralY),
        not(kontrola_tah_kral_(CKralX,CKralY,KamX,KamY))
    ).

kontrola_tah_(c_kral,OdkudX,OdkudY,KamX,KamY,Sachovnice) :- 
    kontrola_tah_kral_(OdkudX,OdkudY,KamX,KamY),
    ( % král nesmí udělat pohyb na pozici, na kterou by se mohl platným pohybem dostat opačný král
        member([PoziceBKral,b_kral],Sachovnice),
        pozice(PoziceBKral,BKralX,BKralY),
        not(kontrola_tah_kral_(BKralX,BKralY,KamX,KamY))
    ).


kontrola_tah_(b_dama,OdkudX,OdkudY,KamX,KamY,Sachovnice) :- 
    (KamX =:= OdkudX, KamY =\= OdkudY, neobsazeno_svisle(OdkudX,OdkudY,KamX,KamY,Sachovnice)); % svisle o libovolný počet polí, 
    (KamX =\= OdkudX, KamY =:= OdkudY, neobsazeno_vodorovne(OdkudX,OdkudY,KamX,KamY,Sachovnice)); % vodorovně o libovolný počet polí
    (abs(KamX-OdkudX) =:= abs(KamY-OdkudY),neobsazeno_diagonalne(OdkudX,OdkudY,KamX,KamY,Sachovnice)). % diagonálně o libovolný počet polí
     
kontrola_tah_(c_dama,OdkudX,OdkudY,KamX,KamY,Sachovnice) :-
    (KamX =:= OdkudX, KamY =\= OdkudY, neobsazeno_svisle(OdkudX,OdkudY,KamX,KamY,Sachovnice)); % svisle o libovolný počet polí, 
    (KamX =\= OdkudX, KamY =:= OdkudY, neobsazeno_vodorovne(OdkudX,OdkudY,KamX,KamY,Sachovnice)); % vodorovně o libovolný počet polí
    (abs(KamX-OdkudX) =:= abs(KamY-OdkudY),neobsazeno_diagonalne(OdkudX,OdkudY,KamX,KamY,Sachovnice)). % diagonálně o libovolný počet polí

kontrola_tah_(b_vez,OdkudX,OdkudY,KamX,KamY,Sachovnice) :-
    (KamX =:= OdkudX, KamY =\= OdkudY, neobsazeno_svisle(OdkudX,OdkudY,KamX,KamY,Sachovnice)); % svisle o libovolný počet polí, 
    (KamX =\= OdkudX, KamY =:= OdkudY, neobsazeno_vodorovne(OdkudX,OdkudY,KamX,KamY,Sachovnice)). % vodorovně o libovolný počet polí

kontrola_tah_(c_vez,OdkudX,OdkudY,KamX,KamY,Sachovnice) :-
    (KamX =:= OdkudX, KamY =\= OdkudY, neobsazeno_svisle(OdkudX,OdkudY,KamX,KamY,Sachovnice)); % svisle o libovolný počet polí, 
    (KamX =\= OdkudX, KamY =:= OdkudY, neobsazeno_vodorovne(OdkudX,OdkudY,KamX,KamY,Sachovnice)). % vodorovně o libovolný počet polí

kontrola_tah_(b_strelec,OdkudX,OdkudY,KamX,KamY,Sachovnice) :-
    abs(KamX-OdkudX) =:= abs(KamY-OdkudY),
    neobsazeno_diagonalne(OdkudX,OdkudY,KamX,KamY,Sachovnice).

kontrola_tah_(c_strelec,OdkudX,OdkudY,KamX,KamY,Sachovnice) :-
    abs(KamX-OdkudX) =:= abs(KamY-OdkudY),
    neobsazeno_diagonalne(OdkudX,OdkudY,KamX,KamY,Sachovnice).

kontrola_tah_(b_jezdec,OdkudX,OdkudY,KamX,KamY,_) :-
    RozdilX is abs(OdkudX-KamX),
    RozdilY is abs(OdkudY-KamY),
    ((RozdilX=:=2,RozdilY=:=1);(RozdilX=:=1,RozdilY=:=2)).

kontrola_tah_(c_jezdec,OdkudX,OdkudY,KamX,KamY,_) :-
    RozdilX is abs(OdkudX-KamX),
    RozdilY is abs(OdkudY-KamY),
    ((RozdilX=:=2,RozdilY=:=1);(RozdilX=:=1,RozdilY=:=2)).

kontrola_tah_(b_pesec,OdkudX,OdkudY,KamX,KamY,Sachovnice) :-
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

kontrola_tah_(c_pesec,OdkudX,OdkudY,KamX,KamY,Sachovnice) :-
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

neobsazeno_vodorovne(OdkudX,OdkudY,KamX,_,Sachovnice) :-
    OdkudX =\= KamX,
    X1 is min(OdkudX,KamX)+1,
    X2 is max(OdkudX,KamX),
    findall(P,(pozice(P,X,OdkudY),X>=X1,X<X2),KontrolPozice),
    findall(Poz,(member(Poz,KontrolPozice),member([Poz,_],Sachovnice)),ObsazenePozice),
    length(ObsazenePozice,0).

neobsazeno_svisle(OdkudX,OdkudY,_,KamY,Sachovnice) :- 
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


% mat
kontrola_mat(Kral,Sachovnice) :-
    not(member([_,Kral],Sachovnice)).

% šach
kontrola_sach(Kral,KamX,KamY,Sachovnice) :-
    figurka(Kral,BarvaHrac,_),
    pozice(CilPoziceKral,KamX,KamY),
    kontrola_sach_(CilPoziceKral,Sachovnice,BarvaHrac).
        
kontrola_sach(Kral,Sachovnice,BarvaHrac) :-
    member([PoziceKral,Kral],Sachovnice),
    kontrola_sach_(PoziceKral,Sachovnice,BarvaHrac).

kontrola_sach_(PoziceKral,Sachovnice,BarvaHrac) :-
     findall(
    	F,
        (
        	figurka(F,Barva,_,_),
            (F\==b_kral,F\==c_kral),
            Barva\==BarvaHrac,
            member([P,F],Sachovnice),
            kontrola_tah(P,PoziceKral,Sachovnice,BarvaHrac)
        ),
        SachujiciFigurky
   	),
    length(SachujiciFigurky,L),
    L=\=0.


posun_figurky(Odkud,Kam,Sachovnice,SachovniceNova) :-
    pozice(Odkud,_,_), pozice(Kam,_,_),
    member([Odkud,Figurka],Sachovnice),
    delete(Sachovnice,[Odkud,Figurka],Sachovnice2),
    append(Sachovnice2,[[Kam,Figurka]],SachovniceNova).

obsazena_cilova_pozice_protihrac(Kam,Sachovnice,BarvaHrac,Figurka) :- %možnost braní?: Kam je pozice, na které je figurka protihráče
    pozice(Kam,_,_),
    member([Kam,Figurka],Sachovnice),
    barva_hraci(BarvaHrac,BarvaProtihrac),
    figurka(Figurka,BarvaProtihrac,_,_).

brani_protihracovy_figurky(Kam,ProtihracovaFigurka,BF,BFn,S,Sbrani) :-
    delete(S,[Kam,ProtihracovaFigurka],Sbrani),
    append(BF,[ProtihracovaFigurka],BFn),
    figurka(ProtihracovaFigurka,_,Znak,_),
    write('Bereš soupeřovi figurku: '),write(Znak),nl.

vypis_sebranych_figurek(BF,BarvaHrac) :-
    findall(Znak,(figurka(Figurka,Barva,Znak,_),Barva\==BarvaHrac,member(Figurka,BF)),SebraneFigurky),
    write('Sebrané figurky protihráče: ['),
    vypis_seznamu(SebraneFigurky),
    write(']'),nl.

vypis_seznamu([]).
vypis_seznamu([H|T]) :-
    write(H),write(' '),
    vypis_seznamu(T).


% MINIMAX
% predikát ohodnoceni_sachovnice slouží pro ohodnocení konečných stavů (po dosažení dané hloubky)
ohodnoceni_sachovnice(Sachovnice,Hodnoceni) :-
    ohodnoceni_sachovnice_(Sachovnice,Hodnoceni,0).

ohodnoceni_sachovnice_([],Hodnoceni,Hodnoceni).
ohodnoceni_sachovnice_([[_,Figurka]|T],Hodnoceni,HodnoceniKumul):-
    figurka(Figurka,_,_,Hodnota),
    HodnoceniKumulNove is HodnoceniKumul+Hodnota,
    ohodnoceni_sachovnice_(T,Hodnoceni,HodnoceniKumulNove).

% predikáty slouží pro výběr nejlépe ohodnocené šachovnice (stavu hry), které minimax vygeneroval jako možné a přiřadil k nim ohodnocení
vyber_nejlepsi_tah([[S,_,BF,Pozice]],S,BF,Pozice).

vyber_nejlepsi_tah([[Sachovnice1,Hodnoceni1,BF1,Pozice1],[Sachovnice2,Hodnoceni2,BF2,Pozice2]|T],MaxSachovnice,BF,Pozice) :-
    Hodnoceni1 = Hodnoceni2,
    % pokud dva možné vygenerované stavy mají stejné hodnocení, vybere se náhodně 
    random_member(X,[[Sachovnice1,Hodnoceni1,BF1,Pozice1],[Sachovnice2,Hodnoceni2,BF2,Pozice2]]),
    vyber_nejlepsi_tah([X|T],MaxSachovnice,BF,Pozice).

vyber_nejlepsi_tah([[Sachovnice1,Hodnoceni1,BF1,Pozice1],[_,Hodnoceni2,_,_]|T],MaxSachovnice,BF,Pozice) :-
    Hodnoceni1 > Hodnoceni2,
    vyber_nejlepsi_tah([[Sachovnice1,Hodnoceni1,BF1,Pozice1]|T],MaxSachovnice,BF,Pozice).

vyber_nejlepsi_tah([[_,Hodnoceni1,_,_],[Sachovnice2,Hodnoceni2,BF2,Pozice2]|T],MaxSachovnice,BF,Pozice) :-
    Hodnoceni1 < Hodnoceni2,
    vyber_nejlepsi_tah([[Sachovnice2,Hodnoceni2,BF2,Pozice2]|T],MaxSachovnice,BF,Pozice).


posun_figurky_minimax(Odkud,Kam,Sachovnice,SachovniceNova,BF,BFn,BarvaHrac) :- % pohyb s ukládáním braných figurek (potřebné jen pro první úroveň stromu)
	(   
    obsazena_cilova_pozice_protihrac(Kam,Sachovnice,BarvaHrac,ProtihracovaFigurka) ->  
      delete(Sachovnice,[Kam,ProtihracovaFigurka],Sbrani), 
      append(BF,[ProtihracovaFigurka],BFn),
      posun_figurky(Odkud,Kam,Sbrani,SachovniceNova) 
      ;
      BFn=BF,
      posun_figurky(Odkud,Kam,Sachovnice,SachovniceNova)
    ).

posun_figurky_minimax(Odkud,Kam,Sachovnice,SachovniceNova,BarvaHrac) :- % pohyb bez ukádání braných figurek (pro generování tahů v dalších úrovních stromu)
	(   
    obsazena_cilova_pozice_protihrac(Kam,Sachovnice,BarvaHrac,ProtihracovaFigurka) ->  
      delete(Sachovnice,[Kam,ProtihracovaFigurka],Sbrani), 
      posun_figurky(Odkud,Kam,Sbrani,SachovniceNova)
      ;
      posun_figurky(Odkud,Kam,Sachovnice,SachovniceNova)
    ).

minimax_generuj_tah(S,BarvaHrac,Hodnoceni,Hloubka) :-
    barva_hraci(BarvaHrac,BarvaProtihrac),
    
    figurka(F,BarvaHrac,_,_),member([PoziceOdkud,F],S),
    (
    (pozice(PoziceKam,_,_),not(member([PoziceKam,_],S)));
    (pozice(PoziceKam,_,_),member([PoziceKam,ProtihracF],S),figurka(ProtihracF,BarvaProtihrac,_,_))
    ),
    kontrola_tah(PoziceOdkud,PoziceKam,S,cerna),
    posun_figurky_minimax(PoziceOdkud,PoziceKam,S,GenS,BarvaHrac), 
    
    minimax(GenS,BarvaProtihrac,Hodnoceni,Hloubka). %pro vygenerovaný tah se opět zavolá minimax, další vrstva (hloubka), ale tentokrát s opačnou barvou

minimax(Sachovnice,_,Hodnoceni,1) :- % minimax(Sachovnice,BarvaHrace,Hodnoceni,Hloubka)
    ohodnoceni_sachovnice(Sachovnice,Hodnoceni).

minimax(S,cerna,NejHodnoceni,Hloubka) :- % maximalizujici hrac (cerna) = AI - cerne figurky jsou kladně ohodnocené, tj. musím maximalizovat (minimax v této aplikaci vybírá tah pro černého hráče)
    Hloubka>1,
    HloubkaNova is Hloubka-1,
    aggregate_all(max(V),minimax_generuj_tah(S,cerna,V,HloubkaNova),NejHodnoceni).

minimax(S,bila,NejHodnoceni,Hloubka) :- % minimalizujici hrac (bila) = uživatel
    Hloubka>1,
    HloubkaNova is Hloubka-1,
    aggregate_all(min(V),minimax_generuj_tah(S,bila,V,HloubkaNova),NejHodnoceni).

sachy_krok_minimax(S,VysledekS,BF,VysledekBF,BarvaHrac) :- 
    vypis_sachovnice(S),
    findall(
        G,
        (
            figurka(F,cerna,_,_),member([PoziceOdkud,F],S),
			(
            	(pozice(PoziceKam,_,_),not(member([PoziceKam,_],S)));
        		(pozice(PoziceKam,_,_),member([PoziceKam,ProtihracF],S),figurka(ProtihracF,bila,_,_))
        	),
            kontrola_tah(PoziceOdkud,PoziceKam,S,cerna),
            posun_figurky_minimax(PoziceOdkud,PoziceKam,S,GenS,BF,BFn,BarvaHrac), 
            minimax(GenS,bila,Hodnoceni,2),
            G = [GenS,Hodnoceni,BFn,[PoziceOdkud,PoziceKam]]
        ),
        GenerovaneS
    ),
    vyber_nejlepsi_tah(GenerovaneS,VysledekS,VysledekBF,[PoziceOdkud,PoziceKam]),
    write('Černý hráč (AI) provedl tah: '),write(PoziceOdkud),write(','),write(PoziceKam),nl.


% HRAC
pohyb_hrac(Odkud,Kam,S,Sn,BF,BFn,BarvaHrac) :-
    (
        obsazena_cilova_pozice_protihrac(Kam,S,BarvaHrac,ProtihracovaFigurka) -> 
            brani_protihracovy_figurky(Kam,ProtihracovaFigurka,BF,BFn,S,Sbrani),
            posun_figurky(Odkud,Kam,Sbrani,Sn)
            ;
            BFn = BF,
            posun_figurky(Odkud,Kam,S,Sn)
    ).

sachy_krok_hrac(S,Sn,BF,BFn,BarvaHrac) :- 
    vypis_sachovnice(S),
    vstup_tah(Odkud,Kam,S,BarvaHrac),
    pohyb_hrac(Odkud,Kam,S,Sn,BF,BFn,BarvaHrac).    
    

% HERNÍ SMYČKA

sachy_krok(S,BF,bila) :- 
    write('Bílý hráč je na tahu.'),nl,
    vypis_sebranych_figurek(BF,bila),
    (   
    kontrola_sach(b_kral,S,bila) -> 
        write('Šach - bílý král ohrožen!'),nl
        ;
        true
    ),
    sachy_krok_hrac(S,Sn,BF,BFn,bila),
    (
    kontrola_mat(c_kral,Sn) -> 
        write('Mat černého krále. Bílý hráč vítězí!'),nl,
        vypis_sachovnice(Sn),
        write('Konec hry.'),nl,
        halt
        ;
        nl,
        sachy_krok(Sn,BFn,cerna)
    ).


sachy_krok(S,BF,cerna) :-
    write('Černý hráč je na tahu.'),nl,
    vypis_sebranych_figurek(BF,cerna),
    sachy_krok_minimax(S,Sn,BF,BFn,cerna),
    (
    kontrola_mat(b_kral,Sn) ->
        write('Mat bílého krále. Černý hráč vítězí!'),nl,
        vypis_sachovnice(Sn),
        write('Konec hry.'),nl,
        halt
        ;
        nl,
        sachy_krok(Sn,BFn,bila)
    ).


sachy :- 
    start_sachovnice(S),
    start_brane_figurky(BF),
    sachy_krok(S,BF,bila).


% chybové hlášky
chyba_vstup() :- write('Neplatný vstup.'),nl.
chyba_vstup_pozice() :- write('Zadána neplatná pozice na šachovnici.'),nl.
chyba_vstup_odkud_figurka() :- write('Hraj laskavě se svými figurkami.'),nl.
chyba_vstup_kam_figurka() :- write('Na cílové pozici se nachází figurka tvojí barvy.'),nl.
chyba_kontrola_tah() :- write('Zadaný tah nesplňuje pravidla.'),nl.