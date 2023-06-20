:- module(figurky,[figurka/4]).


% figurky na šachovnici (označení, barva, znak pro výpis, ohodnocení)
figurka(b_kral,bila,'\u2654',(-900)).
figurka(b_dama,bila,'\u2655',(-90)).
figurka(b_vez,bila,'\u2656',(-50)).
figurka(b_jezdec,bila,'\u2658',(-30)).
figurka(b_strelec,bila,'\u2657',(-30)).
figurka(b_pesec,bila,'\u2659',(-10)).


figurka(c_kral,cerna,'\u265a',900).
figurka(c_dama,cerna,'\u265b',90).
figurka(c_vez,cerna,'\u265c',50).
figurka(c_jezdec,cerna,'\u265e',30).
figurka(c_strelec,cerna,'\u265d',30).
figurka(c_pesec,cerna,'\u265f',10).

