# AI Chess - Prolog

## Popis projektu
Tento repozitář obsahuje kód pro semestrální projekt z předmětu 4IZ531. Cílem projektu je implementovat šachový program v programovacím jazyce Prolog, který umožní hrát hru proti umělé inteligenci. Součástí programu je tedy i základní šachový engine pro vyhodnocování aktuálního stavu hry a generování tahů.

Šachový program využívá algoritmu minimax s omezenou hloubkou pro rozhodování o nejlepším tahu v dané pozici. Algoritmus minimax je klasickým algoritmem pro hledání optimálního tahu ve hře s nulovým součtem. Omezení hloubky algoritmu umožňuje snížit výpočetní složitost a získat rychlejší odezvu.

## Obsah repozitáře
Repozitář obsahuje následující soubory:
- `sachy.pl`: Hlavní soubor obsahující implementaci šachového programu v Prologu,
- `pozice.pl`: Soubor obsahující jednotlivé pozice na šachovnici (jejich označení a souřadnice),
- `figurky.pl`: Soubor obsahující barvy hráčů a jednotlivé figurky (označení, barva, znak pro výpis, hodnota).

## Spuštění
Pro spuštění šachového programu je nutné mít nainstalovaný interpret Prologu na vašem počítači. Program lze spustit z rozhraní příkazové řádky.

```
swipl
["sachy.pl"].
sachy.
```

Po spuštění se zobrazí šachovnice a bílý hráč (uživatel) je vyzván k zadání prvního tahu. Po provedení tahu je na řadě černý hráč, který je reprezentování umělou inteligencí. Umělá inteligence využívající algoritmus minimax s omezenou hloubkou vyhodnodí aktuální stav hry a provede svůj tah. Tento cyklus se opakuje.

