Autor: Wojciech Mańke, 360333

URUCHAMIANIE PROGRAMU
Program uruchamiany zgodnie z treścią polecenia, tzn.:
./latc_x86_64 src/main.lat
Utowrzy plik assemblera src/main.c oraz plik wykonywalny src/main

OPTYMALIZACJE
- zastępowanie wyrażeń ze stałymi stałymi
- zwijanie ifów/whileów, które się nie wykonają
- obliczanie zmiennych martwych w obrębie bloku
- usuwanie instrukcji bez efektu, np. mov %eax,%eax

FRONTEND
Type checking wykonywany jest przed optymalizacjami, a więc np. program zawierający
funkcję:
int f() {
  if (true) {
    return 7;
  }
  else {
    return false;
  }
}
Nie zostanie skompilowany ze względu na zły typ drugiego returna.

Walidacja returnów odbywa się po optymalizacjach, program zawierający funkcję:
int f() {
  if (true && true)
    return 7;
}
int g() {
  if (false) {}
  else {return 7;}
}
Zostanie poprawnie skompilowany.

Podobnie jak Java nie dopuszczam przedefiniowanie parametru funkcji, np.:
int f(int a) {
  int a = 7;
  return a;
}
int g(int a) {
  if (a != 0) {
    int a = 7;
  }
}
nie zostaną skompilowane.

PRZESTRZEŃ NAZW
Przestrzeń nazw jest współdzielona przez zmienne, funkcje i klasy (np. nie można
zdefiniować zmiennej o nazwie równej nazwie klasy/funkcji).
Nazwy funkcji muszą być unikalne, nie mogą istnieć dwie funkcje o tej samej nazwie,
ale różnej liczbie parametrów.

PROTOKÓŁ WYWOŁANIA FUNKCJI
- argumenty na stosie, od ostatniego do pierwszego
- zachowywany tylko rejestr rbp
- wynik w eax/rax

ROZSZERZENIA:
Zaimplementowałem tablice oraz struktury, wraz ze wszystkimi możliwymi kombinacjami, tzn. np. tablice dwuwymiarowe, tablice struktur, struktury zawierające tablice

ALOKACJA REJESTRÓW
Zaimplementowałem alokację rejestrów ze zmiennymi żywymi obliczanymi w obrębie jednego bloku.

STRUKTURA KATALOGÓW:
src - katalog z plikami źródłowymi
runtime - katalog z plikiem wykonywalnym runtime, jak i jego implementacją w C++
grammar - katalog z gramatyką w formacie BNFC
