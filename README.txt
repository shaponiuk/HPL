HPL

leniwy język funkcyjny z korutynami i referencjami

Typami wbudowanymi, prymitywnym jest Int (), Sempahore () i Ref (a).
Są dwa wbudowane typy algebraiczne: List (a) i SList ()
List jest listą jak w Haskellu, SList jest listą intów, które interpreter traktuje jako ciąg znaków ascii, działa to w obie strony
Dzięki temu wszystko możemy robić przy pomocy pattern-matchingu.
Dodatkowo możemy oczywiście definiować własne typy algebraiczne, nawet z referencjami

możemy definiować funkcje zwykłe i funkcje opóźnione
funkcje zwykłe działają standardowo, możemy stosować pattern matching, działają jak w Haskellu
funkcje opóźnione tworzą korutynę, która jest wstawiana na koniec kolejki wykonania. 
Może jednak zostać wykonana wcześniej, jeśli spróbujemy odpakować jej wynik (oczywiście musi zostać najpierw obliczony), albo jeśli wywołamy yield lub p wywołamy yield lub p.
yield uruchamia inną korutynę, a potem wraca do wykonania aktualnej, nic nie robi, jeśli nie ma innej dostępnej korutyny w kolejce.
p i v to są standardowe operacje na semaforach, jak w przykładach.
semafor musi być referencją do typu Semaphore ().
mamy operacje IO print (x), getline () i gets (x)
print (x) - typu () - wypisuje w nowej linii swój argument x
getline () - typu SList () - pobiera jedną linię
gets (x) - typu SList () - pobiera x znaków - x jest typu Int ()

operacje porównania działają jak notacja Iversona, zwraca 1 lub 0

operacje warunkowe if a then b else c - jeśli a == 0 to c, wpp. b

mamy instrukcję 'force let in' i 'let in', różnią się tym, że let in jest leniwe, czyli np. operacja print, która zwraca tylko (), jej wartość nie będzie użyta, nie wykona się.
Natomiast force let in wykonuje wszystko po kolei i do końca (jedynie korutyn nie wykonuje do końca, ze względu na ideę korutyn).

możemy wywoływać funkcję z częściową liczbą argumentów jak w Haskellu

taka funkcja
fun (Int () -> (Int () -> Int ())) a (x) = { y -> x () + y () };
jest normalną dwuargumentową funkcją.

w good są przykłady poprawnych (na razie, ponieważ nie ma włączonego sprawdzania typów) programów
w bad są dwa przykładowe złe programy (wykrycie deadlocka i dwie deklaracje konstruktorów o tych samych nazwach), dojdzie więcej przykładów, kiedy będę wiecej rzeczy sprawdzał statycznie