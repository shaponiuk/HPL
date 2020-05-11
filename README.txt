INTERPRETER JEST JESZCZE NIEDOKOŃCZONY

90% runtime i 60% statycznego sprawdzania uznaję za gotowę
Do tego wymagany jest jeszcze pewien refactor

Niemniejjednak wszystkie główne cechy języka działają (np. leniwość, korutyny, synchronizacja korutyn, pattern-matching typów algebraicznych itp.)

Potrzebuję:
-przerobić statyczne sprawdzanie na takie z numerami linii i znaku.
-dodać kilka wbudowanych typów algebraicznych (np. Maybe, List)
-zaimplementować pozostałe operatory porównania i dzielenie
-ogólny refactor kodu
-doimplementować sprawdzanie typów i obecności tylko zadeklarowanych funkcji i referencji (chwilowo jest zakomentowane)
-inne małe poprawki

Testowałem na GHC 8.6.5
kompilacja standardowo komendą make, stworzy się plik binarny "interpreter", który przyjmuje jako argument jeden plik.


