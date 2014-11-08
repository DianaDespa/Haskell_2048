Haskell: 2048

	Tabla si mutarile
		Am reprezentat tabla de joc printr-o structura formata dintr-o lista de
liste, sau o matrice, de numere intregi si o valoare intreaga care retine
scorul. Accesul la campurile structurii se face prin intermediul functiilor
"rows" si respectiv "score".Aceasta structura are un singur constructor
deoarece ea contine in permanenta date de acelasi tip. Am ales sa reprezint
celulele libere prin valoarea 0.
		In continuare voi detalia modul in care am implementat functiile mai
complexe.
		Functia "placeRandomCell" plaseaza aleator o noua celula pe tabla de
joc, cu valoarea 2 sau 4. Am folosit un generator aleator atat pentru alegerea
valorii celulei cat si pentru alegerea pozitiei pe tabla. Am format o lista cu
toate pozitiile libere de pe tabla(care au valoarea 0), cu ajutorul functiei
"elemIndices", din Data.List, am ales aleator un indice valid din aceasta lista.
Pozitia noii celule pe tabla de joc este data de coordonatele pentru linie si
coloana obtinute astfel: linia este valoarea de la indicele ales in lista de
pozitii impartit la 4, iar coloana este acea valoare modulo 4. Cu ajutorul
functiei "setInMatrix" setez pe pozitia gasita numarul generat aleator.
		In functia "initialize" definesc o matrice nula de 4x4 si apelez de
doua ori functia "placeRandomCell", cel de-al doilea apel fiind pe tabla
obtinuta in urma primului apel.
		Functia "moveLeft" realizeaza mutarea celulelor pe tabla la stanga,
care poate produce si "uniri" de celule si actualizeaza scorul. Mutarea se face
pe fiecare linie, cu ajutorul functiei "shiftHelper". Aceasta muta intai toate
zerourile la sfarsitul liniei(functia "shiftZeros") si apoi realizeaza "unirea"
celulelor, daca este cazul, calculand si scorul rezultat pentru mutarea unei
linii(functia "shiftLeft").
		Functiile "moveUp", "moveDown", "moveRight" rotesc matricea astfel
incat, dupa aplicarea functiei "moveLeft" si rotirea inapoi la pozitia initiala,
rezultatul sa fie cel dorit(prin intermediul "moveHelper").
		
	Euristica simpla
		Functia "move" alege mutarea asupra tablei de joc data ca parametru
care maximizeaza numarul de celule libere. Din lista cu toate cele patru
mutari, le pastrez doar pe cele valide, care produc o schimbare in configuratia
tablei. Acestora le atribui numarul de celule libere (functia "freeCells"),
formand perechi, apoi sortez crescator aceste perechi dupa numarul de celule
libere si intorc tabla din ultima pereche din lista (cu cel mai mare numar).
		Functia "play" verifica daca jocul este castigat, caz in care intoarce
configuratia prezenta a tablei de joc si generatorul, sau daca, dupa plasarea
aleatoare a unei celule, jocul este pierdut, caz in care intoarce de asemenea
tabla curenta si generatorul. Altfel se plaseaza o noua celula aleator si se
aplica mutarea asupra tablei de joc.
	
	Bonus: Expectimax
		Structura de tip arbore necesara euristicii Expectimax este reprezentata
printr-o valoare a radacinii si o lista de subarbori descendenti. Aceesul la
aceste campuri se face prin intermediul functiilor "root" si respectiv "desc".
		Am implementat functiile specifice acestei euristici, la fiecare
coborand in arbore printr-un apel recursiv.
		Pentru crearea listei de functii de generare pentru fiecare nivel din
arbore am facut dispersia functiei care genereaza toate configuratiile viitoare
posibile in urma plasarii aleatoare a unei celule("genRandomFunction") prin
lista infinita formata din functiile de mutare care produc mutari valide
("moveFunctions").
		Am ales ca numarul de nivele pe care arborele se dezvolta sa fie 3
deoarece, pentru valori mai mari, creste timpul de executie pana la cateva
minute, dar este suficient cat sa poata fi observata o imbunatatire a scorului
fata de euristica simpla. Am adaugat in arhiva si imagini cu exemple de
rezultate obtinute pentru 3, 4 si respectiv 5 nivele.
