all:
	bison -d parser.y
	flex scanner.l
	g++ --std=c++11 parser.tab.c lex.yy.c main.cpp -L/usr/local/opt/bison/lib -Wno-deprecated-register -o compiler


clean:
	rm -f parser.tab.* lex.yy.c AST.txt a.out compiler
