#include "AST.h"
#include <iostream>
#include <fstream>
using namespace std;

ASTProgram * start;
ofstream outputFile;
extern "C" FILE *yyin;
extern "C" int yyparse();
extern "C" int yylex();

int main(int argv, char** argc) {
    // open a file handle to a particular file:
    FILE *myfile = fopen(argc[1], "r");
    // make sure it's valid:
    if (!myfile) {
        cout << "can't open input_file!" << endl;
        return -1;
    }
    yyin = myfile;
    outputFile.open ("AST.txt");
    do {
        yyparse();
    } while (!feof(yyin));
    cout << "Success" << endl;

    PrintVisitor * foo = new PrintVisitor();
    foo -> visit(start, 0);
    outputFile.close();
}
