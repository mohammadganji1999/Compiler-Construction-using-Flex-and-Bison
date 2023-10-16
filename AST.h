#include <vector>
#include <string>
#include <fstream>
#include <iostream>
#include "Visitor.h"
extern "C" ofstream outputFile;
#define TAB 1

using namespace std;

class ASTProgram;
class ASTFieldDeclaration;
class ASTIdComma;
class ASTVarIdentifier;
class ASTArrayIdentifier;
class ASTMethodDeclaration;
class ASTArgument;
class ASTBlock;
class ASTVarDeclaration;
class ASTStatement;
class ASTIdCommas;
class ASTBreakStatement;
class ASTExpression;
class ASTBinaryExpression;
class ASTUnaryExpression;
class ASTReturnStatement;
class ASTContinueStatement;
class ASTIfStatement;
class ASTAssignmentStatement;
class ASTForStatement;
class ASTIfElseStatement;
class ASTBlockStatement;
class ASTLocation;
class ASTVarLocation;
class ASTArrayLocation;
class ASTLiteral;
class ASTIntLiteral;
class ASTCharLiteral;
class ASTBoolLiteral;
class ASTAssignOperator;
class ASTMethodCall;
class ASTNameMethodCall;
class ASTCallout;
class ASTCalloutArgument;
class ASTCalloutArgumentExpr;
class ASTCalloutArgumentString;

union Node{
    int ival;
    bool bval;
    char cval;
    char *sval;
    ASTProgram* program;
    vector<ASTFieldDeclaration*>* field_declarations;
    vector<ASTMethodDeclaration*>* method_declarations;
    vector<ASTIdComma*>* id_comma;
    ASTFieldDeclaration* field_declaration;
    ASTMethodDeclaration* method_declaration;
    vector<ASTArgument*>* arguments;
    ASTArgument* argument;
    ASTBlock* block;
    vector<ASTVarDeclaration*>* var_declarations;
    ASTVarDeclaration* var_declaration;
    vector<ASTStatement*>* statements;
    ASTStatement* statement;
    vector<ASTIdCommas*>* id_commas;
    vector<ASTExpression*>* expr_comma;
    ASTLocation* location;
    ASTExpression* expr;
    ASTLiteral* literal;
    ASTAssignOperator* assign_op;
    ASTMethodCall* method_call;
    vector<ASTCalloutArgument*>* callout_arg_comma;
};

typedef union Node YYSTYPE;

enum class arithematic_op {
    plus_op,
    minus_op,
    multiply_op,
    divide_op,
    modulo_op,
    and_and,
    or_or,
    less_than,
    more_than,
    less_than_equal,
    more_than_equal,
    equal_equal,
    not_equal,
    unot
};


enum class assign_op {
    equal,
    plus_equal,
    minus_equal
};

enum class datatype {
    int_type,
    bool_type,
    void_type
};

class ASTProgram {
    vector<ASTFieldDeclaration*> *fieldDecls;
    vector<ASTMethodDeclaration*> *methodDecls;
    public:
        ASTProgram(vector<ASTFieldDeclaration*>* fieldDecls, vector<ASTMethodDeclaration*>* methodDecls) {
            this->fieldDecls = fieldDecls;
            this->methodDecls = methodDecls;
        }
        
        ~ASTProgram() {
        }
        
        vector<ASTFieldDeclaration*>* getFieldDecls() {
            return this->fieldDecls;
        }
        
        vector<ASTMethodDeclaration*>* getMethodDecls() {
            return this->methodDecls;
        }
};


class ASTFieldDeclaration {
    datatype type;
    vector < ASTIdComma *> * idList;
        public:
            ASTFieldDeclaration(datatype type, vector < ASTIdComma *> * idList){
                this -> type = type;
                this -> idList = idList;
            }
            ~ASTFieldDeclaration(){}
            datatype getType(){ return this -> type; }
            vector < ASTIdComma *> * getIdList(){ return this -> idList; }
            void accept(visitor * v, int level) { v -> visit(this, level); }
};

class ASTIdComma {
    public:
        ASTIdComma(){};
        ~ASTIdComma(){};
        virtual void accept(visitor * v, int level) = 0;      
};

class ASTVarIdentifier : public ASTIdComma{
    string id;
    public:
        ASTVarIdentifier(string id) { this -> id = id; };
        ~ASTVarIdentifier(){};

        string getId(){ return this -> id; }
        void accept(visitor * v, int level) { v -> visit(this, level); }
};

class ASTArrayIdentifier : public ASTIdComma{
    string id;
    int size;
    public:
        ASTArrayIdentifier(string id, int size){
            this -> id = id;
            this -> size = size;
        }
        ~ASTArrayIdentifier(){};

        string getId(){ return this -> id; }
        int getSize(){ return this -> size; }
        void accept(visitor * v, int level) { v -> visit(this, level); }
};


class ASTMethodDeclaration {
    datatype type;
    string id;
    vector<ASTArgument*>* argument;
    ASTBlock* block;
    
    public:
        ASTMethodDeclaration(datatype type, string id, vector<ASTArgument*>* argument, ASTBlock* block) {
            this->type = type;
            this->id = id;
            this->argument = argument;
            this->block = block;
        }
        
        ~ASTMethodDeclaration() {
        }
        
        datatype getType() {
            return this->type;
        }
  
        string getId() {
            return this->id;
        }
  
        vector<ASTArgument*>* getArgument() {
            return this->argument;
        }
        
        ASTBlock* getBlock() {
            return this->block;
        }
        void accept(visitor* v, int level) {
            v->visit(this, level);
        }
};


class ASTArgument {
    datatype type;
    string id;
    public:
        ASTArgument(datatype type, string id) {
            this->type = type;
            this->id = id;
        }
        
        datatype getType() {
            return this->type;
        }
  
        string getId() {
            return this->id;
        }
  
        ~ASTArgument() {

        }
        void accept(visitor* v, int level) {
            v->visit(this, level);
        }
};


class ASTBlock {
    vector< ASTVarDeclaration*>* varDecls;
    vector< ASTStatement*>* statements;
    public:
        ASTBlock(vector<ASTVarDeclaration*>* varDecls, vector<ASTStatement*>* statements) {
            this->varDecls = varDecls;
            this->statements = statements;
        }
        ~ASTBlock() {

        }
        vector<ASTVarDeclaration*>* getVarDecls() {
            return this->varDecls;
        }
        vector<ASTStatement*>* getStatements() {
            return this->statements;
        }
        void accept(visitor* v, int level) {
            v->visit(this, level);
        }
};

class ASTVarDeclaration {
    datatype type;
    vector<ASTIdCommas*>* idCommas;
    public:
        ASTVarDeclaration(datatype type, vector<ASTIdCommas*>* idCommas) {
            this->type = type;
            this->idCommas = idCommas;
        }
        ~ASTVarDeclaration(){
          
        }
        datatype getType() {
            return this->type;
        }
        vector<ASTIdCommas*>* getIdCommas() {
            return this->idCommas;
        }
        void accept(visitor* v, int level) {
            v->visit(this, level);
        }
};


class ASTStatement {
    public:
        ASTStatement() {
          
        }
        ~ASTStatement() {
          
        }

        virtual void accept(visitor* v, int level) = 0;
};

class ASTIdCommas {
      string id;
      public:
        ASTIdCommas(string id) {
            this->id = id;
        }
        ~ASTIdCommas() {
          
        }
        string getId() {
            return this->id;
        }
        void accept(visitor* v, int level) {
            v->visit(this, level);
        }
};

class ASTBreakStatement: public ASTStatement {
    public:
        ASTBreakStatement() {
        }
        ~ASTBreakStatement() {
        }
        
        void accept(visitor* v, int level) {
            v->visit(this, level);
        }
};

class ASTExpression {
      public:
        ASTExpression() {
        }
        ~ASTExpression() {
        }
        
        virtual void accept(visitor* v, int level) = 0;
};


class ASTBinaryExpression : public ASTExpression {
    ASTExpression* lhs, *rhs;
    arithematic_op oper;
    
    public:
        ASTBinaryExpression(ASTExpression* lhs, arithematic_op oper, ASTExpression* rhs) {
            this->lhs = lhs;
            this->oper = oper;
            this->rhs = rhs;
        }
        
        ~ASTBinaryExpression() {
        }
        
        ASTExpression* getLhs() {
            return this->lhs;
        }
        
        ASTExpression* getRhs() {
            return this->rhs;
        }
        
        arithematic_op getOper() {
            return this->oper;
        }
        
        void accept(visitor* v, int level) {
            v->visit(this, level);
        }
        
};

class ASTUnaryExpression : public ASTExpression {
    ASTExpression* expr;
    arithematic_op oper;
    
    public:
        ASTUnaryExpression(arithematic_op oper, ASTExpression* expr) {
            this->expr = expr;
            this->oper = oper;
        }
        
        ~ASTUnaryExpression() {
        }
        
        ASTExpression* getExpr() {
            return this->expr;
        }
        
        arithematic_op getOper() {
            return this->oper;
        }
        
        void accept(visitor* v, int level) {
            v->visit(this, level);
        }
};


class ASTReturnStatement: public ASTStatement {
    ASTExpression* expr;
    public:
        ASTReturnStatement(ASTExpression* expr) {
            this->expr = expr;
        }
        ~ASTReturnStatement() {
        }
        ASTExpression* getExpr() {
            return this->expr;
        }
        void accept(visitor* v, int level) {
            v->visit(this, level);
        }
};


class ASTContinueStatement: public ASTStatement {
    public:
        ASTContinueStatement() {
        }
        ~ASTContinueStatement() {
        }
  
        void accept(visitor* v, int level) {
            v->visit(this, level);
        }
};

class ASTIfStatement: public ASTStatement {
    ASTExpression* expr;
    ASTBlock* block;
    public:
        ASTIfStatement(ASTExpression* expr, ASTBlock* block) {
            this->expr = expr;
            this->block = block;
        }
  
        ~ASTIfStatement() {
        }
  
        ASTExpression* getExpr() {
            return this->expr;
        }
        
        ASTBlock* getBlock() {
            return this->block;
        }
        void accept(visitor* v, int level) {
            v->visit(this, level);
        }
};

class ASTAssignmentStatement : public ASTStatement {
    ASTLocation* location;
    ASTAssignOperator* oper;
    ASTExpression* expr;
    public:
        ASTAssignmentStatement(ASTLocation* location, ASTAssignOperator* oper, ASTExpression* expr) {
            this->location = location;
            this->oper = oper;
            this->expr = expr;
        }
        ~ASTAssignmentStatement() {

        }
        ASTLocation* getLocation() {
            return this->location;
        }
        ASTAssignOperator* getOper() {
            return this->oper;
        }
        ASTExpression* getExpr() {
            return this->expr;
        }
        void accept(visitor* v, int level) {
            v->visit(this, level);
        }

};


class ASTForStatement: public ASTStatement {
    string id;
    ASTExpression* expr1, *expr2;
    ASTBlock* block;
    public:
        ASTForStatement(string id, ASTExpression* expr1, ASTExpression* expr2, ASTBlock* block) {
            this->id = id;
            this->expr1 = expr1;
            this->expr2 = expr2;
            this->block = block;
        }

        ~ASTForStatement() {

        }

        string getId() {
            return this->id;
        }

        ASTExpression* getExpr1() {
            return this->expr1;
        }

        ASTExpression* getExpr2() {
            return this->expr2;
        }

        ASTBlock* getBlock() {
            return this->block;
        }

        void accept(visitor* v, int level) {
            v->visit(this, level);
        }
};

class ASTIfElseStatement: public ASTStatement {
    ASTExpression* expr;
    ASTBlock* ifBlock;
    ASTBlock* elseBlock;
    public:
        ASTIfElseStatement(ASTExpression* expr, ASTBlock* ifBlock, ASTBlock* elseBlock) {
            this->expr = expr;
            this->ifBlock = ifBlock;
            this->elseBlock = elseBlock;
        }
  
        ~ASTIfElseStatement() {
        }
        
        ASTExpression* getExpr() {
            return this->expr;
        }
        ASTBlock* getIfBlock() {
            return this->ifBlock;
        }
        ASTBlock* getElseBlock() {
            return this->elseBlock;
        }
       void accept(visitor* v, int level) {
            v->visit(this, level);
        }
};


class ASTBlockStatement: public ASTStatement {
    ASTBlock* block;
    public:
        ASTBlockStatement(ASTBlock* block) {
            this->block = block;
        }
        ~ASTBlockStatement() {
        }
        ASTBlock* getBlock() {
            return this->block;
        }
        void accept(visitor* v, int level) {
            v->visit(this, level);
        }
        
};


class ASTLocation : public ASTExpression {
    public:
        ASTLocation() {
        }
        ~ASTLocation() {
        }
        virtual void accept(visitor* v, int level) = 0;
};


class ASTVarLocation : public ASTLocation {
    string id;
    public:
        ASTVarLocation(string id) {
            this->id = id;
        }
        ~ASTVarLocation(){
        }
        string getId() {
            return this->id;
        }
  
        void accept(visitor* v, int level) {
            v->visit(this, level);
        }
};


class ASTArrayLocation : public ASTLocation {
    string id;
    ASTExpression* expr;
    public:
        ASTArrayLocation(string id, ASTExpression* expr) {
            this->id = id;
            this->expr = expr;
        }
        ~ASTArrayLocation() {
        }
        string getId() {
            return this->id;
        }
        ASTExpression* getExpr(){
          return this -> expr;
        }
        void accept(visitor* v, int level) {
            v->visit(this, level);
        }
};

class ASTLiteral : public ASTExpression {
    public:
        ASTLiteral() {
        }
        ~ASTLiteral() {
        }
        virtual void accept(visitor* v, int level) = 0;
};

class ASTIntLiteral : public ASTLiteral {
    int value;
    public:
        ASTIntLiteral(int value) {
            this->value = value;
        }
        ~ASTIntLiteral() {
        }
        int getValue() {
            return this->value;
        }
        void accept(visitor* v, int level) {
            v->visit(this, level);
        }
};

class ASTCharLiteral : public ASTLiteral {
    char value;
    public:
        ASTCharLiteral(char value) {
            this->value = value;
        }
        ~ASTCharLiteral() {
        }
        char getValue() {
            return this->value;
        }
        void accept(visitor* v, int level) {
            v->visit(this, level);
        }
};


class ASTBoolLiteral : public ASTLiteral {
    bool value;
    public:
        ASTBoolLiteral(bool value) {
            this->value = value;
        }
        ~ASTBoolLiteral() {
        }
        bool getValue() {
            return this->value;
        }
        void accept(visitor* v, int level) {
            v->visit(this, level);
        }
};


class ASTAssignOperator {
    assign_op oper;
    public:
        ASTAssignOperator(assign_op oper) {
            this->oper = oper;
        }
        ~ASTAssignOperator() {

        }

        assign_op getOper() {
            return this->oper;
        }

        void accept(visitor* v, int level) {
            v->visit(this, level);
        }

};


class ASTMethodCall : public ASTStatement, public ASTExpression {
    public:
        ASTMethodCall() {

        }
        ~ASTMethodCall() {

        }

        virtual void accept(visitor* v, int level) = 0;
};

class ASTNameMethodCall : public ASTMethodCall {
    string id;
    vector<ASTExpression*>* exprComma;
    public:
        ASTNameMethodCall(string id, vector<ASTExpression*>* exprComma) {
            this->id = id;
            this->exprComma = exprComma;
        }
        ~ASTNameMethodCall() {

        }

        string getId() {
            return this->id;
        }

        vector<ASTExpression*>* getExprComma() {
            return this->exprComma;
        }

        void accept(visitor* v, int level) {
            v->visit(this, level);
        }
};


class ASTCallout : public ASTMethodCall {
    string value;
    vector<ASTCalloutArgument*>* args;
    public:
        ASTCallout(string value, vector<ASTCalloutArgument*>* args) {
            this->value = value;
            this->args = args;
        }
        ~ASTCallout() {

        }
        string getValue() {
            return this->value;
        }
        vector<ASTCalloutArgument*>* getArgs() {
            return this->args;
        }
        void accept(visitor* v, int level) {
            v->visit(this, level);
        }

};

class ASTCalloutArgument {
    public:
        ASTCalloutArgument() {

        }
        ~ASTCalloutArgument() {

        }
        virtual void accept(visitor* v, int level) = 0;
};

class ASTCalloutArgumentExpr : public ASTCalloutArgument {
    ASTExpression* expr;
    public:
        ASTCalloutArgumentExpr(ASTExpression* expr) {
            this->expr = expr;
        }
        ~ASTCalloutArgumentExpr() {

        }

        ASTExpression* getExpr() {
            return this->expr;
        }
        void accept(visitor* v, int level) {
            v->visit(this, level);
        }
};

class ASTCalloutArgumentString : public ASTCalloutArgument {
    string value;
    public:
        ASTCalloutArgumentString(string value) {
            this->value = value;
        }
        ~ASTCalloutArgumentString() {

        }
        string getValue() {
            return this->value;
        }
        void accept(visitor* v, int level) {
            v->visit(this, level);
        }

};

class PrintVisitor : public visitor{
    public:
        void printTabs(int tabsCount){ 
            for(int i = 0; i < tabsCount; i++) 
                outputFile << "\t";
            return;
        }

        void visit(ASTProgram * node, int level){
            printTabs(level);
            outputFile << "<program>" << endl;
            printTabs(level + 1);
            outputFile << "<field_declarations count=\"";
            if(node -> getFieldDecls() != NULL){
                outputFile << node -> getFieldDecls() -> size() << "\">" << endl;
                for(auto it : *(node -> getFieldDecls())) it -> accept(this, level + 2);
            }
            else outputFile << "0" << "\">" << endl;
            printTabs(level + 1);
            outputFile << "</field_declarations>" << endl; 
            printTabs(level + 1);
            outputFile << "<method_declarations count=\"";
            if(node -> getMethodDecls() != NULL){
                outputFile << node -> getMethodDecls() -> size() << "\">" << endl;
                for(auto it : *(node -> getMethodDecls())) it -> accept(this, level + 2);   
            }
            else outputFile << "0" << "\">" << endl;
            printTabs(level + 1);
            outputFile << "</method_declarations>" << endl;
            outputFile << "</program>" << endl;
            return;
        }

        void visit(ASTFieldDeclaration * node, int level){
            printTabs(level);
            outputFile << "<field_declaration type=\"";
            if(node -> getType() == datatype::int_type) outputFile << "integer";
            else outputFile << "boolean";
            outputFile << "\">" << endl;
            for(auto it : *(node -> getIdList())) it -> accept(this, level + 1);
            printTabs(level);
            outputFile << "</field_declaration>" << endl;
            return;
        }

        void visit(ASTVarIdentifier * node, int level){
            printTabs(level);
            outputFile << "<declaration name=\"" << node -> getId() << "\" />" << endl;
            return;
        }

        void visit(ASTArrayIdentifier * node, int level){
            printTabs(level);
            outputFile << "<declaration name=\"" << node -> getId() << "\" count=\"" << node -> getSize() << "\" />" << endl;
            return;
        }

        void visit(ASTMethodDeclaration * node, int level){
            printTabs(level);
            outputFile << "<method_declaration type=\"";
            if(node -> getType() == datatype::int_type) outputFile << "integer";
            else if(node -> getType() == datatype::bool_type) outputFile << "boolean";
            else outputFile << "void" ;
            outputFile << "\" id=\"" << node -> getId() << "\">" << endl;
            printTabs(level + 1);
            outputFile <<  "<arguments count=\"" << node -> getArgument() -> size() << "\">" << endl;
            for(auto it : *(node -> getArgument())) it -> accept(this, level + 2);
            printTabs(level + 1);
            outputFile << "</arguments>" << endl;
            printTabs(level + 1);
            outputFile << "<block>" << endl;
            node -> getBlock() -> accept(this, level + 2);
            printTabs(level + 1);
            outputFile << "</block>" << endl;
            printTabs(level);
            outputFile << "</method_declaration>" << endl;
            return;
        }

        void visit(ASTArgument * node, int level){
            printTabs(level);
            outputFile << "<argument id=\"" << node -> getId() << "\" type=\"";
            if(node -> getType() == datatype::int_type) outputFile << "integer";
            else if(node -> getType() == datatype::bool_type) outputFile << "boolean";
            else outputFile << "void";
            outputFile << "\" />" << endl;
            return;
        }

        void visit(ASTBlock * node, int level){
            printTabs(level);
            outputFile << "<var_declarations count=\"";
            if(node -> getVarDecls() != NULL){
                outputFile << node -> getVarDecls() -> size() << "\">" << endl;
                for(auto it : *(node -> getVarDecls())) it -> accept(this, level + 1);    
            }
            else outputFile << "0\">" << endl;
            printTabs(level);
            outputFile << "</var_declarations>" << endl;
            printTabs(level);
            outputFile << "<statements outputFile=\"";
            if(node -> getStatements() != NULL){
                outputFile << node -> getStatements() -> size() << "\">" << endl;
                for(auto it : *(node -> getStatements())) it -> accept(this, level + 1);
            }
            else outputFile << "0\">" << endl;
            printTabs(level);
            outputFile << "</statements>" << endl;
            return;
        }

        void visit(ASTVarDeclaration * node, int level){
            printTabs(level);
            outputFile << "<var_declaration type=\"";
            if(node -> getType() == datatype::int_type) outputFile << "integer";
            else if(node -> getType() == datatype::bool_type) outputFile << "boolean";
            else outputFile << "void";
            outputFile << "\">" << endl;
            for(auto it : *(node -> getIdCommas())) it -> accept(this, level + 1);
            printTabs(level);
            outputFile << "</var_declaration>" << endl;
            return;
        }

        void visit(ASTIdCommas * node, int level){
            printTabs(level);
            outputFile << "<id=\"" << node -> getId() << "\" />" << endl;
            return;
        }
  
        void visit(ASTBreakStatement * node, int level){
            printTabs(level);
            outputFile << "<break encountered />" << endl;
            return;
        }

        void visit(ASTContinueStatement * node, int level){
            printTabs(level);
            outputFile << "<continue encountered />" << endl;
            return;
        }

        void visit(ASTBinaryExpression * node, int level){
            printTabs(level);
            outputFile << "<binary_expression arithematic_operation=\"" ;
            if(node -> getOper() == arithematic_op::plus_op) outputFile << "addition";
            else if(node -> getOper() == arithematic_op::minus_op) outputFile << "subtraction";
            else if(node -> getOper() == arithematic_op::divide_op) outputFile << "division";
            else if(node -> getOper() == arithematic_op::multiply_op) outputFile << "multiply";
            else outputFile << "modulus";
            outputFile << "\">" << endl;
            printTabs(level + 1);
            outputFile << "<left_hand_expression>" << endl;
            node -> getLhs() -> accept(this, level + 2);
            printTabs(level + 1);
            outputFile << "</left_hand_expression>" << endl;
            printTabs(level + 1);
            outputFile << "<right_hand_expression>" << endl;
            node -> getRhs() -> accept(this, level + 2);
            printTabs(level + 1);
            outputFile << "</right_hand_expression>" << endl;
        }
  
        void visit(ASTVarLocation * node, int level){
            printTabs(level);
            outputFile << "<var_location id=\"" << node -> getId() << "\" />" << endl;
            return;
        }

        void visit(ASTArrayLocation * node, int level){
            printTabs(level);
            outputFile << "<array_location id=\"" << node -> getId() << "\">" << endl;
            printTabs(level + 1);
            outputFile << "<size_expression/>" << endl;
            node -> getExpr() -> accept(this, level + 2);
            printTabs(level + 1);
            outputFile << "</size_expression>" << endl;
            printTabs(level);
            outputFile << "</array_location>" << endl;
            return;
        }

        void visit(ASTAssignOperator * node, int level){
            if(node -> getOper() == assign_op::equal) outputFile << "equal";
            else if(node -> getOper() == assign_op::plus_equal) outputFile << "plus_equal";
            else outputFile << "minus_equal";
            return;
        }

        void visit(ASTAssignmentStatement * node, int level){
            printTabs(level);
            outputFile << "<assignment_statement>" << endl;
            printTabs(level + 1);
            outputFile << "<location>" << endl;
            node -> getLocation() -> accept(this, level + 2);
            printTabs(level + 1);
            outputFile << "</location>" << endl;
            printTabs(level + 1);
            outputFile << "<operator=\"";
            node -> getOper() -> accept(this, level); 
            outputFile << "\" />" << endl;
            printTabs(level + 1);
            outputFile << "<expression>" << endl;
            node -> getExpr() -> accept(this, level + 2);
            printTabs(level + 1);
            outputFile << "</expression>" << endl;
            printTabs(level);
            outputFile << "</assignment_statement>" << endl;
            return;
        }

        void visit (ASTNameMethodCall * node, int level){
            printTabs(level);
            outputFile << "<method_call name=\"" << node -> getId() << "\">" << endl;
            printTabs(level + 1);
            outputFile << "<arguments count=\"" << node -> getExprComma() -> size() << "\">" << endl;
            for(auto it : *(node -> getExprComma())) it -> accept(this, level + 2);
            printTabs(level + 1);
            outputFile << "</arguments>" << endl;
            printTabs(level);
            outputFile << "</method_call>" << endl;
            return;
        }

        void visit(ASTCalloutArgumentString * node, int level){
            printTabs(level);
            outputFile << "<string argument=\"" << node -> getValue() << "\" />" << endl;
            return;
        }
  
        void visit(ASTUnaryExpression * node, int level) {
            printTabs(level);
            if(node->getOper() == arithematic_op::minus_op) {
                outputFile << "<unary_minus>" << endl;
                node->getExpr()->accept(this, level+1);
                printTabs(level);
                outputFile << "</unary_minus>" << endl;
            }
            else {
                outputFile << "<unary_not>" << endl;
                node->getExpr()->accept(this, level+1);
                printTabs(level);
                outputFile << "</unary_not>" << endl;   
            }
        }

        void visit(ASTIntLiteral* node, int level) {
            printTabs(level);
            outputFile << "<literal type=\"integer\" value=\"" << node->getValue() << "\" />" << endl;
            return;
        }

        void visit(ASTCharLiteral* node, int level) {
            printTabs(level);
            outputFile << "<literal type=\"character\" value=\"" << node->getValue() << "\" />" << endl;
            return;
        }

        void visit(ASTBoolLiteral* node, int level) {
            printTabs(level);
            if(node->getValue())
                outputFile << "<literal type=\"integer\" value=\"true\" />" << endl;
            else
                outputFile << "<literal type=\"integer\" value=\"false\" />" << endl;
            return;
        }

        void visit(ASTBlockStatement* node, int level) {
            node->getBlock()->accept(this, level+1);
        }

        void visit(ASTReturnStatement* node, int level) {
            printTabs(level);
            outputFile << "<return>" << endl;
            if(node->getExpr() != NULL) {
                node->getExpr()->accept(this, level+1);
            }
            printTabs(level);
            outputFile << "</return>" << endl;
        }

        void visit(ASTIfStatement* node, int level) {
            printTabs(level);
            outputFile << "<if>" << endl;
            printTabs(level + 1);
            outputFile << "<condition>" << endl;
            node->getExpr()->accept(this, level+2);
            printTabs(level + 1);
            outputFile << "</condition>" << endl;
            printTabs(level + 1);
            outputFile << "<body>" << endl;
            node->getBlock()->accept(this, level+2);
            printTabs(level + 1);
            outputFile << "</body>" << endl;
            printTabs(level);
            outputFile << "</if>" << endl;
        }

        void visit(ASTIfElseStatement* node, int level) {
            printTabs(level);
            outputFile << "<if_else>" << endl;
            printTabs(level + 1);
            outputFile << "<condition>" << endl;
            node->getExpr()->accept(this, level+2);
            printTabs(level + 1);
            outputFile << "</condition>" << endl;
            printTabs(level + 1);
            outputFile << "<if_body>" << endl;
            node->getIfBlock()->accept(this, level+2);
            printTabs(level + 1);
            outputFile << "</if_body>" << endl;
            printTabs(level + 1);
            outputFile << "<else_body>" << endl;
            node->getElseBlock()->accept(this, level+2);
            printTabs(level + 1);
            outputFile << "</else_body>" << endl;
            printTabs(level);
            outputFile << "</if_else>" << endl;
        }

        void visit(ASTForStatement* node, int level) {
            printTabs(level);
            outputFile << "<for name=\"" << node->getId() << "\" >" << endl;
            printTabs(level+1);
            outputFile << "<expression_1>" << endl;
            node->getExpr1()->accept(this, level+2);
            printTabs(level+1);
            outputFile << "</expression_1>" << endl;
            printTabs(level+1);
            outputFile << "<expression_2>" << endl;
            node->getExpr2()->accept(this, level+2);
            printTabs(level+1);
            outputFile << "</expression_2>" << endl;
            printTabs(level+1);
            outputFile << "<block>" << endl;
            node->getBlock()->accept(this, level+2);
            printTabs(level+1);
            outputFile << "</block>" << endl;
            printTabs(level);
            outputFile << "</for>" << endl;
        }
        void visit(ASTCallout* node, int level) {
            if(node->getArgs() != NULL) {
                printTabs(level);
                outputFile << "<callout name=\"" << node->getValue() << "\" >" << endl; 
                printTabs(level + 1);
                outputFile << "<arguments size=\"" << node->getArgs()->size() << "\" >" << endl;
                for(auto it : *(node->getArgs()))
                    it->accept(this, level+2);
                printTabs(level+1);
                outputFile << "</arguments>" << endl;
                printTabs(level);
                outputFile << "</callout>" << endl;
            }
            else {
                printTabs(level);
                outputFile << "<callout name=\"" << node->getValue() << "\" />" << endl; 
            }
            
        }
        void visit(ASTCalloutArgumentExpr* node, int level) {
            node->getExpr()->accept(this, level);
        }

};