# include <cctype>
# include <cstdlib>
# include <cstring>
# include <cstdio>
# include <string.h>
# include <stdio.h>
# include <iostream>
# include <map>
# include <string>
# include <vector>
# include <sstream>
# include <exception>
# include <iomanip>
# define cmdNum 39

using namespace std;
int gLine = 0, gCol = 0 ;
int gparennum = 0  ;
bool geof = false ; // 若本行就遇到eof
bool gfirstprint = true ;
enum TokenType {
  EMPTY, NIL, INT, FLOAT, T, STRING, SHARP, DOT,
  OPERATOR, LEFT_PAREN, RIGHT_PAREN, SYMBOL, EXIT, QUOTE, DOUBLEQUOTE, LINECOMMENT, // proj1
  CONS, LIST, DEFINE, CAR, CDR, ISPAIR, ISLIST, ISATOM, ISNULL,          // proj2
  ISINT, ISREAL, ISNUM, ISSTR, ISBOOL, ISSYM, NOT, AND, OR, BIGG, BIGEQ,
  SML, SMLEQ, EQ, STRAPP, STRBIG, ADD, SUB, MULT, DIV,
  STRSML, STREQL, ISEQV, ISEQL, BEGIN, IF, COND, CLEAN

};

string gInternalCmd[ cmdNum ] = { "cons", "list", "quote", "define", "car", "cdr",
                         "pair?", "list?", "atom?", "null?", "integer?", "real?", "number?",
                         "string?", "boolean?", "symbol?", "+", "-", "*",
                         "/", "not", "and", "or", ">", ">=", "<", "<=",
                         "=", "string-append", "string>?", "string<?", "string=?", "eqv?", "equal?",
                         "begin", "if", "cond", "clean-environment", "exit"
                                } ;

struct Token {
  int mTokentype  ;
  string mContent  ;
  int col ;
  int line ;
};




int gTestnum = 0 ;

class List {
  public :
  vector<Token> mtoken ;
  List * mright  ;
  List * mleft ;
  bool mhasRightparen  ;
  bool mhasQuote ;
  bool miseva ;
  List() {
    mright = NULL ;
    mleft = NULL ;
    mhasRightparen = false ;
    mhasQuote = false  ;
    miseva = false ;
  } // List()
} ;

typedef List * Listptr ;

struct DefineSym {
  string symbol ;
  bool iscommand  ;
  Listptr defineptr ;
};

vector< Listptr >  gfix ;
vector <DefineSym> gdefine ;
Listptr  gfixptr ;

class Ourscheme {
public :

  bool IsSexepresion( vector<Token> test ) {
    int numofleft = 0 ;
    int numofright = 0 ;
    int quote = 0 ;
    int els = 0 ;
    for ( int i = 0 ; i < test.size() ; i++ ) {
      if ( test[i].mTokentype == LEFT_PAREN ) {
        numofleft++;
      } // if

      if ( test[i].mTokentype == RIGHT_PAREN  ) {
        numofright++;
      } // if

      if (  test[i].mTokentype == QUOTE ) {
        quote++ ;
      } // if
      else {
        els++ ;
      } // else

      if ( quote > 0 ) {
        if ( ( numofleft == 0  ||  numofright == 0 ) && els == 0 )  // '
          ;

        else if ( ( numofleft == 0  && numofright == 0 ) && els != 0 ) { // 'Atom
          return true ;
        } // else if

        else if ( numofleft == numofright ) {  // ' ( sexp )
          return true  ;
        } // else if

      } // if

      else if  ( numofleft == numofright ) // ( sexp ) or Atom
        return true  ;

    } // for

    return false ;
  } // IsSexepresion()

  void FixSexp( vector<Token> &sexp ) {   //  把sexp () 變成nil  Float 變成 取3位 SYMBOL
    for ( int i = 0 ; i < sexp.size() ; i++ ) {
      if ( sexp[i].mTokentype == LEFT_PAREN ) {
        if ( i + 1 < sexp.size() ) {
          if ( sexp[i+1].mTokentype == RIGHT_PAREN ) {
            sexp[i].mContent = "nil" ;
            sexp[i].mTokentype = NIL ;
            sexp.erase( sexp.begin() + i + 1 ) ;
          } // if
        }  // if
      } // if

      else if ( sexp[i].mTokentype == FLOAT ) {
        stringstream ss ;
        ss << fixed << setprecision( 3 ) << atof( sexp[i].mContent.c_str() ) ;
        sexp[i].mContent = ss.str() ;
      } // else if

      else if ( sexp.size() > 1 && sexp[i].mContent == "quote" && sexp[i-1].mTokentype == LEFT_PAREN  ) {
        sexp[i].mTokentype = QUOTE ;
        sexp.erase( sexp.begin() + i -1 ) ;
        for ( int a = 0 ; a < sexp.size() ; a++ ) {
          if ( sexp[a].mTokentype == RIGHT_PAREN ) {
            sexp.erase( sexp.begin() + a ) ;
            a = sexp.size() ;
          } // if
        } // for
      } // else if
    } // for
  } // FixSexp()

  void FixToken( Token &temp ) {
    int digit = 0 ;
    if ( temp.mTokentype == FLOAT ) {
      for ( int i = 0 ; i < temp.mContent.size() ; i++ ) {
        if ( !isdigit( temp.mContent[i] ) && temp.mContent[i] != '.' &&  temp.mContent[i] != '-'
             && temp.mContent[i] != '+'  ) {

          temp.mTokentype = SYMBOL ;
        } // if

        if ( isdigit( temp.mContent[i] ) )
          digit ++ ;

      } // for

      if ( digit == 0 )
        temp.mTokentype = SYMBOL ;
    } // if


    if ( temp.mTokentype == T ) {
      temp.mContent = "#t" ;
    } // if

    if ( temp.mTokentype == NIL ) {
      temp.mContent = "nil" ;
    } // if

  } // FixToken()

  bool CheckDoterror( vector<Token> sexp, int &fault ) {
    bool level[30] = {false} ;
    int nowlevel = 0 ;
    for ( int i = 0 ; i < sexp.size() ; i++ ) {
      if ( sexp[i].mTokentype == LEFT_PAREN ) {
        nowlevel++ ;
      } // if

      if ( sexp[i].mTokentype == RIGHT_PAREN ) {
        level[nowlevel] = false ;
        nowlevel-- ;
      } // if

      if ( sexp[i].mTokentype == DOT ) {  // 同一個( 中間有兩個DOT )
        if ( level[nowlevel] == true ) {
          fault = i ;
          return true ;
        } // if
        else {
          level[nowlevel] = true ;
        } // else

      } // if
    } // for

    return false ;

  } // CheckDoterror()

  bool CheckError( vector<Token> sexp  ) {
    int i = 0 ;
    int errordot = 0 ;

    while ( i < sexp.size() ) {

      if ( CheckDoterror( sexp, errordot ) ) {

        cout << endl ;
        cout << "> " << "ERROR (unexpected token) : ')' expected when token at Line " ;
        cout << sexp[errordot].line  ;
        cout << " Column " ;
        cout << sexp[errordot].col ;
        cout << " is >>" << sexp[errordot].mContent << "<<" << endl ;
        i = i + 2 ;
        return true ;
      } // if

      if (  sexp[i].mTokentype == DOT && SplitisSexepresion( sexp, i )  ) {
        if ( i+1 < sexp.size() && sexp[i+1].mTokentype != RIGHT_PAREN ) {


          cout << endl ;
          cout << "> " ;


          cout << "ERROR (unexpected token) : ')' expected when token at Line " ;
          cout << sexp[i+1].line  ;
          cout << " Column " ;
          cout << sexp[i+1].col ;
          cout << " is >>" << sexp[i+1].mContent << "<<" << endl ;
          i = i + 2 ;
          return true ;
        } // if



      } // if

      if ( sexp.size() > i + 1 && sexp[i].mTokentype == DOT
           &&  ( sexp[i+1].mTokentype == RIGHT_PAREN  ||  sexp[i+1].mTokentype ==    DOT )  ) {
        cout << endl ;
        cout << "> " << "ERROR (unexpected token) : atom or '(' expected when token at Line " ;
        cout << sexp[i+1].line  ;
        cout << " Column " ;

        cout << sexp[i+1].col ;
        cout << " is >>" << sexp[i+1].mContent << "<<" << endl ;
        i = i + 2 ;
        return true ;
      } // if

      else if (  sexp.size() > i + 1 && sexp[i].mTokentype == NIL
                && ( sexp[i+1].mTokentype == LEFT_PAREN  || sexp[i+1].mTokentype == DOT ) ) {
        if ( sexp[i-1].mTokentype == DOT ) {
          cout << endl ;
          cout << "> " << "ERROR (unexpected token) : atom or '(' expected when token at Line " ;
          cout << sexp[i+1].line  ;
          cout << " Column " ;

          cout << sexp[i+1].col ;
          cout << " is >>" << sexp[i+1].mContent << "<<" << endl ;
          i = i + 2 ;
          return true ;
        } // if
      } // else if

      else if ( sexp.size() > i + 2 && sexp[i].mTokentype == DOT
                && ( sexp[i+2].mTokentype != LEFT_PAREN  && sexp[i+2].mTokentype != RIGHT_PAREN
                     && sexp[i+2].mTokentype != QUOTE ) ) {
        if ( ( sexp[i+1].mTokentype != LEFT_PAREN && sexp[i+1].mTokentype != RIGHT_PAREN
               && sexp[i+1].mTokentype != QUOTE ) ) {
          cout << endl ;
          cout << "> " << "ERROR (unexpected token) : ')' expected when token at Line " ;
          cout << sexp[i+2].line  ;
          cout << " Column " ;


          sexp[i+2].col = sexp[i+2].col - sexp[i+2].mContent.size()  + 1 ;
          cout <<  sexp[i+2].col   ;
          cout << " is >>" << sexp[i+2].mContent << "<<" << endl ;
          i = i + 3 ;
          return true ;
        } // if
      } // else if

      else if (  sexp.size() > i + 1 && sexp[i+1].mTokentype == DOT  && sexp[i-1].mTokentype == DOT  ) {
        cout << endl ;
        cout << "> " << "ERROR (unexpected token) : atom or '(' expected when token at Line " ;
        cout << sexp[i+1].line  ;
        cout << " Column " ;
        cout << sexp[i+1].col ;
        cout << " is >>" << sexp[i+1].mContent << "<<" << endl ;
        i = i + 2 ;
        return true ;
      } // else if

      else if ( sexp.size() > i + 1 && sexp[i].mTokentype == LEFT_PAREN && sexp[i+1].mTokentype == DOT  ) {
        cout << endl ;
        cout << "> " << "ERROR (unexpected token) : atom or '(' expected when token at Line " ;
        cout << sexp[i+1].line  ;
        cout << " Column " ;

        cout << sexp[i+1].col ;
        cout << " is >>" << sexp[i+1].mContent << "<<" << endl ;
        i = i + 2 ;
        return true ;
      } // else if

      else if ( sexp[i].mTokentype == STRING ) {
        if ( NumOfdoublequote( sexp[i].mContent ) == 1  ) {
          cout << endl ;
          cout << "> ERROR (no closing quote) : END-OF-LINE encountered at Line " ;
          cout << sexp[i].line ;
          cout << " Column ";

          sexp[i].col++ ;
          cout << sexp[i].col << endl ;
          return true;
        } // if
      } // else if

      else if ( sexp.size() == 1 && ( sexp[i].mTokentype == DOT ||  sexp[i].mTokentype == RIGHT_PAREN ) ) {
        cout << endl ;
        cout << "> " << "ERROR (unexpected token) : atom or '(' expected when token at Line " ;
        cout << sexp[i].line  ;
        cout << " Column " ;

        cout << sexp[i].col   ;
        cout << " is >>"  << sexp[i].mContent << "<<" << endl ;
        return true ;
      } // else if

      i++ ;
    } // while

    return false ;
  } // CheckError()

  void GetTokenlist( string str, vector<Token> &final ) {
    char buf = '\0';
    bool encounterTEnd = false; // 需要結束
    Token temp ;
    temp.mTokentype = EMPTY;
    bool encounterleftright = false ; // 區 分() 和 單獨左括號右括號
    temp.mContent = "" ;
    int strmax = str.size()-1 ; // strmax 從0開始屬
    int now = 0 ;
    bool isstring = false ;

    while (  str[now] == ' ' && now <= strmax ) {  // 一開始就遇到空白
      now++ ;
      gCol++ ;
    } // while


    while ( now <= strmax ) {

      if ( isalpha( str[now] ) && temp.mTokentype == EMPTY &&  str[now] == 't' ) {
        temp.mTokentype = T ;
        temp.mContent += str[now] ;
      } // if

      else if (  isdigit( str[now] )  &&  ( ( temp.mTokentype == INT ) || ( temp.mTokentype == EMPTY ) ) ) {
        temp.mTokentype = INT;
        temp.mContent += str[now];
      } // else if


      else if ( ( str[now] == '(' || str[now] == ')'  ||  str[now] == '\'' ) ) {   //   ( 或 )
        encounterTEnd = true ;
      } // else if

      else if ( str[now] == '.' &&  temp.mTokentype == EMPTY ) { // 單獨.
        temp.mTokentype = DOT;
        temp.mContent += str[now] ;
      } // else if


      else if ( str[now] == '\"' ) {
        temp.mTokentype = STRING;
        while ( now <= strmax  && !isstring ) {
          temp.mContent += str[now] ;
          now++ ;
          gCol++ ;
          if ( str[now] == '\"' && str[now-1] != '\\' ) {
            temp.mContent += str[now] ;
            now++ ;
            gCol++ ;
            isstring = true ;
          } // if
        } // while
        now-- ;
        gCol-- ;
      } // else if


      else if ( str[now] == '#' && temp.mTokentype == EMPTY ) {
        temp.mTokentype = SHARP;
        temp.mContent += str[now] ;
      } // else if

      else if ( ( str[now] == '+' || str[now] == '-' ) &&  temp.mTokentype == EMPTY ) {
        temp.mTokentype = OPERATOR;
        temp.mContent += str[now] ;
      } // else if



      else if ( str[now] == ';' ) {
        if (   temp.mTokentype != EMPTY )
          final.push_back( temp ) ;
        while ( now <= strmax ) {
          now++ ;

        } // while
      } // else if

      else if ( temp.mTokentype == SHARP &&  ( str[now] == 't' || str[now] == 'f' ) ) {
        if ( str[now] == 't' ) {
          temp.mTokentype =  T ;
          temp.mContent +=  str[now];
        } // if

        if ( str[now] == 'f' ) {
          temp.mTokentype = NIL;
          temp.mContent +=  str[now];
        } // if
      } // else if


      else if ( ( temp.mTokentype == INT || temp.mTokentype == OPERATOR ) &&  str[now] == '.' ) {
        temp.mTokentype =  FLOAT ;
        temp.mContent += str[now] ;
      } // else if  2.???

      else if ( temp.mTokentype == OPERATOR &&  isdigit( str[now] ) ) {
        temp.mTokentype = INT ;
        temp.mContent += str[now] ;
      } // else if

      else if ( temp.mTokentype == FLOAT  && isdigit( str[now] ) ) {
        temp.mTokentype =  FLOAT ;
        temp.mContent += str[now] ;
      } // else if 3.???

      else if ( temp.mTokentype == DOT && isdigit( str[now] ) ) {
        temp.mTokentype =  FLOAT ;
        temp.mContent += str[now] ;
      } // else if  .23

      else {
        if ( str[now] != ' '  && str[now] != '\n' &&  str[now] != '\t'  ) {
          temp.mTokentype = SYMBOL;
          temp.mContent += str[now] ;
        } // if
      } // else


      if ( temp.mContent == "nil" ) {
        temp.mTokentype = NIL ;
      }  // if 不然nil會被判定為symbol

      if  ( str[now] == ' ' ||  now == strmax || encounterTEnd || encounterleftright
            || isstring || str[now+1] == '\"'  ) {  // 切token
        FixToken( temp ) ;

        if ( str[now] == ' ' && now != strmax  ) {
          temp.col = gCol  ;
        } // if //因為遇到空白


        else if ( now == strmax && !encounterTEnd )
          temp.col = gCol + 1   ;   // 因為到最後才切

        else
          temp.col = gCol ;  // 因為左括號或右括號或quote

        temp.line = gLine;


        if ( encounterTEnd ) {               // 左括號或右括號開頭
          if ( temp.mTokentype != EMPTY && str[now] != '\n'   ) {   // 把刮號前面的切掉
            final.push_back( temp ) ;
            temp.mTokentype = EMPTY ;
            temp.mContent = "" ;
          } // if

          if ( str[now] == '(' ) {        // 遇到左括號
            temp.mTokentype = LEFT_PAREN ;
            temp.mContent = "(" ;

            temp.col = gCol + 1  ;
            temp.line = gLine;
            final.push_back( temp ) ;
          } // if

          else if ( str[now] == ')' ) {    // 遇
            temp.mTokentype = RIGHT_PAREN ;
            temp.col = gCol + 1 ;
            temp.line = gLine;
            temp.mContent = ")" ;
            final.push_back( temp ) ;
          } // else if

          else if ( str[now] == '\'' ) {    // 遇
            temp.mTokentype = QUOTE ;
            temp.col = gCol + 1 ;
            temp.line = gLine;
            final.push_back( temp ) ;
          } // else if
        } // if

        else if ( temp.mTokentype != EMPTY  && str[now] != '\n' ) {   // 怕把空白也放進去
          final.push_back( temp ) ;
        } // else if

        temp.mTokentype = EMPTY ;
        temp.mContent = "" ;
        encounterTEnd = false ;
        isstring = false ;
      } // if

      gCol++ ;
      now++ ;
    } // while
  } // GetTokenlist()

  void Printtoken( Token tokenlist ) {

    float translate = 0 ;
    int tempint = 0 ;

    if ( tokenlist.mTokentype == FLOAT ) {
      translate = atof( tokenlist.mContent.c_str() ) ;
      printf( "%.3f", translate ) ;
    } // if

    else if ( tokenlist.mTokentype == INT ) {
      for ( int a = 0 ; a < tokenlist.mContent.size() ; a++ ) {
        if ( tokenlist.mContent[a] != '+' )
          cout <<  tokenlist.mContent[a] ;
      } // for
    } // else if

    else if ( tokenlist.mTokentype == STRING ) {
      PrintString( tokenlist ) ;
    } // else if

    else {
      cout << tokenlist.mContent  ;
    } // else


  } // Printtoken()

  void PrintSexep( vector <Token> tokenlist ) {
    float translate = 0 ;
    int tempint = 0 ;
    bool findsymbol = false ;
    for ( int i = 0 ; i < tokenlist.size() ; i++ ) {
      if ( tokenlist[i].mTokentype == FLOAT ) {
        translate = atof( tokenlist[i].mContent.c_str() ) ;
        printf( "> %.3f\n", translate ) ;
      } // if

      else if ( tokenlist[i].mTokentype == INT ) {
        cout << "> " ;
        for ( int a = 0 ; a < tokenlist[i].mContent.size() ; a++ ) {
          if ( tokenlist[i].mContent[a] != '+' )
            cout <<  tokenlist[i].mContent[a] ;
        } // for

        cout << endl ;
      } // else if

      else if ( tokenlist[i].mTokentype == STRING ) {
        cout << "> ";
        PrintString( tokenlist[i] ) ;
        cout << endl ;
      } // else if

      else if ( tokenlist[i].mTokentype == LEFT_PAREN ) {
        if ( i + 1 < tokenlist.size() ) {
          if ( tokenlist[i+1].mTokentype == RIGHT_PAREN ) {
            tokenlist[i].mContent = "nil" ;
            tokenlist[i].mTokentype = NIL ;
            tokenlist.erase( tokenlist.begin() + i + 1 ) ;
          } // if
        } // if
      } // else if

      else if ( tokenlist[i].mTokentype == SYMBOL ) {
        for ( int j = 0 ; j < gdefine.size() ; j++ ) {
          if ( tokenlist[i].mContent == gdefine[j].symbol ) {
            cout  << "> " ;
            string a = "" ; // 用不到
            Printlist( gdefine[j].defineptr, false, a  ) ;
            if ( gfix.size() > 0 ) {
              for ( int a = 0 ; a < gfix.size() ; a++ ) {
                if ( gfix[a]-> mhasRightparen == false )
                  gfix[a]-> mhasRightparen = true ;
              } // for
            } // if

            gfix.clear() ;
            gparennum = 0 ;
            gfirstprint = true ;
            findsymbol = true ;
          } // if
        } // for

        for ( int a = 0 ; a < cmdNum ; a++ ) {
          if ( tokenlist[i].mContent == gInternalCmd[a] ) {
            cout  << "> #<procedure " << gInternalCmd[a] << ">" << endl  ;
            gparennum = 0 ;
            findsymbol = true ;
          } // if
        } // for

        if ( !findsymbol )
          cout <<  "> ERROR (unbound symbol) : " << tokenlist[i].mContent << endl ;
      } // else if



      else {
        cout << "> " << tokenlist[i].mContent << endl ;
      } // else
    } // for


  } // PrintSexep()

  int NumOfdoublequote( string temp ) {
    int i = 0 ;
    int num = 0 ;
    while ( i < temp.size() ) {
      if ( temp[i] == '\"'  ) {
        if ( i > 0 && temp[i-1] == '\\' )
          ;

        else {
          num++ ;
        } // else
      } // if

      i++ ;
    } // while

    return num ;
  } // NumOfdoublequote()

  void PrintString( Token string ) {
    int now = 0 ;
    int maxsize = NumOfdoublequote( string.mContent ) ;
    int nowsize = 0 ;

    while ( now <= string.mContent.size()-1 && nowsize < maxsize ) {
      if ( string.mContent[now] == '\"' ) {
        nowsize++ ;
      } // if

      if ( string.mContent[now] == '\\' ) {
        if ( string.mContent[now+1] == 'n' ) {
          cout << endl ;
          now = now + 1 ;
        } // if

        else if ( string.mContent[now+1] == '\\' ) {
          cout << "\\" ;
          now = now + 1 ;
        } // else if

        else if ( string.mContent[now+1] == 't' ) {
          cout << " " ;
          now = now + 1 ;
        } // else if

        else if ( string.mContent[now+1] == '\"' ) {
          cout << "\"" ;
          now = now + 1 ;
        } // else if

        else {
          cout << string.mContent[now] ;
          now++ ;
          cout << string.mContent[now] ;
        } // else
      } // if

      else if ( now <= string.mContent.size()-1 ) {
        cout << string.mContent[now]  ;
      } // else if

      now++ ;
    } // while

  } // PrintString()

  void Delitokenlist( vector<Token> &tokenlist, Token sexp ) {
    int i = 0 ;
    for ( int a = 0 ; a < tokenlist.size() ; a++ ) {
      if ( sexp.line == tokenlist[a].line )
        i++;
    } // for

    tokenlist.erase( tokenlist.begin(), tokenlist.begin() + i ) ;
  } // Delitokenlist()


  bool IsExit(  vector<Token> tokenlist ) { // ( exit ) or ( exit.nil )
    string left = "1" ;
    string right = "3" ;
    string exit = "2" ;
    string nil = "5" ;
    string dot = "4" ;
    string result = "" ;
    if ( tokenlist.size() >= 3 ) {
      for ( int i = 0 ; i < tokenlist.size() ; i++ ) {
        if ( tokenlist[i].mContent == "exit" )
          result += exit ;
        else if ( tokenlist[i].mContent == ")" )
          result += right ;
        else if ( tokenlist[i].mContent == "(" )
          result += left ;
        else if ( tokenlist[i].mContent == "\n" )
          ;
        else if ( tokenlist[i].mTokentype == NIL )
          result += nil;
        else if ( tokenlist[i].mTokentype == DOT )
          result += dot ;
        else
          result += "6" ;

      } // for

      if ( result == "123" || result == "12453" )
        return true ;
      else
        return false ;
    } // if

    else {
      return false ;
    } // else

  } // IsExit()

  bool Getline( string &str )  {   // eof 在裡面判斷
    str = "" ;
    char a = '\0' ;
    if ( scanf( "%c", &a ) != EOF ) {

      if (   a != '\n' ) {
        str += a ;
        while ( scanf( "%c", &a ) != EOF && a != '\n'  ) {
          str += a ;
        } // while
      } // if

      if ( a != '\n' )
        return true ;

      return false ;
    } // if

    else {
      return true ;
    } // else

  } // Getline()

  void ReadSexp( vector<Token> nowtoken, Listptr  &root, int maxsize, int &now, bool isquote ) {
    bool quoteparent = false ;
    if ( now >= maxsize ) {
      return ;
    } // if

    if ( root == NULL ) {
      root = new List ;
    } // if

    if ( nowtoken[now].mTokentype == LEFT_PAREN || nowtoken[now].mTokentype == QUOTE )  {
      root -> mleft = new List ;
      root -> mleft -> mhasRightparen = true ;

      if ( nowtoken[now].mTokentype == QUOTE ) {
        root -> mleft -> mhasQuote = true ;
        now++ ;
        ReadSexp( nowtoken, root-> mleft, maxsize, now, true ) ;
        quoteparent = true ;
        if ( root -> mhasQuote ) {
          return ;
        } // if
      } // if
      else {
        now++ ;
        ReadSexp( nowtoken, root-> mleft, maxsize, now, false  ) ;
        if ( root -> mhasQuote ) {
          now++ ;
          return ;
        } // if
      } // else

    } // if

    if ( now >= maxsize )
      return ;

    if (  nowtoken[now].mTokentype != LEFT_PAREN && nowtoken[now].mTokentype != QUOTE && !quoteparent   ) {
      if ( nowtoken[now].mTokentype != RIGHT_PAREN &&  nowtoken[now].mTokentype != DOT  ) {
        root -> mtoken.push_back( nowtoken[now] )  ;
        if ( isquote ) {
          now++ ;
          return ;
        } // if

      } // if

      if (  ( now + 1 ) < nowtoken.size() && nowtoken[now+1].mTokentype == DOT  ) {
        if ( nowtoken[now+2].mTokentype == LEFT_PAREN ) {
          now = now + 3 ;
          ReadSexp( nowtoken,  root-> mright, maxsize, now, false  ) ;
        } // if
        else if ( nowtoken[now+2].mTokentype == QUOTE ) {
          nowtoken[now+2].mTokentype = SYMBOL ;
          nowtoken[now+2].mContent = "quote" ;
          now = now + 2 ;
          ReadSexp( nowtoken,  root-> mright, maxsize, now, false  ) ;

        } // else if
        else if ( nowtoken[now+2].mTokentype != LEFT_PAREN ) {
          root -> mtoken.push_back( nowtoken[now+2] )  ;
          now = now + 2 ;
        } // else if


        else {
          now = now + 2 ; // .nil
        } // else
      } // if

    }   // if

    if ( now >= maxsize )
      return ;

    if  (  nowtoken[now].mTokentype != LEFT_PAREN
          && nowtoken[now].mTokentype != QUOTE && !quoteparent  ) {
      now++ ;
    } // if

    if ( now >= maxsize )
      return ;

    if ( nowtoken[now].mTokentype != RIGHT_PAREN    )
      ReadSexp( nowtoken,  root-> mright, maxsize, now, false ) ;

  } // ReadSexp()




  bool SplitisSexepresion( vector<Token> test, int &i  ) { // i從dot放進來
    int numofleft = 0 ;                                  // 檢查錯誤用
    int numofright = 0 ;
    int quote = 0 ;
    int els = 0 ;

    for (  int a = i + 1 ;  a < test.size() ; a++ ) {

      if ( test[a].mTokentype == LEFT_PAREN ) {
        numofleft++;
      } // if

      if ( test[a].mTokentype == RIGHT_PAREN  ) {
        numofright++;
      } // if

      if (  test[a].mTokentype == QUOTE ) {
        quote++ ;
      } // if

      else {
        els++ ;
      } // else

      if ( quote > 0 ) {
        if ( ( numofleft == 0  ||  numofright == 0 ) && els == 0 )  // '
          ;

        else if ( ( numofleft == 0  && numofright == 0 ) && els != 0 ) { // 'Atom
          i = a ;
          return true ;
        } // else if

        else if ( numofleft == numofright ) {  // ' ( sexp )
          i = a ;
          return true  ;
        } // else if
      } // if

      else if  ( numofleft == numofright ) { // ( sexp ) or Atom
        i = a ;
        return true  ;
      } // else if

    } // for

    return false ;
  } // SplitisSexepresion()

  void Buildsexp( vector<Token> &sexp, Token atoken ) {
    sexp.push_back( atoken ) ;
  } // Buildsexp()

  void Printlist(  List * tokenlist, bool samelevel, string &str ) { // 印樹
    if ( tokenlist == NULL   ) {                // str只用在判斷iseql
      return ;
    } // if

    if ( tokenlist->mhasRightparen == true ) {

      if ( !gfirstprint && !samelevel  ) {
        for ( int i = 0 ; i < gparennum ; i++ ) {
          str += "a" ;
          cout << "  "  ;
        } // for
      } // if

      str += "b" ;
      cout << "( " ;

      if ( tokenlist -> mtoken.size() == 0 ) {
        samelevel = true ;
      } // if
      else {
        samelevel = false ;
      } // else


      if ( tokenlist->mhasQuote )  {
        samelevel = false ;
        str += "c" ;
        cout << "quote" << endl  ;
        gfirstprint = false ;
      } // if

      gparennum ++ ;
    }  // if

    Printlist( tokenlist -> mleft, samelevel, str ) ;

    if ( gfirstprint  ) {


      if ( tokenlist -> mtoken.size() == 1  ) {

        if ( tokenlist -> mleft != NULL ) {
          for ( int i = 0 ; i < gparennum ; i++ ) {
            str += "d" ;
            cout << "  " ;
          } // for

          str += "e" ;
          cout << "." << endl ;
        } // if

        str += "f" ;
        Printtoken( tokenlist->mtoken[0] ) ;
        cout << endl ;
      } // if

      if ( tokenlist -> mtoken.size() == 2 ) {
        str += "g" ;
        Printtoken( tokenlist->mtoken[0] ) ;
        cout << endl ;
        if ( tokenlist->mtoken[1].mTokentype != NIL ) {
          for ( int i = 0 ; i < gparennum ; i++ ) {
            str += "h" ;
            cout << "  " ;
          } // for

          str += "i" ;
          cout << "." << endl ;
          for ( int i = 0 ; i < gparennum ; i++ ) {
            str += "j" ;
            cout << "  " ;
          } // for

          str += "k" ;
          Printtoken( tokenlist->mtoken[1] ) ;
          cout << endl ;
        } // if
      } // if

      else {
        ;
      } // else

      gfirstprint = false ;
    } // if

    else {
      if ( tokenlist -> mtoken.size() == 1  ) {
        if ( tokenlist -> mleft != NULL && tokenlist->mtoken[0].mTokentype == NIL )
          ;

        else {
          if ( tokenlist -> mleft != NULL ) {
            for ( int i = 0 ; i < gparennum ; i++ ) {
              str += "l" ;
              cout << "  " ;
            } // for

            str += "m" ;
            cout <<  "." << endl ;
            if ( tokenlist->mhasRightparen == true && tokenlist->mhasQuote == false  ) {
              for ( int i = 0 ; i < gparennum ; i++ ) {
                str += "n" ;
                cout << "  " ;
              } // for
            } // if
          } // if

          if ( tokenlist->mhasRightparen == false || tokenlist->mhasQuote == true ) {
            for ( int i = 0 ; i < gparennum ; i++ ) {
              str += "o" ;
              cout << "  " ;
            } // for

          } // if

          str += "p" ;
          Printtoken( tokenlist->mtoken[0] ) ;
          cout << endl ;
        } // else
      } // if

      if ( tokenlist -> mtoken.size() == 2 ) {
        if ( tokenlist->mhasRightparen == false ) {
          for ( int i = 0 ; i < gparennum ; i++ ) {
            str += "q" ;
            cout << "  " ;
          } // for
        } // if

        str += "r" ;
        Printtoken( tokenlist->mtoken[0] ) ;
        cout << endl ;

        if ( tokenlist->mtoken[1].mTokentype != NIL ) {
          for ( int i = 0 ; i < gparennum ; i++ ) {
            str += "s" ;
            cout << "  " ;
          } // for

          str += "t" ;
          cout << "." << endl ;
          for ( int i = 0 ; i < gparennum ; i++ ) {
            str += "u" ;
            cout << "  " ;
          } // for

          str += "v" ;
          Printtoken( tokenlist->mtoken[1] ) ;
          cout << endl ;
        } // if
      } // if

      else {
        ;
      }  // else
    } // else

    if ( tokenlist->mright != NULL && tokenlist -> mright-> mhasRightparen ) {
      gfixptr = tokenlist->mright ;
      gfix.push_back( gfixptr ) ;
      tokenlist -> mright-> mhasRightparen = false ;
    } // if

    Printlist( tokenlist -> mright, false, str  ) ;

    if ( tokenlist->mhasRightparen == true ) {
      gparennum -- ;
      for ( int i = 0 ; i < gparennum ; i++ ) {
        str += "w" ;
        cout << "  " ;
      } // for

      str += "x" ;
      cout << ")" << endl ;
    }  // if
  } // Printlist()


  void Checkiseql(  List * tokenlist, bool samelevel, string &str ) { // 印樹
    if ( tokenlist == NULL   ) {                // str只用在判斷iseql
      return ;
    } // if

    if ( tokenlist->mhasRightparen == true ) {

      if ( !gfirstprint && !samelevel  ) {
        for ( int i = 0 ; i < gparennum ; i++ ) {
          str += "a" ; // ' '
        } // for
      } // if

      str += "b" ; // b = ( c = )   d= .

      if ( tokenlist -> mtoken.size() == 0 ) {
        samelevel = true ;
      } // if
      else {
        samelevel = false ;
      } // else


      if ( tokenlist->mhasQuote )  {
        samelevel = false ;
        str += "quote" ;
        gfirstprint = false ;
      } // if

      gparennum ++ ;
    }  // if

    Checkiseql( tokenlist -> mleft, samelevel, str ) ;

    if ( gfirstprint  ) {


      if ( tokenlist -> mtoken.size() == 1  ) {

        if ( tokenlist -> mleft != NULL ) {
          for ( int i = 0 ; i < gparennum ; i++ ) {
            str += "a" ;
          } // for

          str += "d" ;
        } // if

        str += tokenlist->mtoken[0].mContent ;
      } // if

      if ( tokenlist -> mtoken.size() == 2 ) {
        str += tokenlist->mtoken[0].mContent ;
        if ( tokenlist->mtoken[1].mTokentype != NIL ) {
          for ( int i = 0 ; i < gparennum ; i++ ) {
            str += "a" ;
          } // for

          str += "d" ;
          for ( int i = 0 ; i < gparennum ; i++ ) {
            str += "a" ;
          } // for

          str += tokenlist->mtoken[1].mContent ;
        } // if
      } // if

      else {
        ;
      } // else

      gfirstprint = false ;
    } // if

    else {
      if ( tokenlist -> mtoken.size() == 1  ) {
        if ( tokenlist -> mleft != NULL && tokenlist->mtoken[0].mTokentype == NIL )
          ;

        else {
          if ( tokenlist -> mleft != NULL ) {
            for ( int i = 0 ; i < gparennum ; i++ ) {
              str += "a" ;
            } // for

            str += "d" ;
            if ( tokenlist->mhasRightparen == true && tokenlist->mhasQuote == false  ) {
              for ( int i = 0 ; i < gparennum ; i++ ) {
                str += "a" ;

              } // for
            } // if
          } // if

          if ( tokenlist->mhasRightparen == false || tokenlist->mhasQuote == true ) {
            for ( int i = 0 ; i < gparennum ; i++ ) {
              str += "a" ;
            } // for

          } // if

          str += tokenlist->mtoken[0].mContent ;

        } // else
      } // if

      if ( tokenlist -> mtoken.size() == 2 ) {
        if ( tokenlist->mhasRightparen == false ) {
          for ( int i = 0 ; i < gparennum ; i++ ) {
            str += "a" ;
          } // for
        } // if

        str += tokenlist->mtoken[0].mContent ;

        if ( tokenlist->mtoken[1].mTokentype != NIL ) {
          for ( int i = 0 ; i < gparennum ; i++ ) {
            str += "a" ;
          } // for

          str += "d" ;
          for ( int i = 0 ; i < gparennum ; i++ ) {
            str += "a" ;
          } // for

          str += tokenlist->mtoken[1].mContent ;
        } // if
      } // if

      else {
        ;
      }  // else
    } // else


    Checkiseql( tokenlist -> mright, false, str  ) ;

    if ( tokenlist->mhasRightparen == true ) {
      gparennum -- ;
      for ( int i = 0 ; i < gparennum ; i++ ) {
        str += "a" ;
      } // for

      str += "c" ;
    }  // if
  } // Checkiseql()


  bool Isevadefinesym( Listptr temp ) { // 檢查是否是被EVA後的DEFINE SYMBOL
    for ( int i = 0 ; i < gdefine.size() ; i++ ) {
      if ( temp->mtoken[0].mContent == gdefine[i].symbol )
        return true ;
    } // for

    return false ;
  } // Isevadefinesym()


  bool IsandGetinternalCmd( List * aeva ) {   // getcommand


    if ( aeva->mhasQuote ) {

      return true ;
    } // if

    if ( aeva->mtoken.size() == 1 ) {
      for ( int a = 0 ; a < gdefine.size() ; a++ ) {
        if ( aeva->mtoken[0].mContent == gdefine[a].symbol ) {
          if ( gdefine[a].iscommand ) {
            aeva->mtoken[0].mContent = gdefine[a].defineptr->mtoken[0].mContent ;
            aeva->mtoken[0].mTokentype = gdefine[a].defineptr->mtoken[0].mTokentype ; // symbol 裡面是command
            return true ;
          } // if
        } // if
      } // for
    } // if


    if ( aeva->mtoken.size() == 1 ) {
      if ( aeva->mtoken[0].mContent == "cons" || aeva->mtoken[0].mContent == "#<procedure cons>"  ) {
        aeva->mtoken[0].mTokentype = CONS ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "list" || aeva->mtoken[0].mContent == "#<procedure list>" ) {
        aeva->mtoken[0].mTokentype = LIST ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "define" || aeva->mtoken[0].mContent == "#<procedure define>" ) {
        aeva->mtoken[0].mTokentype = DEFINE ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "car" || aeva->mtoken[0].mContent == "#<procedure car>" ) {
        aeva->mtoken[0].mTokentype = CAR ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "cdr" || aeva->mtoken[0].mContent == "#<procedure cdr>" ) {
        aeva->mtoken[0].mTokentype = CDR ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "pair?" || aeva->mtoken[0].mContent == "#<procedure pair?>" ) {
        aeva->mtoken[0].mTokentype = ISPAIR ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "list?" || aeva->mtoken[0].mContent == "#<procedure list?>" ) {
        aeva->mtoken[0].mTokentype = ISLIST ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "atom?" || aeva->mtoken[0].mContent == "#<procedure atom?>" ) {
        aeva->mtoken[0].mTokentype = ISATOM ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "null?" || aeva->mtoken[0].mContent == "#<procedure null?>" ) {
        aeva->mtoken[0].mTokentype = ISNULL ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "integer?" || aeva->mtoken[0].mContent == "#<procedure integer?>" ) {
        aeva->mtoken[0].mTokentype = ISINT ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "real?" || aeva->mtoken[0].mContent == "#<procedure real?>" ) {
        aeva->mtoken[0].mTokentype = ISREAL ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "number?" || aeva->mtoken[0].mContent == "#<procedure number?>" ) {
        aeva->mtoken[0].mTokentype = ISNUM ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "string?" || aeva->mtoken[0].mContent == "#<procedure string?>" ) {
        aeva->mtoken[0].mTokentype = ISSTR ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "boolean?" || aeva->mtoken[0].mContent == "#<procedure boolean?>" ) {
        aeva->mtoken[0].mTokentype = ISBOOL ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "symbol?" || aeva->mtoken[0].mContent == "#<procedure symbol?>" ) {
        aeva->mtoken[0].mTokentype = ISSYM ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "+" || aeva->mtoken[0].mContent == "#<procedure +>" ) {
        aeva->mtoken[0].mTokentype = ADD ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "-" || aeva->mtoken[0].mContent == "#<procedure ->" ) {
        aeva->mtoken[0].mTokentype = SUB ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "*" || aeva->mtoken[0].mContent == "#<procedure *>" ) {
        aeva->mtoken[0].mTokentype = MULT ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "/" || aeva->mtoken[0].mContent == "#<procedure />" ) {
        aeva->mtoken[0].mTokentype = DIV ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "not" || aeva->mtoken[0].mContent == "#<procedure not>" ) {
        aeva->mtoken[0].mTokentype = NOT ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "and" || aeva->mtoken[0].mContent == "#<procedure and>" ) {
        aeva->mtoken[0].mTokentype = AND ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "or" || aeva->mtoken[0].mContent == "#<procedure or>" ) {
        aeva->mtoken[0].mTokentype = OR ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == ">" || aeva->mtoken[0].mContent == "#<procedure >>" ) {
        aeva->mtoken[0].mTokentype = BIGG ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == ">=" || aeva->mtoken[0].mContent == "#<procedure >=>" ) {
        aeva->mtoken[0].mTokentype = BIGEQ ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "<" || aeva->mtoken[0].mContent == "#<procedure <>" ) {
        aeva->mtoken[0].mTokentype = SML ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "<=" || aeva->mtoken[0].mContent == "#<procedure <=>" ) {
        aeva->mtoken[0].mTokentype = SMLEQ ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "=" || aeva->mtoken[0].mContent == "#<procedure =>" ) {
        aeva->mtoken[0].mTokentype = EQ ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "string-append"
           || aeva->mtoken[0].mContent == "#<procedure string-append>" ) {
        aeva->mtoken[0].mTokentype = STRAPP ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "string>?" || aeva->mtoken[0].mContent == "#<procedure string>?>" ) {
        aeva->mtoken[0].mTokentype = STRBIG ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "string<?" || aeva->mtoken[0].mContent == "#<procedure string<?>" ) {
        aeva->mtoken[0].mTokentype = STRSML ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "string=?" || aeva->mtoken[0].mContent == "#<procedure string=?>" ) {
        aeva->mtoken[0].mTokentype = STREQL ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "eqv?" || aeva->mtoken[0].mContent == "#<procedure eqv?>" ) {
        aeva->mtoken[0].mTokentype = ISEQV ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "equal?" || aeva->mtoken[0].mContent == "#<procedure equal>" ) {
        aeva->mtoken[0].mTokentype = ISEQL ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "begin" || aeva->mtoken[0].mContent == "#<procedure begin>" ) {
        aeva->mtoken[0].mTokentype = BEGIN ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "if" || aeva->mtoken[0].mContent == "#<procedure if>" ) {
        aeva->mtoken[0].mTokentype = IF ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "cond" || aeva->mtoken[0].mContent == "#<procedure cond>" ) {
        aeva->mtoken[0].mTokentype = COND ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "clean-environment"
           || aeva->mtoken[0].mContent == "#<procedure clean-environment>" ) {
        aeva->mtoken[0].mTokentype = CLEAN ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "string=?" || aeva->mtoken[0].mContent == "#<procedure string=?>" ) {
        aeva->mtoken[0].mTokentype = STREQL ;
        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "quote" || aeva->mtoken[0].mContent == "#<procedure quote>" ) {
        aeva->mtoken[0].mTokentype = QUOTE ;
        aeva->mhasQuote = true ;
        aeva->mtoken[0].mContent = "" ;

        return true ;
      } // if

      if ( aeva->mtoken[0].mContent == "exit" || aeva->mtoken[0].mContent == "#<procedure exit>" ) {
        aeva->mtoken[0].mTokentype = EXIT ;
        return true ;
      } // if

      else {

        return false ;    // unbound symbol
      } // else

    } // if

    else {
      return false ;   // attempt no function
    } // else

  } // IsandGetinternalCmd()

  void Fixarg( vector<Listptr> &arg, bool &error ) {
    int nowarg = 0 ;


    for ( int i = 0 ; i < arg.size() ; i++ ) {
      if ( arg[i]->mtoken.size() > 0 ) {
        for ( int a = 0 ; a < gdefine.size() ; a++ ) {
          if ( !arg[i]->mhasQuote && arg[i]->mtoken[0].mContent == gdefine[a].symbol ) {
            if ( gdefine[a].iscommand && arg[i]->mhasRightparen ) {
              arg[i]->mtoken[0].mContent = gdefine[a].defineptr->mtoken[0].mContent ;
              arg[i]->mtoken[0].mTokentype = gdefine[a].defineptr->mtoken[0].mTokentype ;
            } // if
            else {
              arg[i] = gdefine[a].defineptr ;
              a = gdefine.size() ;
            } // else
          } // if
        } // for
      } // if

      if ( !arg[i]->mhasRightparen && arg[i]->mtoken.size() > 0 ) {
        for ( int j = 0 ; j < cmdNum ; j++ ) {
          if ( arg[i]->mtoken[0].mContent == gInternalCmd[j] ) {
            IsandGetinternalCmd( arg[i] ) ;
            arg[i]->mtoken[0].mContent = "#<procedure " + gInternalCmd[j] + ">" ;
          } // if
        } // for
      } // if
    } // for



    while ( nowarg < arg.size() ) {
      if ( arg[nowarg]->mtoken.size() > 0 && arg[nowarg]->mtoken[0].mTokentype == SYMBOL
           && !arg[nowarg]->mhasRightparen  ) {
        error = true ;
        cout << "\n> ERROR (unbound symbol) : " << arg[nowarg]->mtoken[0].mContent  << endl ;
        return ;
      } // if

      if ( arg[nowarg]->mhasRightparen && !arg[nowarg]->miseva ) {
        if ( arg[nowarg]->mtoken.size() > 0 ) {
          if ( arg[nowarg]->mtoken[0].mContent == "clean-environment"
               || arg[nowarg]->mtoken[0].mContent == "#<procedure clean-environment>" )  {
            cout << "\n> ERROR (level of CLEAN-ENVIRONMENT)\n" ;
            error = true ;
            return ;
          } // if

          if( arg[nowarg]->mtoken[0].mContent == "define"
             || arg[nowarg]->mtoken[0].mContent == "#<procedure define>" ) {
            cout << "\n> ERROR (level of DEFINE)\n" ;
            error = true ;
            return ;
          } // if
          arg[nowarg] = Evaluate( arg[nowarg], error ) ;
          if ( error )
            return  ;

          arg[nowarg]->miseva = true ;
        } // if

        else {
          arg[nowarg] = Evaluate( arg[nowarg], error ) ;
          if ( error )
            return  ;

          arg[nowarg]->miseva = true ;
        } // else
      } // if

      nowarg++ ;
    } // while


  } // Fixarg()

  List * Cons( Listptr  root, bool &error  ) {
    vector<Listptr> arg ;
    Listptr result ;
    Token temptoken ;
    int nowarg = 0 ;
    Splitarg( root, arg ) ;
    string a = "" ;
    if ( arg.size() != 2  ) {
      if ( arg.size() == 1 ) {
        if ( arg[0]->mtoken.size() == 2 ) {
          cout << endl << "> ERROR (non-list) : "  ;
          Printlist( root, false, a ) ;

          error = true ;
          return NULL ;
        } // if
      } // if
      cout << endl << "> ERROR (incorrect number of arguments) : cons" << endl ;
      error = true ;
      return NULL ;
    } // if

    else {
      Fixarg( arg, error ) ; // 把 arg裡的symbol變成正確的
      if ( error )
        return NULL ;
    } // else

    nowarg = 0 ;
    result = new List ;
    result -> mhasRightparen = true ;
    while ( nowarg < arg.size() ) {
      if ( nowarg == 0 ) {
        if ( arg[nowarg]->mhasRightparen )
          result->mleft = arg[nowarg] ;
        else {
          result->mtoken.push_back( arg[nowarg]->mtoken[0] ) ;
        } // else
      } // if

      else if ( nowarg == 1 ) {
        if ( arg[nowarg]->mhasRightparen ) {
          result -> mright = arg[nowarg] ;
        } // if

        else {
          result->mtoken.push_back( arg[nowarg]->mtoken[0] ) ;
        } // else
      } // else if

      nowarg++ ;

    } // while

    if ( result-> mtoken.size() == 2 ) {
      if ( result->mtoken[1].mTokentype == NIL )
        result->mtoken.erase( result->mtoken.begin() + 1 ) ;
    } // if

    return result ;
  } // Cons()


  List * Quote( Listptr root, bool & error ) {
    Listptr result ;
    Token temp ;
    if ( root -> mright != NULL ) {
      error = true ;
      cout << endl << "> ERROR (incorrect number of arguments) : quote" << endl ;
      return NULL;
    } // if

    if ( root -> mleft != NULL ) {
      result = root -> mleft ;
    } // if

    else if ( root->mleft == NULL && root->mtoken.size() == 1 ) {
      result = new List ;
      temp.mContent = root->mtoken[0].mContent ;
      temp.mTokentype = root->mtoken[0].mTokentype ;
      result->mtoken.push_back( temp ) ;
    } // else if

    result->miseva = true ;
    return result ;
  } // Quote()

  void Define( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result ;
    DefineSym tempdefine ;
    tempdefine.iscommand = false ;
    tempdefine.symbol = "" ;
    tempdefine.defineptr = NULL ;
    Splitarg( root, arg ) ;
    if ( arg.size() != 2 ) {
      cout << endl << "ERROR (DEFINE format) : " ;
      string a = "" ; // 用不到
      Printlist( root, false, a ) ;

      error = true ;
      return ;
    } // if

    for ( int i = 0 ; i < cmdNum ; i++ ) {
      if ( arg[0]->mtoken[0].mTokentype != SYMBOL ) {
        cout << endl << "> ERROR (DEFINE format) : " ;
        string a = "" ; // 用不到
        Printlist( root, false, a ) ;

        error = true ;
        return ;
      } // if
      else {
        if ( gInternalCmd[i] == arg[0]->mtoken[0].mContent ) {
          cout << endl << "> ERROR (DEFINE format) : " ;
          string a = "" ; // 用不到
          Printlist( root, false, a ) ;

          error = true ;
          return ;
        } // if
      } // else
    } // for

    for ( int a = 0 ; a < gdefine.size() ; a++ ) {
      if (  arg[1]->mtoken.size() > 0 && arg[1]->mtoken[0].mTokentype == SYMBOL   // 後面那個symbol前面define過
           && arg[1]->mtoken[0].mContent == gdefine[a].symbol ) {
        arg[1] = gdefine[a].defineptr ;
        tempdefine.symbol = arg[0]->mtoken[0].mContent ;

        for ( int c = 0 ; c < gdefine.size() ; c++ ) { // 前面那個symbol redefine
          if ( gdefine[c].symbol == arg[0]->mtoken[0].mContent )
            gdefine.erase( gdefine.begin() + c ) ;
        } // for

        if ( gdefine[a].iscommand )
          tempdefine.iscommand = true  ;
        tempdefine.defineptr = arg[1] ;
        cout << endl << "> " << tempdefine.symbol << " defined" << endl ;
        gdefine.push_back( tempdefine ) ;
        return ;
      } // if
    } // for

    if ( arg[1]->mhasRightparen &&  !arg[1]->miseva ) {
      arg[1] = Evaluate( arg[1], error ) ;
      if ( error )
        return ;
      arg[1]->miseva = true ;
      for ( int a = 0 ; a < gdefine.size() ; a++ ) { // 前面那個symbol redefine
        if ( gdefine[a].symbol == arg[0]->mtoken[0].mContent )
          gdefine.erase( gdefine.begin() + a ) ;
      } // for

      if ( error )
        return ;

      else {
        tempdefine.symbol = arg[0]->mtoken[0].mContent ;
        tempdefine.iscommand = false ;
        tempdefine.defineptr = arg[1] ;
        cout << endl << "> " << tempdefine.symbol << " defined" << endl ;
        gdefine.push_back( tempdefine ) ;
      } // else
    } // if

    else if ( !arg[1]->mhasRightparen ) {
      for ( int a = 0 ; a < gdefine.size() ; a++ ) { // redefine
        if ( gdefine[a].symbol == arg[0]->mtoken[0].mContent )
          gdefine.erase( gdefine.begin() + a ) ;
      } // for

      if (  IsandGetinternalCmd( arg[1] ) ) { // 會把裡面的command 變成type
        tempdefine.symbol = arg[0]->mtoken[0].mContent ;
        tempdefine.iscommand = true ;
        arg[1]->mtoken[0].mContent = "#<procedure " + arg[1]->mtoken[0].mContent + ">" ;
        tempdefine.defineptr = arg[1] ;
        cout << endl << "> " << tempdefine.symbol << " defined" << endl ;
        gdefine.push_back( tempdefine ) ;
      } // if

      else {
        tempdefine.symbol = arg[0]->mtoken[0].mContent ;
        tempdefine.iscommand = false ;
        tempdefine.defineptr = arg[1] ;
        cout << endl << "> " << tempdefine.symbol << " defined" << endl ;
        gdefine.push_back( tempdefine ) ;
      } // else
    } // else if

  } // Define()

  List * Lis( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result ;
    Token temptoken ;
    string a = "a" ;
    int nowarg = 0 ;
    Splitarg( root, arg ) ;
    if (  arg.size() == 0 ) {
      result = new List ;
      temptoken.mContent = "nil" ;
      temptoken.mTokentype = NIL ;
      result->mtoken.push_back( temptoken ) ;
      return result  ;
    } // if

    else
      Fixarg( arg, error ) ;

    if ( error )
      return NULL ;

    nowarg = 0 ;
    result = new List ;
    Listptr temp = result ;
    temp -> mhasRightparen = true ;
    while ( nowarg < arg.size() ) {
      if ( arg[nowarg]-> mtoken.size() == 2 ) {
        if ( arg[nowarg]->mtoken[1].mTokentype == NIL )
          arg[nowarg]->mtoken.erase( arg[nowarg]->mtoken.begin() + 1 ) ;
      } // if

      if ( arg[nowarg]->mhasRightparen ) {
        temp->mleft = arg[nowarg] ;
      } // if

      else {
        temp->mtoken.push_back( arg[nowarg]->mtoken[0] ) ;
      } // else

      temp->mright = new List ;
      temp = temp -> mright ;
      nowarg++ ;

    } // while

    if ( temp->mtoken.size() == 0 )  {
      temp = NULL ;
    } // if

    return result ;
  } // Lis()


  Listptr Div( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result = NULL  ;
    Token temptoken ;
    Listptr temp = NULL ;
    int nowarg = 0 ;
    bool isfloat = false  ;
    int resultint = 0 ;
    float resultfloat = 0 ;
    Splitarg( root, arg ) ;

    if ( arg.size() == 1 || arg.size() == 0  ) {
      cout << endl << "> ERROR (incorrect number of arguments) : /" << endl ;
      error = true ;
      return NULL ;
    } // if

    else {
      Fixarg( arg, error ) ; // 把 arg裡的symbol變成正確的
      if ( error )
        return NULL ;
    } // else

    for ( int i = 0 ; i < arg.size() ; i++ ) {
      if ( arg[i]->mhasRightparen ) {
        cout << endl << "> ERROR (/ with incorrect argument type) : "   ;
        string a = "" ; // 用不到
        Printlist( arg[i], false, a ) ;

        error = true ;
        return NULL ;
      } // if
      else if ( arg[i]->mtoken[0].mTokentype != INT && arg[i]->mtoken[0].mTokentype != FLOAT ) {
        cout << endl << "> ERROR (/ with incorrect argument type) : "   ;
        cout <<  arg[i]->mtoken[0].mContent ;
        cout << endl ;
        error = true ;
        return NULL ;
      } // else if

      else if ( arg[i]->mtoken[0].mTokentype == T || arg[i]->mtoken[0].mTokentype == NIL ) {
        if (  arg[i]->mtoken[0].mTokentype == T ) {
          arg[i]->mtoken[0].mTokentype = INT ;
          arg[i]->mtoken[0].mContent = "1" ;
        } // if

        else {
          arg[i]->mtoken[0].mTokentype = INT ;
          arg[i]->mtoken[0].mContent = "0" ;
        } // else
      } // else if

      else if ( arg[i]->mtoken[0].mTokentype == FLOAT ) {
        isfloat = true ;
      } // else if

      if ( i > 0 && ( arg[i]->mtoken[0].mTokentype == INT  || arg[i]->mtoken[0].mTokentype == FLOAT ) ) {
        if ( arg[i]->mtoken[0].mContent == "0" ) {
          cout << endl << "ERROR (division by zero) : /" << endl ;
          error = true ;
          return NULL ;
        } // if
      } // if
    } // for

    if ( isfloat )
      resultfloat = atof( arg[0]->mtoken[0].mContent.c_str() ) ;
    else
      resultint = atoi( arg[0]->mtoken[0].mContent.c_str() ) ;

    for ( int j = 1 ; j < arg.size() ; j++ ) {
      if ( isfloat ) {
        resultfloat = resultfloat / atof( arg[j]->mtoken[0].mContent.c_str() ) ;
      } // if
      else {
        resultint = resultint / atoi( arg[j]->mtoken[0].mContent.c_str() ) ;
      } // else
    } // for

    stringstream ss ;

    if ( isfloat ) {
      ss << fixed << setprecision( 3 ) << resultfloat ;
      result = new List ;
      temptoken.mContent = ss.str() ;
      temptoken.mTokentype = FLOAT ;
      result->mtoken.push_back( temptoken ) ;
      return result ;
    } // if
    else {
      ss << resultint ;
      result = new List ;
      temptoken.mContent = ss.str() ;
      temptoken.mTokentype = INT ;
      result->mtoken.push_back( temptoken ) ;
      return result ;
    } // else

    error = true ;   // 上面全部沒進?????????????????????
    return  NULL ;
  } // Div()



  Listptr Mult( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result = NULL  ;
    Token temptoken ;
    Listptr temp = NULL ;
    int nowarg = 0 ;
    bool isfloat = false  ;
    int resultint = 1 ;
    float resultfloat = 1 ;
    Splitarg( root, arg ) ;

    if (  arg.size() == 1 || arg.size() == 0   ) {
      cout << endl << "> ERROR (incorrect number of arguments) : *" << endl ;
      error = true ;
      return NULL ;
    } // if

    else {
      Fixarg( arg, error ) ; // 把 arg裡的symbol變成正確的
      if ( error )
        return NULL ;
    } // else

    for ( int i = 0 ; i < arg.size() ; i++ ) {
      if ( arg[i]->mhasRightparen ) {
        cout << endl << "> ERROR (* with incorrect argument type) : "   ;
        string a = "" ; // 用不到
        Printlist( arg[i], false, a ) ;
        error = true ;
        return NULL ;
      } // if
      else if ( arg[i]->mtoken[0].mTokentype != INT && arg[i]->mtoken[0].mTokentype != FLOAT ) {
        cout << endl << "> ERROR (* with incorrect argument type) : "   ;
        cout <<  arg[i]->mtoken[0].mContent ;
        cout << endl ;
        error = true ;
        return NULL ;
      } // else if

      else if ( arg[i]->mtoken[0].mTokentype == T || arg[i]->mtoken[0].mTokentype == NIL ) {
        if (  arg[i]->mtoken[0].mTokentype == T ) {
          arg[i]->mtoken[0].mTokentype = INT ;
          arg[i]->mtoken[0].mContent = "1" ;
        } // if
        else {
          arg[i]->mtoken[0].mTokentype = INT ;
          arg[i]->mtoken[0].mContent = "0" ;
        } // else
      } // else if

      else if ( arg[i]->mtoken[0].mTokentype == FLOAT ) {
        isfloat = true ;
      } // else if
    } // for

    for ( int j = 0 ; j < arg.size() ; j++ ) {
      if ( isfloat ) {
        resultfloat = resultfloat * atof( arg[j]->mtoken[0].mContent.c_str() ) ;
      } // if
      else {
        resultint = resultint * atoi( arg[j]->mtoken[0].mContent.c_str() ) ;
      } // else
    } // for

    stringstream ss ;

    if ( isfloat ) {
      ss << fixed << setprecision( 3 ) << resultfloat ;
      result = new List ;
      temptoken.mContent = ss.str() ;
      temptoken.mTokentype = FLOAT ;
      result->mtoken.push_back( temptoken ) ;
      return result ;
    } // if

    else {
      ss << resultint ;
      result = new List ;
      temptoken.mContent = ss.str() ;
      temptoken.mTokentype = INT ;
      result->mtoken.push_back( temptoken ) ;
      return result ;
    } // else

    error = true ;   // 上面全部沒進?????????????????????
    return  NULL ;
  } // Mult()

  Listptr Sub( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result = NULL  ;
    Token temptoken ;
    Listptr temp = NULL ;
    int nowarg = 0 ;
    bool isfloat = false  ;
    int resultint = 0 ;
    float resultfloat = 0 ;
    Splitarg( root, arg ) ;


    if (  arg.size() == 1 || arg.size() == 0   ) {
      cout << endl << "> ERROR (incorrect number of arguments) : -" << endl ;
      error = true ;
      return NULL ;
    } // if

    else {
      Fixarg( arg, error ) ; // 把 arg裡的symbol變成正確的
      if ( error )
        return NULL ;
    } // else

    for ( int i = 0 ; i < arg.size() ; i++ ) {
      if ( arg[i]->mhasRightparen ) {
        cout << endl << "> ERROR (- with incorrect argument type) : "   ;
        string a = "" ; // 用不到
        Printlist( arg[i], false, a ) ;
        error = true ;
        return NULL ;
      } // if
      else if ( arg[i]->mtoken[0].mTokentype != INT && arg[i]->mtoken[0].mTokentype != FLOAT ) {
        cout << endl << "> ERROR (- with incorrect argument type) : "   ;
        cout <<  arg[i]->mtoken[0].mContent ;
        cout << endl ;
        error = true ;
        return NULL ;
      } // else if

      else if ( arg[i]->mtoken[0].mTokentype == T || arg[i]->mtoken[0].mTokentype == NIL ) {
        if (  arg[i]->mtoken[0].mTokentype == T ) {
          arg[i]->mtoken[0].mTokentype = INT ;
          arg[i]->mtoken[0].mContent = "1" ;
        } // if

        else {
          arg[i]->mtoken[0].mTokentype = INT ;
          arg[i]->mtoken[0].mContent = "0" ;
        } // else

      } // else if

      else if ( arg[i]->mtoken[0].mTokentype == FLOAT ) {
        isfloat = true ;
      } // else if
    } // for

    if ( isfloat )
      resultfloat = atof( arg[0]->mtoken[0].mContent.c_str() ) ;
    else
      resultint = atoi( arg[0]->mtoken[0].mContent.c_str() ) ;

    for ( int j = 1 ; j < arg.size() ; j++ ) {
      if ( isfloat ) {
        resultfloat = resultfloat - atof( arg[j]->mtoken[0].mContent.c_str() ) ;
      } // if

      else {
        resultint = resultint - atoi( arg[j]->mtoken[0].mContent.c_str() ) ;
      } // else

    } // for

    stringstream ss ;

    if ( isfloat ) {
      ss << fixed << setprecision( 3 ) << resultfloat ;
      result = new List ;
      temptoken.mContent = ss.str() ;
      temptoken.mTokentype = FLOAT ;
      result->mtoken.push_back( temptoken ) ;
      return result ;
    } // if

    else {
      ss << resultint ;
      result = new List ;
      temptoken.mContent = ss.str() ;
      temptoken.mTokentype = INT ;
      result->mtoken.push_back( temptoken ) ;
      return result ;
    } // else

    error = true ;   // 上面全部沒進?????????????????????
    return  NULL ;
  } // Sub()

  Listptr Streql( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result = NULL  ;
    Token temptoken ;
    string tempstr ;
    int nowarg = 0 ;
    string resultstr = "" ;
    Splitarg( root, arg ) ;

    if ( arg.size() == 1 || arg.size() == 0  ) {
      cout << endl << "> ERROR (incorrect number of arguments) : string=?" << endl ;
      error = true ;
      return NULL ;
    } // if
    else {
      Fixarg( arg, error ) ; // 把 arg裡的symbol變成正確的
      if ( error ) {
        return NULL ;
      } // if
    } // else

    for ( int i = 0 ; i < arg.size() ; i++ ) {
      if ( arg[i]->mhasRightparen ) {
        cout << endl << "> ERROR (string=? with incorrect argument type) : "   ;
        string a = "" ; // 用不到
        Printlist( arg[i], false, a ) ;

        error = true ;
        return NULL ;
      } // if


      else if ( arg[i]->mtoken[0].mTokentype != STRING ) {
        cout << endl << "> ERROR (string=?  with incorrect argument type) : "   ;
        cout <<  arg[i]->mtoken[0].mContent ;
        cout << endl ;
        error = true ;
        return NULL ;
      } // else if
    } // for

    tempstr = arg[0]->mtoken[0].mContent ;
    for ( int j = 1 ; j < arg.size() ; j++ ) {
      if ( tempstr == arg[j]->mtoken[0].mContent ) {
        tempstr = arg[j]->mtoken[0].mContent ;
      } // if

      else {
        result = new List ;
        temptoken.mContent = "nil" ;
        temptoken.mTokentype = NIL ;
        result->mtoken.push_back( temptoken ) ;
        return result ;
      } // else
    } // for

    result = new List ;
    temptoken.mContent = "#t" ;
    temptoken.mTokentype = T ;
    result->mtoken.push_back( temptoken ) ;
    return result ;

  } // Streql()

  Listptr Strsml( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result = NULL  ;
    Token temptoken ;
    string tempstr ;
    int nowarg = 0 ;
    string resultstr = "" ;
    Splitarg( root, arg ) ;

    if ( arg.size() == 1 || arg.size() == 0  ) {
      cout << endl << "> ERROR (incorrect number of arguments) : string<?" << endl ;
      error = true ;
      return NULL ;
    } // if
    else {
      Fixarg( arg, error ) ; // 把 arg裡的symbol變成正確的
      if ( error ) {
        return NULL ;
      } // if
    } // else

    for ( int i = 0 ; i < arg.size() ; i++ ) {
      if ( arg[i]->mhasRightparen ) {
        cout << endl << "> ERROR (string<? with incorrect argument type) : "   ;
        string a = "" ; // 用不到
        Printlist( arg[i], false, a ) ;
        error = true ;
        return NULL ;
      } // if


      else if ( arg[i]->mtoken[0].mTokentype != STRING ) {
        cout << endl << "> ERROR (string<?  with incorrect argument type) : "   ;
        cout <<  arg[i]->mtoken[0].mContent ;
        cout << endl ;
        error = true ;
        return NULL ;
      } // else if
    } // for

    tempstr = arg[0]->mtoken[0].mContent ;
    for ( int j = 1 ; j < arg.size() ; j++ ) {
      if ( tempstr < arg[j]->mtoken[0].mContent ) {
        tempstr = arg[j]->mtoken[0].mContent ;
      } // if

      else {
        result = new List ;
        temptoken.mContent = "nil" ;
        temptoken.mTokentype = NIL ;
        result->mtoken.push_back( temptoken ) ;
        return result ;
      } // else
    } // for

    result = new List ;
    temptoken.mContent = "#t" ;
    temptoken.mTokentype = T ;
    result->mtoken.push_back( temptoken ) ;
    return result ;

  } // Strsml()

  Listptr Strbig( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result = NULL  ;
    Token temptoken ;
    string tempstr ;
    int nowarg = 0 ;
    string resultstr = "" ;
    Splitarg( root, arg ) ;

    if ( arg.size() == 1 || arg.size() == 0  ) {
      cout << endl << "> ERROR (incorrect number of arguments) : string>?" << endl ;
      error = true ;
      return NULL ;
    } // if
    else {
      Fixarg( arg, error ) ; // 把 arg裡的symbol變成正確的
      if ( error ) {
        return NULL ;
      } // if
    } // else

    for ( int i = 0 ; i < arg.size() ; i++ ) {
      if ( arg[i]->mhasRightparen ) {
        cout << endl << "> ERROR (string>? with incorrect argument type) : "   ;
        string a = "" ; // 用不到
        Printlist( arg[i], false, a ) ;
        error = true ;
        return NULL ;
      } // if


      else if ( arg[i]->mtoken[0].mTokentype != STRING ) {
        cout << endl << "> ERROR (string>?  with incorrect argument type) : "   ;
        cout <<  arg[i]->mtoken[0].mContent ;
        cout << endl ;
        error = true ;
        return NULL ;
      } // else if
    } // for

    tempstr = arg[0]->mtoken[0].mContent ;
    for ( int j = 1 ; j < arg.size() ; j++ ) {
      if ( tempstr > arg[j]->mtoken[0].mContent ) {
        tempstr = arg[j]->mtoken[0].mContent ;
      } // if

      else {
        result = new List ;
        temptoken.mContent = "nil" ;
        temptoken.mTokentype = NIL ;
        result->mtoken.push_back( temptoken ) ;
        return result ;
      } // else
    } // for

    result = new List ;
    temptoken.mContent = "#t" ;
    temptoken.mTokentype = T ;
    result->mtoken.push_back( temptoken ) ;
    return result ;

  } // Strbig()


  Listptr Strapp( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result = NULL  ;
    Token temptoken ;
    string tempstr ;
    int nowarg = 0 ;
    string resultstr = "" ;
    Splitarg( root, arg ) ;

    if ( arg.size() == 1 || arg.size() == 0  ) {
      cout << endl << "> ERROR (incorrect number of arguments) : string-append" << endl ;
      error = true ;
      return NULL ;
    } // if
    else {
      Fixarg( arg, error ) ; // 把 arg裡的symbol變成正確的
      if ( error ) {
        return NULL ;
      } // if
    } // else

    for ( int i = 0 ; i < arg.size() ; i++ ) {
      if ( arg[i]->mhasRightparen ) {
        cout << endl << "> ERROR (string-append with incorrect argument type) : "   ;
        string a = "" ; // 用不到
        Printlist( arg[i], false, a ) ;

        error = true ;
        return NULL ;
      } // if


      else if ( arg[i]->mtoken[0].mTokentype != STRING ) {
        cout << endl << "> ERROR (string-append  with incorrect argument type) : "   ;
        cout <<  arg[i]->mtoken[0].mContent ;
        cout << endl ;
        error = true ;
        return NULL ;
      } // else if
    } // for

    for ( int j = 0 ; j < arg.size() ; j++ ) {
      for ( int a = 0 ; a < arg[j]->mtoken[0].mContent.size() ; a++ ) {
        if ( a != 0  && a != arg[j]->mtoken[0].mContent.size()-1 ) {
          tempstr += arg[j]->mtoken[0].mContent[a];
        } // if
      } // for
    } // for

    resultstr = tempstr ;
    resultstr = "\"" +  resultstr + "\"" ;
    result = new List ;
    temptoken.mContent = resultstr ;
    temptoken.mTokentype = STRING ;
    result->mtoken.push_back( temptoken ) ;
    return result ;

  } // Strapp()


  Listptr Begin( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result = NULL  ;
    Token temptoken ;
    string tempstr ;
    int nowarg = 0 ;
    string resultstr = "" ;
    Splitarg( root, arg ) ;

    if (  arg.size() == 0  ) {
      cout << endl << "> ERROR (incorrect number of arguments) : begin" << endl ;
      error = true ;
      return NULL ;
    } // if
    else {
      Fixarg( arg, error ) ; // 把 arg裡的symbol變成正確的
      if ( error ) {
        return NULL ;
      } // if
    } // else

    nowarg = arg.size() - 1 ;
    result = arg[nowarg] ;
    return result ;

  } // Begin()


  Listptr Add( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result = NULL  ;
    Token temptoken ;
    Listptr temp = NULL ;
    int nowarg = 0 ;
    bool isfloat = false  ;
    int resultint = 0 ;
    float resultfloat = 0 ;
    Splitarg( root, arg ) ;

    if ( arg.size() == 1 || arg.size() == 0  ) {
      cout << endl << "> ERROR (incorrect number of arguments) : +" << endl ;
      error = true ;
      return NULL ;
    } // if

    else {
      Fixarg( arg, error ) ; // 把 arg裡的symbol變成正確的
      if ( error ) {
        return NULL ;
      } // if
    } // else

    for ( int i = 0 ; i < arg.size() ; i++ ) {
      if ( arg[i]->mhasRightparen ) {
        cout << endl << "> ERROR (+ with incorrect argument type) : "   ;
        string a = "" ; // 用不到
        Printlist( arg[i], false, a ) ;

        error = true ;
        return NULL ;
      } // if


      else if ( arg[i]->mtoken[0].mTokentype != INT && arg[i]->mtoken[0].mTokentype != FLOAT ) {
        cout << endl << "> ERROR (+ with incorrect argument type) : "   ;
        cout <<  arg[i]->mtoken[0].mContent ;
        cout << endl ;
        error = true ;
        return NULL ;
      } // else if

      else if ( arg[i]->mtoken[0].mTokentype == T || arg[i]->mtoken[0].mTokentype == NIL ) {
        if (  arg[i]->mtoken[0].mTokentype == T ) {
          arg[i]->mtoken[0].mTokentype = INT ;
          arg[i]->mtoken[0].mContent = "1" ;
        } // if
        else {
          arg[i]->mtoken[0].mTokentype = INT ;
          arg[i]->mtoken[0].mContent = "0" ;
        } // else
      } // else if

      else if ( arg[i]->mtoken[0].mTokentype == FLOAT ) {
        isfloat = true ;
      } // else if
    } // for

    for ( int j = 0 ; j < arg.size() ; j++ ) {
      if ( isfloat ) {
        resultfloat = resultfloat + atof( arg[j]->mtoken[0].mContent.c_str() ) ;
      } // if
      else {
        resultint = resultint + atoi( arg[j]->mtoken[0].mContent.c_str() ) ;
      } // else
    } // for

    stringstream ss ;

    if ( isfloat ) {
      ss << fixed << setprecision( 3 ) << resultfloat ;
      result = new List ;
      temptoken.mContent = ss.str() ;
      temptoken.mTokentype = FLOAT ;
      result->mtoken.push_back( temptoken ) ;
      return result ;
    } // if

    else {
      ss << resultint ;
      result = new List ;
      temptoken.mContent = ss.str() ;
      temptoken.mTokentype = INT ;
      result->mtoken.push_back( temptoken ) ;
      return result ;
    } // else

    error = true ;   // 上面全部沒進?????????????????????
    return  NULL ;
  } // Add()

  Listptr Islist( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result = NULL  ;
    Token temptoken ;
    Listptr temp = NULL ;
    int nowarg = 0 ;
    Splitarg( root, arg ) ;

    if ( arg.size() != 1  ) {
      cout << endl << "> ERROR (incorrect number of arguments) : list?" << endl ;
      error = true ;
      return NULL ;
    } // if

    else {
      Fixarg( arg, error ) ; // 把 arg裡的symbol變成正確的
      if ( error )
        return NULL ;
    } // else

    if (  arg[0]->mhasRightparen  ) {
      for ( temp = arg[0] ; temp != NULL ; temp = temp->mright ) {
        if ( temp->mtoken.size() == 2 ) {
          result = new List ;
          result->mhasRightparen = false ;
          temptoken.mContent = "nil" ;
          temptoken.mTokentype = NIL ;
          result -> mtoken.push_back( temptoken ) ;
          return result ;
        } // if
      } // for

      result = new List ;
      result->mhasRightparen = false ;
      temptoken.mContent = "#t" ;
      temptoken.mTokentype = T ;
      result -> mtoken.push_back( temptoken ) ;
      return result ;
    } // if

    else if (  !arg[0]->mhasRightparen  ) {
      result = new List ;
      result->mhasRightparen = false ;
      temptoken.mContent = "nil" ;
      temptoken.mTokentype = NIL ;
      result -> mtoken.push_back( temptoken ) ;
      return result ;
    } // else if

    return result ;
  } // Islist()


  Listptr Iseql( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result = NULL  ;
    Token temptoken ;
    Listptr temp = NULL ;
    int nowarg = 0 ;
    Splitarg( root, arg ) ;
    string temp1 ;
    string temp2 ;
    if ( arg.size() != 2  ) {
      cout << endl << "> ERROR (incorrect number of arguments) : equal?" << endl ;
      error = true ;
      return NULL ;
    } // if

    else {
      Fixarg( arg, error ) ; // 把 arg裡的symbol變成正確的
      if ( error )
        return NULL ;
    } // else

    Checkiseql( arg[0], false, temp1 ) ;
    gfirstprint = true ;
    gparennum = 0 ;
    Checkiseql( arg[1], false, temp2 ) ;

    if (  temp1 == temp2  ) { // the sAME
      result = new List ;
      result->mhasRightparen = false ;
      temptoken.mContent = "#t" ;
      temptoken.mTokentype = T ;
      result -> mtoken.push_back( temptoken ) ;
      return result ;
    } // if

    result = new List ;
    result->mhasRightparen = false ;
    temptoken.mContent = "nil" ;
    temptoken.mTokentype = NIL ;
    result -> mtoken.push_back( temptoken ) ;
    return result ;

  } // Iseql()

  void Getdefine( Listptr &temp ) {
    for ( int i = 0 ; i < gdefine.size() ; i++ ) {
      if ( temp->mtoken[0].mContent == gdefine[i].symbol ) {
        temp = gdefine[i].defineptr ;
        if (  gdefine[i].iscommand )
          temp->mhasRightparen = true ;
      } // if
    } // for
  } // Getdefine()

  Listptr Iseqv( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result = NULL  ;
    Token temptoken ;
    Listptr temp = NULL ;
    int nowarg = 0 ;
    Splitarg( root, arg ) ;

    if ( arg.size() != 2  ) {
      cout << endl << "> ERROR (incorrect number of arguments) : eqv?" << endl ;
      error = true ;
      return NULL ;
    } // if

    else {
      Fixarg( arg, error ) ; // 把 arg裡的symbol變成正確的
      if ( error )
        return NULL ;
    } // else

    if (  arg[0] == arg[1]  ) { // same memory
      result = new List ;
      result->mhasRightparen = false ;
      temptoken.mContent = "#t" ;
      temptoken.mTokentype = T ;
      result -> mtoken.push_back( temptoken ) ;
      return result ;
    } // if

    else if ( ( !arg[0]->mhasRightparen &&  !arg[1]->mhasRightparen )
              && ( arg[0]->mtoken[0].mTokentype == INT ||  arg[0]->mtoken[0].mTokentype == FLOAT )
              &&  ( arg[1]->mtoken[0].mTokentype == INT ||  arg[1]->mtoken[0].mTokentype == FLOAT ) ) {
      if ( arg[0]->mtoken[0].mContent == arg[1]->mtoken[0].mContent ) {
        result = new List ;
        result->mhasRightparen = false ;
        temptoken.mContent = "#t" ;
        temptoken.mTokentype = T ;
        result -> mtoken.push_back( temptoken ) ;
        return result ;
      } // if
    } // else if

    else if ( ( !arg[0]->mhasRightparen && !arg[1]->mhasRightparen ) &&
              ( arg[0]->mtoken[0].mTokentype == T && arg[1]->mtoken[0].mTokentype == T ) ) {
      result = new List ;
      result->mhasRightparen = false ;
      temptoken.mContent = "#t" ;
      temptoken.mTokentype = T ;
      result -> mtoken.push_back( temptoken ) ;
      return result ;
    } // else if

    else if ( ( !arg[0]->mhasRightparen && !arg[1]->mhasRightparen ) &&
              ( arg[0]->mtoken[0].mTokentype == NIL && arg[1]->mtoken[0].mTokentype == NIL ) ) {
      result = new List ;
      result->mhasRightparen = false ;
      temptoken.mContent = "#t" ;
      temptoken.mTokentype = T ;
      result -> mtoken.push_back( temptoken ) ;
      return result ;
    } // else if


    result = new List ;
    result->mhasRightparen = false ;
    temptoken.mContent = "nil" ;
    temptoken.mTokentype = NIL ;
    result -> mtoken.push_back( temptoken ) ;
    return result;

  } // Iseqv()

  Listptr Cond( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result = NULL  ;
    Token temptoken ;
    Splitarg( root, arg ) ;
    Listptr key ;
    vector<Listptr> tempsmallarg ;
    vector<Listptr> smallarg ;
    string a = "" ;


    if (  arg.size() == 0 ) {
      cout << endl << "> ERROR (incorrect number of arguments) : cond" << endl ;
      error = true ;
      return NULL ;
    } // if

    else {
      for ( int nowarg = 0 ; nowarg < arg.size() ; nowarg++ )  {
        if ( arg[nowarg]->mhasRightparen ) {
          if ( arg[nowarg]->mleft != NULL
               && arg[nowarg]->mleft->mhasRightparen ) { // 表示還有一個運算式
            key = Evaluate( arg[nowarg]->mleft, error ) ;                    // ( cond ( 運算 ) a b c )
            if ( error )
              return NULL ;
            if ( key->mtoken.size() == 1 && key->mtoken[0].mTokentype == NIL && !key->mhasRightparen ) {
              key = NULL ;
            } // if

            else {
              Splitarg( arg[nowarg], tempsmallarg ) ;
              if ( tempsmallarg.size() == 0 ) {
                cout << endl << "> ERROR (COND format) : " ;
                Printlist( root, false, a ) ;

                error = true ;
                return NULL ;
              } // if
              else {

                Fixarg( tempsmallarg, error ) ;
                if ( error )
                  return NULL ;
                result = tempsmallarg[tempsmallarg.size()-1] ;
                return result ;
              } // else
            } // else
          } // if

          else if ( arg[nowarg]->mleft == NULL ) {  // ( else

            if ( nowarg ==  arg.size() - 1  ) {
              Splitarg( arg[nowarg], tempsmallarg ) ;
              if ( tempsmallarg.size() == 0 ) {
                cout << endl << "> ERROR (COND format) : " ;
                Printlist( root, false, a ) ;
                error = true ;
                return NULL ;
              } // if

              if ( arg[nowarg]->mtoken[0].mTokentype != NIL
                   || arg[nowarg]->mtoken[0].mContent == "else" ) {
                Fixarg( tempsmallarg, error ) ;
                if ( error )
                  return NULL ;
                result = tempsmallarg[tempsmallarg.size()-1] ;
                return result ;
              } // if

              else if ( arg[nowarg]->mtoken[0].mTokentype == SYMBOL ) {
                Getdefine( arg[nowarg] ) ;
                if ( arg[nowarg]->mtoken[0].mTokentype != NIL
                     || arg[nowarg]->mtoken[0].mContent == "else" ) {

                  Fixarg( tempsmallarg, error ) ;
                  if ( error )
                    return NULL ;
                  result = tempsmallarg[tempsmallarg.size()-1] ;
                  return result ;
                } // if

                cout << endl ;
                cout << "> " << "ERROR (unbound symbol) : " ;
                cout << arg[nowarg]->mtoken[0].mContent << endl ;
                error = true ;
                return NULL ;
              } // else if
            } // if

            else {
              Splitarg( arg[nowarg], tempsmallarg ) ;
              Getdefine( arg[nowarg] ) ;
              if ( tempsmallarg.size() == 0 ) {
                cout << endl << "> ERROR (COND format) : " ;
                Printlist( root, false, a ) ;
                error = true ;
                return NULL ;
              } // if

              if ( arg[nowarg]->mtoken[0].mTokentype != NIL
                   && arg[nowarg]->mtoken[0].mTokentype != SYMBOL ) {
                Fixarg( tempsmallarg, error ) ;

                if ( error )
                  return NULL ;
                result = tempsmallarg[tempsmallarg.size()-1] ;
                return result ;
              } // if

              else if ( arg[nowarg]->mtoken[0].mTokentype == SYMBOL ) {
                cout << endl ;
                cout << "> " << "ERROR (unbound symbol) : " ;
                cout << arg[nowarg]->mtoken[0].mContent << endl ;
                error = true ;
                return NULL ;
              } // else if
            } // else
          } // else if
        } // if

        else {
          cout << endl << "> ERROR (COND format) : " ;
          Printlist( root, false, a ) ;
          error = true ;
          return NULL ;
        } // else

        tempsmallarg.clear() ;
        key = NULL ;
      } // for
    } // else

    cout << "\n> ERROR (no return value) : " ;
    Printlist( root, false, a ) ;
    result = NULL;
    error = true;
    return NULL ;

  } // Cond()

  Listptr If( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result = NULL  ;
    Token temptoken ;
    int nowarg = 0 ;
    Splitarg( root, arg ) ;
    vector<Listptr> temparg ;
    vector<Listptr> checkarg ;

    if ( error ) {
      return NULL ;
    } // if

    if ( arg.size() != 2  &&  arg.size() != 3 ) {
      cout << endl << "> ERROR (incorrect number of arguments) : if" << endl ;
      error = true ;
      return NULL ;
    } // if

    checkarg.push_back( arg[0] )  ;
    Fixarg( checkarg, error ) ; // 把 arg裡的symbol變成正確的

    if ( !checkarg[0]->mhasRightparen
         && checkarg[0]->mtoken.size() == 1
         &&  checkarg[0]->mtoken[0].mTokentype == NIL ) {
      if ( arg.size() != 3 ) {
        string a = "" ; // 用不到
        cout << endl  << "> ERROR (no return value) : " ;
        Printlist( root, false, a  ) ;
        error = true ;
        return NULL ;
      } // if

      else {
        temparg.push_back( arg[2] ) ;
        Fixarg( temparg, error ) ; // 把 arg裡的symbol變成正確的
        if ( error ) {
          return NULL ;
        } // if

        result = temparg[0] ;
        return result ;
      } // else
    } // if

    else  {
      temparg.push_back( arg[1] ) ;
      Fixarg( temparg, error ) ; // 把 arg裡的symbol變成正確的
      if ( error ) {
        return NULL ;
      } // if

      result = temparg[0] ;
      return result ;
    } // else if

    cout << "fuck you " ;
    error = true ;
    return NULL ;

  } // If()

  Listptr Ispair( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result = NULL  ;
    Token temptoken ;
    int nowarg = 0 ;
    Splitarg( root, arg ) ;


    if ( arg.size() != 1  ) {
      cout << endl << "> ERROR (incorrect number of arguments) : pair?" << endl ;
      error = true ;
      return NULL ;
    } // if

    else {
      Fixarg( arg, error ) ; // 把 arg裡的symbol變成正確的
      if ( error ) {
        return NULL ;
      } // if
    } // else

    if ( arg[0]->mhasRightparen ) {            // 沒有( ( .... ) ) 只有 ( s-exp. sexp) 或 ( sexp ) 一個左括號
      result = new List ;
      result->mhasRightparen = false ;
      temptoken.mContent = "#t" ;
      temptoken.mTokentype = T ;
      result -> mtoken.push_back( temptoken ) ;
      return result ;
    } // if

    else if (  !arg[0]->mhasRightparen  ) {
      result = new List ;
      result->mhasRightparen = false ;
      temptoken.mContent = "nil" ;
      temptoken.mTokentype = NIL ;
      result -> mtoken.push_back( temptoken ) ;
      return result ;
    } // else if


    return result ;

  } // Ispair()

  Listptr Isnull( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result = NULL  ;
    Token temptoken ;
    int nowarg = 0 ;
    Splitarg( root, arg ) ;


    if ( arg.size() != 1  ) {
      cout << endl << "> ERROR (incorrect number of arguments) : null?" << endl ;
      error = true ;
      return NULL ;
    } // if

    else {
      Fixarg( arg, error ) ; // 把 arg裡的symbol變成正確的
      if ( error )
        return NULL ;
    } // else

    if (  arg[0]->mhasRightparen  ) {            // 沒有( ( .... ) ) 只有 ( s-exp. sexp) 或 ( sexp ) 一個左括號
      result = new List ;
      result->mhasRightparen = false ;
      temptoken.mContent = "nil" ;
      temptoken.mTokentype = NIL ;
      result -> mtoken.push_back( temptoken ) ;
      return result ;
    } // if

    else if (  !arg[0]->mhasRightparen  ) {
      if ( arg[0]->mtoken[0].mTokentype == NIL ) {
        result = new List ;
        result->mhasRightparen = false ;
        temptoken.mContent = "#t" ;
        temptoken.mTokentype = T ;
        result -> mtoken.push_back( temptoken ) ;
        return result ;
      } // if

      else {
        result = new List ;
        result->mhasRightparen = false ;
        temptoken.mContent = "nil" ;
        temptoken.mTokentype = NIL ;
        result -> mtoken.push_back( temptoken ) ;
        return result ;
      } // else

    } // else if

    return result ;
  } // Isnull()


  Listptr Issymbol( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result = NULL  ;
    Token temptoken ;
    int nowarg = 0 ;
    Splitarg( root, arg ) ;


    if ( arg.size() != 1  ) {
      cout << endl << "> ERROR (incorrect number of arguments) : symbol?" << endl ;
      error = true ;
      return NULL ;
    } // if

    else {
      Fixarg( arg, error ) ; // 把 arg裡的symbol變成正確的
      if ( error )
        return NULL ;
    } // else

    if (  arg[0]->mhasRightparen  ) {            // 沒有( ( .... ) ) 只有 ( s-exp. sexp) 或 ( sexp ) 一個左括號
      result = new List ;
      result->mhasRightparen = false ;
      temptoken.mContent = "nil" ;
      temptoken.mTokentype = NIL ;
      result -> mtoken.push_back( temptoken ) ;
      return result ;
    } // if


    else if (  !arg[0]->mhasRightparen  ) {
      if ( arg[0]->mtoken[0].mTokentype == SYMBOL ) {
        result = new List ;
        result->mhasRightparen = false ;
        temptoken.mContent = "#t" ;
        temptoken.mTokentype = T ;
        result -> mtoken.push_back( temptoken ) ;
        return result ;
      } // if

      else {
        result = new List ;
        result->mhasRightparen = false ;
        temptoken.mContent = "nil" ;
        temptoken.mTokentype = NIL ;
        result -> mtoken.push_back( temptoken ) ;
        return result ;
      } // else

    } // else if

    return result ;

  } // Issymbol()


  Listptr Isstring( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result = NULL  ;
    Token temptoken ;
    int nowarg = 0 ;
    Splitarg( root, arg ) ;


    if ( arg.size() != 1  ) {
      cout << endl << "> ERROR (incorrect number of arguments) : string?" << endl ;
      error = true ;
      return NULL ;
    } // if

    else {
      Fixarg( arg, error ) ; // 把 arg裡的symbol變成正確的
      if ( error )
        return NULL ;
    } // else

    if (  arg[0]->mhasRightparen  ) {            // 沒有( ( .... ) ) 只有 ( s-exp. sexp) 或 ( sexp ) 一個左括號
      result = new List ;
      result->mhasRightparen = false ;
      temptoken.mContent = "nil" ;
      temptoken.mTokentype = NIL ;
      result -> mtoken.push_back( temptoken ) ;
      return result ;
    } // if


    else if (  !arg[0]->mhasRightparen  ) {
      if ( arg[0]->mtoken[0].mTokentype == STRING ) {
        result = new List ;
        result->mhasRightparen = false ;
        temptoken.mContent = "#t" ;
        temptoken.mTokentype = T ;
        result -> mtoken.push_back( temptoken ) ;
        return result ;
      } // if

      else {
        result = new List ;
        result->mhasRightparen = false ;
        temptoken.mContent = "nil" ;
        temptoken.mTokentype = NIL ;
        result -> mtoken.push_back( temptoken ) ;
        return result ;
      } // else

    } // else if

    return result ;
  } // Isstring()

  Listptr Isbool( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result = NULL  ;
    Token temptoken ;
    int nowarg = 0 ;
    Splitarg( root, arg ) ;


    if ( arg.size() != 1  ) {
      cout << endl << "> ERROR (incorrect number of arguments) : bool?" << endl ;
      error = true ;
      return NULL ;
    } // if

    else {
      Fixarg( arg, error ) ; // 把 arg裡的symbol變成正確的
      if ( error )
        return NULL ;
    } // else

    if (  arg[0]->mhasRightparen  ) {            // 沒有( ( .... ) ) 只有 ( s-exp. sexp) 或 ( sexp ) 一個左括號
      result = new List ;
      result->mhasRightparen = false ;
      temptoken.mContent = "nil" ;
      temptoken.mTokentype = NIL ;
      result -> mtoken.push_back( temptoken ) ;
      return result ;
    } // if


    else if (  !arg[0]->mhasRightparen  ) {
      if ( arg[0]->mtoken[0].mTokentype == NIL || arg[0]->mtoken[0].mTokentype == T ) {
        result = new List ;
        result->mhasRightparen = false ;
        temptoken.mContent = "#t" ;
        temptoken.mTokentype = T ;
        result -> mtoken.push_back( temptoken ) ;
        return result ;
      } // if

      else {
        result = new List ;
        result->mhasRightparen = false ;
        temptoken.mContent = "nil" ;
        temptoken.mTokentype = NIL ;
        result -> mtoken.push_back( temptoken ) ;
        return result ;
      } // else
    } // else if

    return result ;

  } // Isbool()

  Listptr Isinteger( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result = NULL  ;
    Token temptoken ;
    int nowarg = 0 ;
    Splitarg( root, arg ) ;


    if ( arg.size() != 1  ) {
      cout << endl << "> ERROR (incorrect number of arguments) : integer?" << endl ;
      error = true ;
      return NULL ;
    } // if

    else {
      Fixarg( arg, error ) ; // 把 arg裡的symbol變成正確的
      if ( error )
        return NULL ;
    } // else

    if (  arg[0]->mhasRightparen  ) {            // 沒有( ( .... ) ) 只有 ( s-exp. sexp) 或 ( sexp ) 一個左括號
      result = new List ;
      result->mhasRightparen = false ;
      temptoken.mContent = "nil" ;
      temptoken.mTokentype = NIL ;
      result -> mtoken.push_back( temptoken ) ;
      return result ;
    } // if


    else if (  !arg[0]->mhasRightparen  ) {
      if ( arg[0]->mtoken[0].mTokentype == INT ) {
        result = new List ;
        result->mhasRightparen = false ;
        temptoken.mContent = "#t" ;
        temptoken.mTokentype = T ;
        result -> mtoken.push_back( temptoken ) ;
        return result ;
      } // if

      else {
        result = new List ;
        result->mhasRightparen = false ;
        temptoken.mContent = "nil" ;
        temptoken.mTokentype = NIL ;
        result -> mtoken.push_back( temptoken ) ;
        return result ;
      } // else

    } // else if

    return result ;
  } // Isinteger()

  Listptr Isnumber( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result = NULL  ;
    Token temptoken ;
    int nowarg = 0 ;
    Splitarg( root, arg ) ;


    if ( arg.size() != 1  ) {
      cout << endl << "> ERROR (incorrect number of arguments) : number?" << endl ;
      error = true ;
      return NULL ;
    } // if

    else {
      Fixarg( arg, error ) ; // 把 arg裡的symbol變成正確的
      if ( error )
        return NULL ;
    } // else

    if (  arg[0]->mhasRightparen  ) {            //
      result = new List ;
      result->mhasRightparen = false ;
      temptoken.mContent = "nil" ;
      temptoken.mTokentype = NIL ;
      result -> mtoken.push_back( temptoken ) ;
      return result ;
    } // if


    else if (  !arg[0]->mhasRightparen  ) {
      if ( arg[0]->mtoken[0].mTokentype == INT || arg[0]->mtoken[0].mTokentype == FLOAT  ) {
        result = new List ;
        result->mhasRightparen = false ;
        temptoken.mContent = "#t" ;
        temptoken.mTokentype = T ;
        result -> mtoken.push_back( temptoken ) ;
        return result ;
      } // if

      else {
        result = new List ;
        result->mhasRightparen = false ;
        temptoken.mContent = "nil" ;
        temptoken.mTokentype = NIL ;
        result -> mtoken.push_back( temptoken ) ;
        return result ;
      } // else

    } // else if

    return result ;

  } // Isnumber()

  Listptr Isreal( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result = NULL  ;
    Token temptoken ;
    int nowarg = 0 ;
    Splitarg( root, arg ) ;


    if ( arg.size() != 1  ) {
      cout << endl << "> ERROR (incorrect number of arguments) : real?" << endl ;
      error = true ;
      return NULL ;
    } // if

    else {
      Fixarg( arg, error ) ; // 把 arg裡的symbol變成正確的
      if ( error )
        return NULL ;
    } // else

    if (  arg[0]->mhasRightparen  ) {            // 沒有( ( .... ) ) 只有 ( s-exp. sexp) 或 ( sexp ) 一個左括號
      result = new List ;
      result->mhasRightparen = false ;
      temptoken.mContent = "nil" ;
      temptoken.mTokentype = NIL ;
      result -> mtoken.push_back( temptoken ) ;
      return result ;
    } // if

    else if (  !arg[0]->mhasRightparen  ) {
      if ( arg[0]->mtoken[0].mTokentype == INT || arg[0]->mtoken[0].mTokentype == FLOAT  ) {
        result = new List ;
        result->mhasRightparen = false ;
        temptoken.mContent = "#t" ;
        temptoken.mTokentype = T ;
        result -> mtoken.push_back( temptoken ) ;
        return result ;
      } // if

      else {
        result = new List ;
        result->mhasRightparen = false ;
        temptoken.mContent = "nil" ;
        temptoken.mTokentype = NIL ;
        result -> mtoken.push_back( temptoken ) ;
        return result ;
      } // else

    } // else if

    return result ;
  } // Isreal()

  Listptr Isatom( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result = NULL  ;
    Token temptoken ;
    int nowarg = 0 ;
    Splitarg( root, arg ) ;


    if ( arg.size() != 1  ) {
      cout << endl << "> ERROR (incorrect number of arguments) : atom?" << endl ;
      error = true ;
      return NULL ;
    } // if

    else {
      Fixarg( arg, error ) ; // 把 arg裡的symbol變成正確的
      if ( error )
        return NULL ;
    } // else

    if (  arg[0]->mhasRightparen  ) {            // 沒有( ( .... ) ) 只有 ( s-exp. sexp) 或 ( sexp ) 一個左括號
      result = new List ;
      result->mhasRightparen = false ;
      temptoken.mContent = "nil" ;
      temptoken.mTokentype = NIL ;
      result -> mtoken.push_back( temptoken ) ;
      return result ;
    } // if

    else if (  !arg[0]->mhasRightparen  ) {
      result = new List ;
      result->mhasRightparen = false ;
      temptoken.mContent = "#t" ;
      temptoken.mTokentype = T ;
      result -> mtoken.push_back( temptoken ) ;
      return result ;
    } // else if

    return result ;

  } // Isatom()


  Listptr Clean( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result = NULL  ;
    Token temptoken ;
    int nowarg = 0 ;
    Splitarg( root, arg ) ;
    if ( arg.size() == 0 ) {
      cout << endl << "> environment cleaned" << endl ;
      gdefine.clear() ;
      error = true ;
      return NULL ;
    } // if

    else {
      cout << endl << "> ERROR (incorrect number of arguments) : clean-environment" << endl ;
      error = true ;
      return NULL ;
    } // else

  } // Clean()

  Listptr Car( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result = NULL  ;
    Token temptoken ;
    int nowarg = 0 ;
    Splitarg( root, arg ) ;

    if ( arg.size() != 1  ) {
      cout << endl << "> ERROR (incorrect number of arguments) : car" << endl ;
      error = true ;
      return NULL ;
    } // if

    else {
      Fixarg( arg, error ) ; // 把 arg裡的symbol變成正確的
      if ( error )
        return NULL ;
    } // else

    if ( arg[0]->mleft == NULL && arg[0]->mhasRightparen  ) {  // 沒有( ( .... ) )
      result = new List ;               // 只有 ( s-exp. sexp) 或 ( sexp ) 一個左括號
      result->mhasRightparen = false ;
      result -> mtoken.push_back( arg[0]->mtoken[0] ) ;
      return result ;
    } // if

    else if (  arg[0]-> mleft != NULL ) {
      result = arg[0] -> mleft ;
      return result;
    } // else if

    if ( result == NULL ) {
      if (  !arg[0]->mhasRightparen && arg[0]->mtoken.size() == 1   ) {
        cout << endl << "> ERROR (car with incorrect argument type) : "  ;
        cout << arg[0]->mtoken[0].mContent << endl ;
        error = true ;
        return NULL ;
      } // if

      else if (  !arg[0]->mhasRightparen && arg[0]->mtoken.size() == 2 )  {  // non -list
        cout << endl << "> ERROR (non-list) : "  ;
        string a = "" ; // 用不到
        Printlist( root, false, a ) ;
        error = true ;
        return NULL ;
      } // else if
    } // if

    return result ;

  } // Car()

  Listptr Or( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result = NULL  ;
    Token temptoken ;
    Splitarg( root, arg ) ;


    if (  arg.size() == 1 || arg.size() == 0   ) {
      cout << endl << "> ERROR (incorrect number of arguments) : or" << endl ;
      error = true ;
      return NULL ;
    } // if

    else {
      Fixarg( arg, error ) ; // 把 arg裡的symbol變成正確的
      if ( error )
        return NULL ;
    } // else

    for ( int nowarg = 0 ; nowarg < arg.size() ; nowarg++ ) {
      if (  arg[nowarg]->mhasRightparen || arg[nowarg]->mtoken[0].mTokentype != NIL ) {  // 沒有( ( .... ) )
        result = arg[nowarg] ;
        return result ;
      } // if

      if ( nowarg == arg.size() - 1 ) {  // 最後一個了
        result = arg[nowarg] ;
        return result ;
      } // if
    } // for

    error = true ;
    return result ;
  } // Or()

  Listptr Bigeq( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result = NULL  ;
    Token temptoken ;
    Listptr temp = NULL ;
    int nowarg = 0 ;
    bool isfloat = false  ;
    int resultint = 0 ;
    float resultfloat = 0 ;
    Splitarg( root, arg ) ;

    if ( arg.size() == 1 || arg.size() == 0  ) {
      cout << endl << "> ERROR (incorrect number of arguments) : >=" << endl ;
      error = true ;
      return NULL ;
    } // if
    else {
      Fixarg( arg, error ) ; // 把 arg裡的symbol變成正確的
      if ( error ) {
        return NULL ;
      } // if
    } // else

    for ( int i = 0 ; i < arg.size() ; i++ ) {
      if ( arg[i]->mhasRightparen ) {
        cout << endl << "> ERROR (>= with incorrect argument type) : "   ;
        string a = "" ; // 用不到
        Printlist( arg[i], false, a ) ;
        error = true ;
        return NULL ;
      } // if

      else if ( arg[i]->mtoken[0].mTokentype != INT && arg[i]->mtoken[0].mTokentype != FLOAT ) {
        cout << endl << "> ERROR (>= with incorrect argument type) : "   ;
        cout <<  arg[i]->mtoken[0].mContent ;
        cout << endl ;
        error = true ;
        return NULL ;
      } // else if


      else if ( arg[i]->mtoken[0].mTokentype == FLOAT ) {
        isfloat = true ;
      } // else if
    } // for

    if ( isfloat ) {
      resultfloat = atof( arg[0]->mtoken[0].mContent.c_str() ) ;
    } // if

    else {
      resultint =  atoi( arg[0]->mtoken[0].mContent.c_str() ) ;
    } // else

    for ( int j = 1 ; j < arg.size() ; j++ ) {
      if ( isfloat ) {
        if ( resultfloat >= atof( arg[j]->mtoken[0].mContent.c_str() ) ) {
          resultfloat = atof( arg[j]->mtoken[0].mContent.c_str() ) ;
        } // if
        else {
          result = new List ;
          temptoken.mContent = "nil" ;
          temptoken.mTokentype = NIL ;
          result->mtoken.push_back( temptoken ) ;
          return result ;
        } // else
      } // if

      else {
        if ( resultint >= atoi( arg[j]->mtoken[0].mContent.c_str() ) ) {
          resultint = atoi( arg[j]->mtoken[0].mContent.c_str() ) ;
        } // if
        else {
          result = new List ;
          temptoken.mContent = "nil" ;
          temptoken.mTokentype = NIL ;
          result->mtoken.push_back( temptoken ) ;
          return result ;
        } // else
      } // else
    } // for

    result = new List ;
    temptoken.mContent = "#t" ;
    temptoken.mTokentype = T ;
    result->mtoken.push_back( temptoken ) ;
    return result ;
  } // Bigeq()

  Listptr Sml( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result = NULL  ;
    Token temptoken ;
    Listptr temp = NULL ;
    int nowarg = 0 ;
    bool isfloat = false  ;
    int resultint = 0 ;
    float resultfloat = 0 ;
    Splitarg( root, arg ) ;

    if ( arg.size() == 1 || arg.size() == 0  ) {
      cout << endl << "> ERROR (incorrect number of arguments) : <" << endl ;
      error = true ;
      return NULL ;
    } // if
    else {
      Fixarg( arg, error ) ; // 把 arg裡的symbol變成正確的
      if ( error ) {
        return NULL ;
      } // if
    } // else

    for ( int i = 0 ; i < arg.size() ; i++ ) {
      if ( arg[i]->mhasRightparen ) {
        cout << endl << "< ERROR (< with incorrect argument type) : "   ;
        string a = "" ; // 用不到
        Printlist( arg[i], false, a ) ;
        error = true ;
        return NULL ;
      } // if

      else if ( arg[i]->mtoken[0].mTokentype != INT && arg[i]->mtoken[0].mTokentype != FLOAT ) {
        cout << endl << "< ERROR (< with incorrect argument type) : "   ;
        cout <<  arg[i]->mtoken[0].mContent ;
        cout << endl ;
        error = true ;
        return NULL ;
      } // else if


      else if ( arg[i]->mtoken[0].mTokentype == FLOAT ) {
        isfloat = true ;
      } // else if
    } // for

    if ( isfloat ) {
      resultfloat = atof( arg[0]->mtoken[0].mContent.c_str() ) ;
    } // if

    else {
      resultint =  atoi( arg[0]->mtoken[0].mContent.c_str() ) ;
    } // else

    for ( int j = 1 ; j < arg.size() ; j++ ) {
      if ( isfloat ) {
        if ( resultfloat < atof( arg[j]->mtoken[0].mContent.c_str() ) ) {
          resultfloat = atof( arg[j]->mtoken[0].mContent.c_str() ) ;
        } // if
        else {
          result = new List ;
          temptoken.mContent = "nil" ;
          temptoken.mTokentype = NIL ;
          result->mtoken.push_back( temptoken ) ;
          return result ;
        } // else
      } // if

      else {
        if ( resultint < atoi( arg[j]->mtoken[0].mContent.c_str() ) ) {
          resultint = atoi( arg[j]->mtoken[0].mContent.c_str() ) ;
        } // if
        else {
          result = new List ;
          temptoken.mContent = "nil" ;
          temptoken.mTokentype = NIL ;
          result->mtoken.push_back( temptoken ) ;
          return result ;
        } // else
      } // else
    } // for

    result = new List ;
    temptoken.mContent = "#t" ;
    temptoken.mTokentype = T ;
    result->mtoken.push_back( temptoken ) ;
    return result ;
  } // Sml()

  Listptr Smleq( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result = NULL  ;
    Token temptoken ;
    Listptr temp = NULL ;
    int nowarg = 0 ;
    bool isfloat = false  ;
    int resultint = 0 ;
    float resultfloat = 0 ;
    Splitarg( root, arg ) ;

    if ( arg.size() == 1 || arg.size() == 0  ) {
      cout << endl << "> ERROR (incorrect number of arguments) : <=" << endl ;
      error = true ;
      return NULL ;
    } // if
    else {
      Fixarg( arg, error ) ; // 把 arg裡的symbol變成正確的
      if ( error ) {
        return NULL ;
      } // if
    } // else

    for ( int i = 0 ; i < arg.size() ; i++ ) {
      if ( arg[i]->mhasRightparen ) {
        cout << endl << "> ERROR (<= with incorrect argument type) : "   ;
        string a = "" ; // 用不到
        Printlist( arg[i], false, a ) ;

        error = true ;
        return NULL ;
      } // if

      else if ( arg[i]->mtoken[0].mTokentype != INT && arg[i]->mtoken[0].mTokentype != FLOAT ) {
        cout << endl << "> ERROR (<= with incorrect argument type) : "   ;
        cout <<  arg[i]->mtoken[0].mContent ;
        cout << endl ;
        error = true ;
        return NULL ;
      } // else if


      else if ( arg[i]->mtoken[0].mTokentype == FLOAT ) {
        isfloat = true ;
      } // else if
    } // for

    if ( isfloat ) {
      resultfloat = atof( arg[0]->mtoken[0].mContent.c_str() ) ;
    } // if

    else {
      resultint =  atoi( arg[0]->mtoken[0].mContent.c_str() ) ;
    } // else

    for ( int j = 1 ; j < arg.size() ; j++ ) {
      if ( isfloat ) {
        if ( resultfloat <= atof( arg[j]->mtoken[0].mContent.c_str() ) ) {
          resultfloat = atof( arg[j]->mtoken[0].mContent.c_str() ) ;
        } // if

        else {
          result = new List ;
          temptoken.mContent = "nil" ;
          temptoken.mTokentype = NIL ;
          result->mtoken.push_back( temptoken ) ;
          return result ;
        } // else
      } // if

      else {
        if ( resultint <= atoi( arg[j]->mtoken[0].mContent.c_str() ) ) {
          resultint = atoi( arg[j]->mtoken[0].mContent.c_str() ) ;
        } // if
        else {
          result = new List ;
          temptoken.mContent = "nil" ;
          temptoken.mTokentype = NIL ;
          result->mtoken.push_back( temptoken ) ;
          return result ;
        } // else
      } // else
    } // for

    result = new List ;
    temptoken.mContent = "#t" ;
    temptoken.mTokentype = T ;
    result->mtoken.push_back( temptoken ) ;
    return result ;
  } // Smleq()

  Listptr Bigg( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result = NULL  ;
    Token temptoken ;
    Listptr temp = NULL ;
    int nowarg = 0 ;
    bool isfloat = false  ;
    int resultint = 0 ;
    float resultfloat = 0 ;
    Splitarg( root, arg ) ;

    if ( arg.size() == 1 || arg.size() == 0  ) {
      cout << endl << "> ERROR (incorrect number of arguments) : >" << endl ;
      error = true ;
      return NULL ;
    } // if
    else {
      Fixarg( arg, error ) ; // 把 arg裡的symbol變成正確的
      if ( error ) {
        return NULL ;
      } // if
    } // else

    for ( int i = 0 ; i < arg.size() ; i++ ) {
      if ( arg[i]->mhasRightparen ) {
        cout << endl << "> ERROR (> with incorrect argument type) : "   ;
        string a = "" ; // 用不到
        Printlist( arg[i], false, a ) ;
        error = true ;
        return NULL ;
      } // if

      else if ( arg[i]->mtoken[0].mTokentype != INT && arg[i]->mtoken[0].mTokentype != FLOAT ) {
        cout << endl << "> ERROR (> with incorrect argument type) : "   ;
        cout <<  arg[i]->mtoken[0].mContent ;
        cout << endl ;
        error = true ;
        return NULL ;
      } // else if


      else if ( arg[i]->mtoken[0].mTokentype == FLOAT ) {
        isfloat = true ;
      } // else if
    } // for

    if ( isfloat ) {
      resultfloat = atof( arg[0]->mtoken[0].mContent.c_str() ) ;
    } // if

    else {
      resultint =  atoi( arg[0]->mtoken[0].mContent.c_str() ) ;
    } // else

    for ( int j = 1 ; j < arg.size() ; j++ ) {
      if ( isfloat ) {
        if ( resultfloat > atof( arg[j]->mtoken[0].mContent.c_str() ) ) {
          resultfloat = atof( arg[j]->mtoken[0].mContent.c_str() ) ;
        } // if
        else {
          result = new List ;
          temptoken.mContent = "nil" ;
          temptoken.mTokentype = NIL ;
          result->mtoken.push_back( temptoken ) ;
          return result ;
        } // else
      } // if

      else {
        if ( resultint > atoi( arg[j]->mtoken[0].mContent.c_str() ) ) {
          resultint = atoi( arg[j]->mtoken[0].mContent.c_str() ) ;
        } // if
        else {
          result = new List ;
          temptoken.mContent = "nil" ;
          temptoken.mTokentype = NIL ;
          result->mtoken.push_back( temptoken ) ;
          return result ;
        } // else
      } // else
    } // for

    result = new List ;
    temptoken.mContent = "#t" ;
    temptoken.mTokentype = T ;
    result->mtoken.push_back( temptoken ) ;
    return result ;
  } // Bigg()


  Listptr And( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result = NULL  ;
    Token temptoken ;
    int nowarg = 0 ;
    Splitarg( root, arg ) ;


    if ( arg.size() == 1 || arg.size() == 0  ) {
      cout << endl << "> ERROR (incorrect number of arguments) : or" << endl ;
      error = true ;
      return NULL ;
    } // if

    else {
      Fixarg( arg, error ) ; // 把 arg裡的symbol變成正確的
      if ( error )
        return NULL ;
    } // else

    for ( int nowarg = 0 ; nowarg < arg.size() ; nowarg++ ) {
      if (  !arg[nowarg]->mhasRightparen &&  arg[nowarg]->mtoken[0].mTokentype == NIL ) {  // 沒有( ( .... ) )
        result = new List ;
        result->mhasRightparen = false ;
        temptoken.mContent = "nil" ;
        temptoken.mTokentype = NIL ;
        result -> mtoken.push_back( temptoken ) ;
        return result ;
      } // if

      if ( nowarg == arg.size() - 1 ) {  // 最後一個了
        result = arg[nowarg] ;
        return result ;
      } // if
    } // for

    error = true ;
    return result ;
  } // And()


  Listptr Not( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result = NULL  ;
    Token temptoken ;
    int nowarg = 0 ;
    Splitarg( root, arg ) ;


    if ( arg.size() != 1  ) {
      cout << endl << "> ERROR (incorrect number of arguments) : not" << endl ;
      error = true ;
      return NULL ;
    } // if

    else {
      Fixarg( arg, error ) ; // 把 arg裡的symbol變成正確的
      if ( error )
        return NULL ;
    } // else

    if (  arg[0]->mhasRightparen  ) {  // 沒有( ( .... ) )
      result = new List ;
      result->mhasRightparen = false ;
      temptoken.mContent = "nil" ;
      temptoken.mTokentype = NIL ;
      result -> mtoken.push_back( temptoken ) ;
      return result ;
    } // if

    else if ( arg[0]->mtoken[0].mTokentype != NIL ) {
      result = new List ;
      result->mhasRightparen = false ;
      temptoken.mContent = "nil" ;
      temptoken.mTokentype = NIL ;
      result -> mtoken.push_back( temptoken ) ;
      return result ;
    } // else if

    else {
      result = new List ;
      result->mhasRightparen = false ;
      temptoken.mContent = "#t" ;
      temptoken.mTokentype = T ;
      result -> mtoken.push_back( temptoken ) ;
      return result ;
    } // else

    return result ;
  } // Not()


  Listptr Cdr( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result = NULL  ;
    Token temptoken ;
    int nowarg = 0 ;
    Splitarg( root, arg ) ;


    if ( arg.size() != 1  ) {
      cout << endl << "> ERROR (incorrect number of arguments) : cdr" << endl ;
      error = true ;
      return NULL ;
    } // if

    else {
      Fixarg( arg, error ) ; // 把 arg裡的symbol變成正確的
      if ( error )
        return NULL ;
    } // else

    if ( arg[0]->mright != NULL && arg[0]->mhasRightparen  ) { // 右 邊不為null ( sexp sexp )
      result = arg[0] -> mright ;
      result->mhasRightparen = true ;
      gfixptr = result ;
      gfix.push_back( gfixptr ) ;
      return result ;
    } // if



    if (  arg[0]-> mleft == NULL && arg[0]->mhasRightparen ) {    // ( 3 . 4 )
      if (  arg[0]->mtoken.size() == 2 ) {
        result = new List ;
        result -> mhasRightparen = false ;
        result->mtoken.push_back( arg[0]->mtoken[1] ) ;
        return result ;
      } // if
    } // if

    if (  arg[0]-> mleft != NULL && arg[0]->mtoken.size() > 0 ) { // ( ( 30 40 ) . 3 )
      result = new List ;
      result -> mhasRightparen = false ;
      result->mtoken.push_back( arg[0]->mtoken[0] ) ;
      return result ;
    } // if

    if ( arg[0]->mright == NULL &&  arg[0]->mhasRightparen &&  arg[0]->mtoken.size() == 1  ) { // 沒有右邊
      result = new List ;
      result -> mhasRightparen = false ;
      temptoken.mContent = "nil" ;
      temptoken.mTokentype = NIL ;
      result->mtoken.push_back( temptoken ) ;
      return result ;
    } // if

    if ( result == NULL ) {
      if (  !arg[0]->mhasRightparen && arg[0]->mtoken.size() == 1   ) {
        cout << endl << "> ERROR (cdr with incorrect argument type) : "  ;
        cout << arg[0]->mtoken[0].mContent << endl ;
        error = true ;
        return NULL ;
      } // if

      else if (  !arg[0]->mhasRightparen && arg[0]->mtoken.size() == 2 )  {  // non -list
        cout << endl << "> ERROR (non-list) : "  ;
        string a = "" ; // 用不到
        Printlist( root, false, a ) ;
        error = true ;
        return NULL ;
      } // else if
    } // if

    return result ;
  } // Cdr()


  Listptr Eq( Listptr root, bool &error ) {
    vector<Listptr> arg ;
    Listptr result = NULL  ;
    Token temptoken ;
    Listptr temp = NULL ;
    int nowarg = 0 ;
    bool isfloat = false  ;
    int resultint = 0 ;
    float resultfloat = 0 ;
    Splitarg( root, arg ) ;

    if ( arg.size() == 1 || arg.size() == 0  ) {
      cout << endl << "> ERROR (incorrect number of arguments) : >" << endl ;
      error = true ;
      return NULL ;
    } // if
    else {
      Fixarg( arg, error ) ; // 把 arg裡的symbol變成正確的
      if ( error ) {
        return NULL ;
      } // if
    } // else

    for ( int i = 0 ; i < arg.size() ; i++ ) {
      if ( arg[i]->mhasRightparen ) {
        cout << endl << "> ERROR (> with incorrect argument type) : "   ;
        string a = "" ; // 用不到
        Printlist( arg[i], false, a ) ;
        error = true ;
        return NULL ;
      } // if

      else if ( arg[i]->mtoken[0].mTokentype != INT && arg[i]->mtoken[0].mTokentype != FLOAT ) {
        cout << endl << "> ERROR (> with incorrect argument type) : "   ;
        cout <<  arg[i]->mtoken[0].mContent ;
        cout << endl ;
        error = true ;
        return NULL ;
      } // else if


      else if ( arg[i]->mtoken[0].mTokentype == FLOAT ) {
        isfloat = true ;
      } // else if
    } // for

    if ( isfloat ) {
      resultfloat = atof( arg[0]->mtoken[0].mContent.c_str() ) ;
    } // if

    else {
      resultint =  atoi( arg[0]->mtoken[0].mContent.c_str() ) ;
    } // else

    for ( int j = 1 ; j < arg.size() ; j++ ) {
      if ( isfloat ) {
        if ( resultfloat == atof( arg[j]->mtoken[0].mContent.c_str() ) ) {
          resultfloat = atof( arg[j]->mtoken[0].mContent.c_str() ) ;
        } // if
        else {
          result = new List ;
          temptoken.mContent = "nil" ;
          temptoken.mTokentype = NIL ;
          result->mtoken.push_back( temptoken ) ;
          return result ;
        } // else
      } // if

      else {
        if ( resultint == atoi( arg[j]->mtoken[0].mContent.c_str() ) ) {
          resultint = atoi( arg[j]->mtoken[0].mContent.c_str() ) ;
        } // if
        else {
          result = new List ;
          temptoken.mContent = "nil" ;
          temptoken.mTokentype = NIL ;
          result->mtoken.push_back( temptoken ) ;
          return result ;
        } // else
      } // else
    } // for

    result = new List ;
    temptoken.mContent = "#t" ;
    temptoken.mTokentype = T ;
    result->mtoken.push_back( temptoken ) ;
    return result ;
  } // Eq()



  Listptr Evaluate(  Listptr  root, bool &error ) {
    Listptr  result  ; // result
    int nowsexp = 0 ;
    error = false ;
    Listptr tempright ;
    if ( root->mhasRightparen && root->mtoken.size() == 0 &&  !root->mhasQuote ) {
      tempright = root -> mright ;
      string a = " " ;
      root = Evaluate( root->mleft, error ) ;
      if ( !error ) {
        root -> mright = tempright ;
        root-> mhasRightparen = true ;
        gfixptr = NULL ;
      } // if
      else
        return NULL ;

    } // if

    if ( root->mhasRightparen  ) {      // 有command
      if ( IsandGetinternalCmd( root )  ) {
        if ( root -> mhasQuote ) {
          result = Quote( root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if
          else
            return NULL ;
        } // if

        else if ( root->mtoken[0].mTokentype == CONS ) {
          result = Cons(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == LIST ) {
          result = Lis(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == DEFINE ) {
          Define( root, error ) ;
          error = true ; // 出去不用印

          if ( error )
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == CAR ) {
          result = Car(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == CDR ) {
          result = Cdr(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == ISATOM ) {
          result = Isatom(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == ISINT ) {
          result = Isinteger(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == ISREAL ) {
          result = Isreal(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == ISNUM ) {
          result = Isnumber(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == ISSTR ) {
          result = Isstring(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == ISBOOL ) {
          result = Isbool(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == ISSYM ) {
          result = Issymbol(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == ISNULL ) {
          result = Isnull(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == ISPAIR ) {
          result = Ispair(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == ISLIST ) {
          result = Islist(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == BEGIN ) {
          result = Begin(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == ADD ) {
          result = Add(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == SUB ) {
          result = Sub(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == MULT ) {
          result = Mult(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == DIV ) {
          result = Div(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == NOT ) {
          result = Not(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == AND ) {
          result = And(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == OR ) {
          result = Or(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == BIGG ) {
          result = Bigg(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == BIGEQ ) {
          result = Bigeq(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == SMLEQ ) {
          result = Smleq(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == SML ) {
          result = Sml(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == STRAPP ) {
          result = Strapp(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == ISEQV ) {
          result = Iseqv(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == ISEQL ) {
          result = Iseql(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == STRBIG ) {
          result = Strbig(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == STRSML ) {
          result = Strsml(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == STREQL ) {
          result = Streql(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == EQ ) {
          result = Eq(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == IF ) {
          result = If(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == COND ) {
          result = Cond(  root, error ) ;
          if ( !error ) {
            result -> miseva = true ;
            return result ;
          } // if

          else
            return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == CLEAN ) {
          result = Clean(  root, error ) ;
          error = true ;
          return NULL ;
        } // else if

        else if ( root->mtoken[0].mTokentype == EXIT ) {
          cout << "\n> ERROR (incorrect number of arguments) : exit\n" ;
          error = true ;
          return NULL ;
        } // else if

      } // if

      else {
        if ( root->mtoken[0].mTokentype == SYMBOL ) {
          cout << endl ;
          cout << "> " << "ERROR (unbound symbol) : " ;
          cout << root->mtoken[0].mContent << endl ;
          error = true ;
        } // if

        else {
          cout << endl ;
          cout << "> " << "ERROR (attempt to apply non-function) : " ;
          cout << root->mtoken[0].mContent << endl ;
          error = true ;
        } // else

        return NULL ;
      } // else
    } // if

    else {
      error = true ;
      cout << endl << "> Why you call this function?" << endl ;
      return NULL ;
    } // else

    error = true ;
    return NULL ;

  } // Evaluate()





  void Splitarg( Listptr root,  vector<Listptr> &arg ) {  // 不會抓到空的那一格
    Listptr temp1 ;
    Listptr result ;
    Listptr temp2 ;
    Listptr temp ;
    temp = root -> mright ;

    while ( temp != NULL ) { // 切參數
      if ( temp != NULL ) {
        if ( temp-> mleft == NULL && temp->mright != NULL ) {  // 左邊沒東西且右邊還有東西
          temp1 = new List ;
          for ( int i = 0 ; i < temp->mtoken.size()  ; i++ )
            temp1->mtoken.push_back( temp->mtoken[i] ) ;

          arg.push_back( temp1 ) ;
          temp = temp -> mright ;
        } // if

        else if ( temp-> mleft != NULL ) {   // 左邊有東西
          arg.push_back( temp->mleft ) ;
          temp = temp->mright ;
        } // if

        else {
          arg.push_back( temp ) ;
          temp = temp->mright ;
        } // else
      } // if
    }  // while

  } // Splitarg()





};


int main() {
  cout <<  " Welcome to OurScheme!" << endl ;
  Ourscheme proj1 ;
  vector<Token> tokenlist ;
  vector<Token> sexp ;
  string str = "" ;
  bool  eof = false ;
  int nowtoken = 0 ; //
  int nowsexp = 0 ;
  int hasleft = 0 ;
  gfixptr = NULL ;
  int maxsexp = 0 ;
  bool error = false ;
  List * root = NULL  ; // before
  List * result = NULL  ; // result
  cin >> gTestnum;
  eof = proj1.Getline( str ) ;
  proj1.GetTokenlist( str, tokenlist ) ;
  bool hasinput = false ;
  bool success = true ;

  while ( !eof && !proj1.IsExit( sexp )  ) {
    while ( !proj1.IsSexepresion( sexp ) && !eof  ) {
      while ( tokenlist.size() == 0 && !eof  ) {
        gLine ++ ;
        gCol = 0 ;
        eof = proj1.Getline( str ) ;
        if ( !eof  || str.size() > 0 )
          proj1.GetTokenlist( str, tokenlist ) ;
      } // while

      if ( !eof &&  tokenlist.size() > 0 ) {
        proj1.Buildsexp( sexp, tokenlist[nowtoken] ) ;
        tokenlist.erase( tokenlist.begin() ) ;

        error = proj1.CheckError( sexp ) ;
        if ( error ) {
          error = false ;
          proj1.Delitokenlist( tokenlist, sexp[sexp.size()-1] ) ;
          sexp.clear();
          gLine = 0 ;
          gCol = 0 ;
        } // if


      }   // if
    } // while

    if ( proj1.IsSexepresion( sexp ) ) {
      if ( tokenlist.size() != 0 ) {
        for ( int z = 0 ; z < tokenlist.size() ; z++ ) {
          tokenlist[z].line = 1 ;
          tokenlist[z].col = tokenlist[z].col - sexp[ sexp.size() - 1 ].col ;

        }  // for

        gLine = 1 ;
        gCol = 0 ;
      } // if

      else {
        gLine = 0 ;
        gCol = 0 ;
      } // else

      proj1.FixSexp( sexp ) ;
    }  // if


    if ( proj1.IsSexepresion( sexp ) && !error && !proj1.IsExit( sexp ) ) {
      hasinput = true ;


      if ( ( sexp[0].mTokentype == LEFT_PAREN || sexp[0].mTokentype == QUOTE )  ) {
        if ( sexp[0].mTokentype == LEFT_PAREN ) {
          root = new List ;
          root -> mhasRightparen = true ;
        } // if

        if ( sexp[0].mTokentype == QUOTE ) {
          root = new List ;
          root -> mhasRightparen = true ;
          root -> mhasQuote = true ;

        } // if

        nowsexp = 1 ;
        proj1.ReadSexp( sexp, root, sexp.size(), nowsexp, false ) ; // before
        result = proj1.Evaluate(  root, error ) ;


        if ( !error ) {
          cout << endl ;
          cout << "> " ;
          string a = "" ; // 用不到
          proj1.Printlist( result,  false, a ) ;
          if ( gfix.size() > 0 ) {
            for ( int a = 0 ; a < gfix.size() ; a++ ) {
              if ( gfix[a]-> mhasRightparen == false )
                gfix[a]-> mhasRightparen = true ;
            } // for
          } // if

        } // if

        gfirstprint = true ;
        gfix.clear() ;
        gfixptr = NULL ;
        gparennum = 0 ;
        sexp.clear() ;
        root = NULL ;
        error = false ;
        result = NULL ;
      } // if


      else {
        cout << endl ;
        proj1.PrintSexep( sexp ) ;

        sexp.clear();
      } // else
    } // if
  } // while


  if ( tokenlist.size() > 0 && eof && !proj1.IsExit( sexp ) ) {
    while ( tokenlist.size() > 0    ) {
      proj1.Buildsexp( sexp, tokenlist[nowtoken] ) ;
      tokenlist.erase( tokenlist.begin() ) ;
      error = proj1.CheckError( sexp ) ;
      success = false ;
      if ( error ) {
        error = false ;
        proj1.Delitokenlist( tokenlist, sexp[sexp.size()-1] ) ;
        sexp.clear();
        gLine = 0 ;
        gCol = 0 ;
      } // if

      else {
        if ( proj1.IsSexepresion( sexp ) && !error && !proj1.IsExit( sexp ) ) {
          for ( int z = 0 ; z < tokenlist.size() ; z++ ) {
            tokenlist[z].line = 1 ;
            tokenlist[z].col = tokenlist[z].col - sexp[ sexp.size() - 1 ].col ;
          } // for

          proj1.FixSexp( sexp ) ;
          if ( ( sexp[0].mTokentype == LEFT_PAREN || sexp[0].mTokentype == QUOTE )  ) {
            if ( sexp[0].mTokentype == LEFT_PAREN ) {
              root = new List ;
              root -> mhasRightparen = true ;
            } // if

            if ( sexp[0].mTokentype == QUOTE ) {
              root = new List ;
              root -> mhasRightparen = true ;
              root -> mhasQuote = true ;
            } // if

            nowsexp = 1 ;
            proj1.ReadSexp( sexp, root, sexp.size(), nowsexp, false ) ;

            result = proj1.Evaluate(  root, error ) ;

            if ( !error ) {
              cout << endl ;
              cout << "> " ;
              string a = "" ; // 用不到
              proj1.Printlist( result,  false, a ) ;

              if ( gfix.size() > 0 ) {
                for ( int a = 0 ; a < gfix.size() ; a++ ) {
                  if ( gfix[a]-> mhasRightparen == false )
                    gfix[a]-> mhasRightparen = true ;
                } // for
              } // if
            } // if

            success = true ;
            gfirstprint = true ;
            gfix.clear() ;
            gfixptr = NULL ;
            gparennum = 0 ;
            sexp.clear() ;
            root = NULL ;
            error = false ;
            result = NULL ;
          } // if

          else {
            cout << endl ;
            proj1.PrintSexep( sexp ) ;
            sexp.clear();
            success = true ;
          } // else

        } // if
      } // else
    } // while

  } // if


  if ( eof && !proj1.IsExit( sexp ) ) {
    if ( hasinput ) {
      if ( !success )
        cout << endl << "> " << endl ;

      cout << endl << "> " << "ERROR (no more input) : END-OF-FILE encountered" ;
    } // if
    else
      cout <<  "> " << "ERROR (no more input) : END-OF-FILE encountered" ;
  } // if

  else {
    if ( hasinput )
      cout << endl << "> "  ;
    else
      cout <<  "> "  ;
  } // else

  cout << endl << "Thanks for using OurScheme!" ;

} // main()
