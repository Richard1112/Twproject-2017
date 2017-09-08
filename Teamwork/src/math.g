class ExprParser extends Parser;

options {
        buildAST=true;
}

expr returns [double value=0]
{double x;}
    :   value=mexpr
        ( PLUS x=mexpr  {value += x;} | MINUS x=mexpr {value -= x;} )*
    ;


mexpr returns [double value=0]
{double x;}
    :   value=atom ( STAR x=atom {value *= x;} | POWER x=atom {value = (Math.pow(value,x));} | DIV x=atom {value = (value/x);} )*
    ;

atom returns [double value=0]
{double x;}
    :   i:NUM {value=Double.parseDouble(i.getText());} |   LPAREN value=expr RPAREN | MINUS x=mexpr {value=-x;}
    ;


class ExprLexer extends Lexer;

options {
    k=2; // needed for newline junk
    charVocabulary='\u0000'..'\u007F'; // allow ascii
}

LPAREN: '(' ;
RPAREN: ')' ;
PLUS  : '+' ;
MINUS : '-' ;
STAR  : '*' ;
POWER : '^' ;
DIV   : '/' ;
NUM   : ('0'..'9' | '.')+ ;
WS    : ( ' '
        | '\r' '\n'
        | '\n'
        | '\t'
        )
        {$setType(Token.SKIP);}
      ;    
