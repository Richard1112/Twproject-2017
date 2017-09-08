package org.jblooming.utilities.math;

import antlr.*;

import java.io.Reader;
import java.io.StringReader;
import java.text.ParseException;


/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class MathParse {

   public static double parse(String exp) throws ParseException {
     try {
    Reader reader = new StringReader(exp);
    ExprLexer lexer = new ExprLexer(reader);
    ExprParser parser = new ExprParser(lexer);
    double result = parser.expr();
    return result;
     } catch (Throwable t) {
       throw new ParseException(t.getMessage(),0);
     }
  }


}
