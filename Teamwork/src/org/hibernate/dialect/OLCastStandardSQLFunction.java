package org.hibernate.dialect;

import org.hibernate.dialect.function.StandardSQLFunction;
import java.util.List;


import org.hibernate.type.Type;
import org.hibernate.engine.spi.SessionFactoryImplementor;

/**
 * OLCastStandardSQLFunction (c) 2008 - Open Lab - www.open-lab.com
 * to be used ONLY in case of dbs where parameter marker can't be of undefined Type/untyped (i.e. DB2 lower function)
 *
 * exception thrown:: A statement contains a use of a parameter marker that is not valid
 * required solution:: Correct the syntax of the statement. If untyped parameter markers are not allowed, use the CAST specification to give the parameter marker a data type.
 */
public class OLCastStandardSQLFunction extends StandardSQLFunction {

private final String name;
//private final Type type;

	/**
	 * Construct a cast SQL function definition with a variable return type;
	 * the actual return type will depend on the types to which the function is applied.
	 *
	 * @param name The name of the SQL function.
	 */
	public OLCastStandardSQLFunction(String name) {
		this( name, null );
	}

	/**
	 * Construct a cast SQL function definition with a static return type.
	 *
	 * @param name The name of the function.
	 * @param type The static return type.
	 */
	public OLCastStandardSQLFunction(String name, Type type) {
		super(name, type);
		this.name = name;
		//this.type= type;
	}

	public String render(List args, SessionFactoryImplementor factory) {
		StringBuffer buf = new StringBuffer();
		buf.append( name ).append( '(' );
		for ( int i = 0; i < args.size(); i++ ) {
			if("?".equals(args.get(i))) {
        buf.append("cast( " + args.get(i)+" as VARCHAR(10000))");
      } else {
			  buf.append(args.get(i));
      }
			if ( i < args.size() - 1 ) {
				buf.append( ", " );
			}
		}
		return buf.append( ')' ).toString();
	}

}