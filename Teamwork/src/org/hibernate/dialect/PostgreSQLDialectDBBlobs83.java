package org.hibernate.dialect;

import org.hibernate.dialect.function.StandardSQLFunction;
import org.hibernate.id.IdentityGenerator;
import org.hibernate.type.StandardBasicTypes;


public class PostgreSQLDialectDBBlobs83 extends PostgreSQL82Dialect {

  public PostgreSQLDialectDBBlobs83() {
    super();
    registerFunction("length", new StandardSQLFunction("length", StandardBasicTypes.LONG));
  }




  /*
   Generate one sequence for each table
   */
  public Class getNativeIdentifierGeneratorClass() {
    return IdentityGenerator.class;
  }

}
