package org.hibernate.dialect;

import org.hibernate.dialect.function.StandardSQLFunction;
import org.hibernate.type.StandardBasicTypes;


public class PostgreSQLDialectDBBlobs extends PostgreSQL82Dialect {

  public PostgreSQLDialectDBBlobs() {
    super();
    registerFunction("length", new StandardSQLFunction("length", StandardBasicTypes.LONG));
  }

}
