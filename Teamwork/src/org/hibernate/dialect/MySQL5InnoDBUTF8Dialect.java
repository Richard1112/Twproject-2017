package org.hibernate.dialect;

import java.sql.Types;

public class MySQL5InnoDBUTF8Dialect extends MySQL5InnoDBDialect {


  public MySQL5InnoDBUTF8Dialect() {
    super();
    //registerColumnType( Types.TIMESTAMP,6, "datetime($1)" );
    //registerColumnType( Types.TIMESTAMP, "datetime(3)" );
  }

  // Create all tables as default UTF8!
  @Override
  public String getTableTypeString() {
    return " ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE utf8_general_ci";
  }
}
