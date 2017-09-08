package org.hibernate.dialect;

import java.sql.Types;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class SQLServer2008UnicodeDialect extends SQLServer2008Dialect {

  public SQLServer2008UnicodeDialect() {
    super();
    registerColumnType( Types.CHAR, "nchar(1)" );
    //registerColumnType( Types.VARCHAR, "nvarchar($l)" );
    //registerColumnType( Types.CLOB, "ntext" );

    registerColumnType( Types.CLOB, "nvarchar(MAX)" );
    registerColumnType( Types.LONGVARCHAR, "nvarchar(MAX)" );
    registerColumnType( Types.VARCHAR, "nvarchar(MAX)" );
    registerColumnType( Types.VARCHAR, 8000, "nvarchar($l)" );
  }

}
