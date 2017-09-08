package org.hibernate.dialect;

import java.sql.Types;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class SQLServerUnicodeDialect extends SQLServerDialect {

  public SQLServerUnicodeDialect() {
    super();
    registerColumnType( Types.CHAR, "nchar(1)" );
		registerColumnType( Types.VARCHAR, "nvarchar($l)" );
    registerColumnType( Types.CLOB, "ntext" );
  }

}
