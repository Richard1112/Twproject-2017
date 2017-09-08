package org.hibernate.dialect;

import java.sql.Types;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class HSQL172PlusDialect extends HSQLDialect {

  public HSQL172PlusDialect(){
    super();
    registerColumnType( Types.BLOB, "longvarbinary" );
    registerColumnType( Types.CLOB, "longvarchar" );

  }



}
