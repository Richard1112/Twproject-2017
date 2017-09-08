package org.hibernate.dialect;
import java.sql.Types;

public class Oracle10gDialectDBText extends Oracle10gDialect{

  public Oracle10gDialectDBText() {
     super();
     registerColumnType(Types.LONGVARCHAR, "clob");
     registerColumnType(Types.CLOB, "clob");
   }

  public String getQuerySequencesString() {
    return    "select sequence_name from user_sequences";
  }

}
