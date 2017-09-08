package org.jblooming.ontology;

import org.hibernate.HibernateException;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Types;

public class SerializedMapClobType extends SerializedMapType{
  public int[] sqlTypes() {
    return new int[]{Types.CLOB};
  }

  public void nullSafeSet(PreparedStatement statement, Object value, int index) throws HibernateException, SQLException {
    if (value == null) {
      statement.setNull(index, Types.CLOB);
    } else {
      SerializedMap serializedMap = (SerializedMap) value;
      statement.setString(index, serializedMap.serialize());
    }
  }

}
