package org.jblooming.ontology;

import org.hibernate.usertype.UserType;
import org.hibernate.HibernateException;

import java.sql.Types;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.PreparedStatement;
import java.io.Serializable;

public class SerializedListClobType extends SerializedListType{

  public int[] sqlTypes() {
    return new int[]{Types.CLOB};
  }

  public void nullSafeSet(PreparedStatement statement, Object value, int index) throws HibernateException, SQLException {
    if (value == null) {
      statement.setNull(index, Types.CLOB);
    } else {
      SerializedList serializedList = (SerializedList) value;
      statement.setString(index, serializedList.serialize());
    }
  }

}