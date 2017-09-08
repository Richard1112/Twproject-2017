package org.jblooming.ontology;

import org.hibernate.usertype.UserType;
import org.hibernate.HibernateException;
import org.hibernate.engine.spi.SessionImplementor;

import java.sql.Types;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.PreparedStatement;
import java.io.Serializable;

public class SerializedMapType implements UserType {

  private static final int[] SQL_TYPES = {Types.VARCHAR};

  public int[] sqlTypes() {
    return SerializedMapType.SQL_TYPES;
  }

  public Class returnedClass() {
    return SerializedMap.class;
  }

  public boolean equals(Object x, Object y) throws HibernateException {
    if (x == y) return true;
    if (x == null || y == null) return false;
    /*return ((SerializedMap) x).serialize().equals(((SerializedMap) y).serialize());*/
    return x.equals(y);
  }

  public int hashCode(Object object) throws HibernateException {
    return 0;  //To change body of implemented methods use File | Settings | File Templates.
  }

  public Object nullSafeGet(ResultSet resultSet, String[] names, SessionImplementor session, Object owner) throws HibernateException, SQLException {
    SerializedMap result = null;
    //if (resultSet.wasNull()) return null;
    if (resultSet != null) {
      String serObj = resultSet.getString(names[0]);
      if (!resultSet.wasNull() && serObj != null)
        result = SerializedMap.deserialize(serObj);
    }
    return result;
  }

  public void nullSafeSet(PreparedStatement statement, Object value, int index ,SessionImplementor session) throws HibernateException, SQLException {
    if (value == null) {
      statement.setNull(index, Types.VARCHAR);
    } else {
      SerializedMap serializedMap = (SerializedMap) value;
      statement.setString(index, serializedMap.serialize());
    }
  }

  public Object deepCopy(Object o) throws HibernateException {
    if (o != null)
      return SerializedMap.deserialize(((SerializedMap) o).serialize());
    else
    return o;
  }

  public boolean isMutable() {
    return false;
  }

  public Serializable disassemble(Object object) throws HibernateException {
    return null;
  }

  public Object assemble(Serializable serializable,Object object) throws HibernateException {
    return null;
  }

  public Object replace(Object object,Object object1,Object object2) throws HibernateException {
    return null;
  }

}
