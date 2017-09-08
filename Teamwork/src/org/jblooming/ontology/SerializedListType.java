package org.jblooming.ontology;

import org.hibernate.usertype.UserType;
import org.hibernate.HibernateException;
import org.hibernate.engine.spi.SessionImplementor;

import java.sql.Types;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.PreparedStatement;
import java.io.Serializable;

public class SerializedListType implements UserType {
  public Object nullSafeGet(ResultSet resultSet, String[] names, SessionImplementor session, Object owner) throws HibernateException, SQLException {
    SerializedList result = null;
    //if (resultSet.wasNull()) return null;
    if (resultSet != null) {
      String serObj = resultSet.getString(names[0]);
      if (!resultSet.wasNull() && serObj != null)
        result = SerializedList.deserialize(serObj);
    }
    return result;
  }

  public void nullSafeSet(PreparedStatement statement, Object value, int index, SessionImplementor session) throws HibernateException, SQLException {
    if (value == null) {
      statement.setNull(index, Types.VARCHAR);
    } else {
      SerializedList serializedList = (SerializedList) value;
      statement.setString(index, serializedList.serialize());
    }
  }

  private static final int[] SQL_TYPES = {Types.VARCHAR};

  public int[] sqlTypes() {
    return SerializedListType.SQL_TYPES;
  }

  public Class returnedClass() {
    return SerializedList.class;
  }

  public boolean equals(Object x, Object y) throws HibernateException {
    if (x == y) return true;
    if (x == null || y == null) return false;
    return x.equals(y);
  }

  public int hashCode(Object object) throws HibernateException {
    return 0;  //To change body of implemented methods use File | Settings | File Templates.
  }


  public Object deepCopy(Object o) throws HibernateException {
    if (o!=null)
      return SerializedList.deserialize(((SerializedList)o).serialize());
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
