package org.jblooming.ontology;

import org.hibernate.HibernateException;
import org.hibernate.engine.spi.SessionImplementor;
import org.hibernate.usertype.UserType;

import java.io.Serializable;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 */
public class PersistentFileType implements UserType {

  private static final int[] SQL_TYPES = {Types.VARCHAR};

  public int[] sqlTypes() {
    return SQL_TYPES;
  }

  public Class returnedClass() {
    return PersistentFile.class;
  }

  public boolean equals(Object x, Object y) throws HibernateException {
    if (x == y) return true;
    if (x == null || y == null) return false;
    return x.equals(y);
  }

  public int hashCode(Object object) throws HibernateException {
    return 0;  //To change body of implemented methods use File | Settings | File Templates.
  }

  public Object nullSafeGet(ResultSet resultSet, String[] names, SessionImplementor session, Object owner) throws HibernateException, SQLException {
    PersistentFile result = null;
    //if (resultSet.wasNull()) return null;
    if (resultSet != null) {
      String serObj = resultSet.getString(names[0]);
      if (!resultSet.wasNull() && serObj != null)
        result = PersistentFile.deserialize(serObj);
    }
    return result;
  }

  public void nullSafeSet(PreparedStatement statement, Object value, int index, SessionImplementor session) throws HibernateException, SQLException {
    if (value == null) {
      statement.setNull(index, Types.VARCHAR);
    } else {
      PersistentFile persistentFile = (PersistentFile) value;
      statement.setString(index, persistentFile.serialize());
    }
  }



  public Object deepCopy(Object o) throws HibernateException {
    if (o!=null)
      return PersistentFile.deserialize(((PersistentFile)o).serialize());
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
