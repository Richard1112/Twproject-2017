package org.hibernate.dialect;

import org.hibernate.type.StandardBasicTypes;

/**
 * OLDB2Dialect (c) 2008 - Open Lab - www.open-lab.com
 */
public class OLDB2Dialect extends DB2Dialect {

  public OLDB2Dialect() {
    super();
    registerFunction("lower", new OLCastStandardSQLFunction("lower", StandardBasicTypes.STRING) );
    registerFunction("lcase", new OLCastStandardSQLFunction("lcase", StandardBasicTypes.STRING) );
    registerFunction("upper", new OLCastStandardSQLFunction("upper", StandardBasicTypes.STRING) );
    registerFunction("ucase", new OLCastStandardSQLFunction("ucase", StandardBasicTypes.STRING) );
  }

/*****************************************************************************************************************************************

 on teoros experience::
 the CLOB fields produced by Hibernate schema generation is very small.
 In fact, they were defaulting to a size of 255 bytes.
 That is pretty small for any piece of metadata.
 DB2 CLOBs used a number and a scaling factor in their DDL language like this:
 FIELD_NAME CLOB (1024 M)


 from:: http://www.mediafly.com/Podcasts/Episodes/Hibernate_Dialect_for_DB2

*****************************************************************************************************************************************/

  /**
   * Get the name of the database type associated with the given
   * {@link java.sql.Types} typecode with the given storage specification
   * parameters. Note: this code calls the parent class getTypeName method
   * unless the type is a CLOB. If the type is a CLOB, the length field from
   * the Hibernate mapping is used as the length of the CLOB. Valid values are
   * from 1 to 99999.
   *
   * The scale field is used to specify the scale of the CLOB. Valid values are 0 - bytes, 1 - kilobytes
   * 2 - megabytes, and 3 - gigabytes. The default is megabytes.
   *
   * @param code The {@link java.sql.Types} typecode
   * @param length The datatype length
   * @param precision The datatype precision
   * @param scale The datatype scale
   * @return the database type name
   * @throws org.hibernate.HibernateException If no mapping was specified for that type.
   */
  /*
  public String getTypeName(int code, int length, int precision, int scale) {
    // validate the length
    if (length < 1 || length > 99999)
      length = 1024;
    // seed the typename
    String result = super.getTypeName( code, length, precision, scale );

    // fix up the clob type for DB2 to create it properly
    if (code == Types.CLOB) {
      StringBuffer buffer = new StringBuffer(16);
      // add the first part of the string
      buffer.append(result.substring(0, result.indexOf(")")));
      // add the scale for bytes/kbytes/mbytes/gbytes
      switch (scale) {
        case 0:
          buffer.append(" ");
          break;
        case 1:
          buffer.append(" K ");
          break;
        case 2:
          buffer.append(" M ");
          break;
        case 3:
          buffer.append(" G ");
          break;
        default:
          buffer.append(" M ");
          break;
      }
      buffer.append(")");
      result = buffer.toString();
    }
    return result;
  }
 */




/*  COULD BE USEFUL IN FUTURE
	public boolean supportsLimitOffset() {
		return false;
	}
	public boolean supportsVariableLimit() {
		return false;
	}
	public String getLimitString(String query, int offset, int limit) {
		if (offset > 0) {
			return
				"select * from (select rownumber() over () as rownumber, t.* from (" +
				query +
				" fetch first " + limit + " row only " +
				") as t)as t where rownumber > " + offset;
		}
    System.out.println(query + " fetch first " + limit + " row only ");
		return query + " fetch first " + limit + " row only ";
	}
	public static void main(String[] args) {
		System.out.println( new DB2Dialect().getLimitString("select * from essai e", -1, 30) );
		System.out.println( new DB2Dialect().getLimitString("select * from essai e", 20, 30) );
	}
	*/
}