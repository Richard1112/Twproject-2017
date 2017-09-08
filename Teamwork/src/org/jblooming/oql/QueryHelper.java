package org.jblooming.oql;

import org.hibernate.HibernateException;
import org.hibernate.engine.query.spi.EntityGraphQueryHint;
import org.hibernate.hql.spi.QueryTranslator;
import org.hibernate.hql.spi.ParameterTranslations;
import org.hibernate.hql.internal.ast.ASTQueryTranslatorFactory;
import org.hibernate.engine.spi.SessionFactoryImplementor;
import org.hibernate.dialect.Dialect;
import org.hibernate.type.LiteralType;
import org.hibernate.type.Type;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.utilities.*;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.waf.constants.Fields;
import org.jblooming.agenda.CompanyCalendar;

import java.text.NumberFormat;
import java.text.ParseException;
import java.util.*;
import java.util.regex.Matcher;

public class QueryHelper {

  public static final String TYPE_CHAR = "C";
  public static final String TYPE_CLOB = "CLOB";
  public static final String TYPE_DATE = "D";
  public static final String TYPE_INT = "N";
  public static final String TYPE_MILLIS = "M"; // accept all shortcuts for millis eg 2, 2h, 1.5, 1:30, 2d1h3m etc
  /**
   * @deprecated use TYPE_INT
   */
  public static final String TYPE_NUM = "N";
  public static final String TYPE_DOUBLE = "DBL";
  public static final String TYPE_FLOAT = "FLT";
  public static final String TYPE_LONG = "LOG";
  public static final String TYPE_logicAND = " and ";
  public static final String TYPE_logicOR = " or ";

  public static final String QBE_CONVERT_TO_UPPER = "QBE_CONVERT_TO_UPPER";

  // in case of sqlserver is false
  public static boolean CLOB_SUPPORTS_UPPER =true;

  // set the default behaviour for uppercase
  public boolean convertToUpper = Fields.TRUE.equals(ApplicationState.getApplicationSetting(QBE_CONVERT_TO_UPPER));

  //"transient" value in single method call: is not empty when calling say addQBEClause, in parsing QBE condition invalid use is found
  private String improperUseOfQBEErrorCode = null;
  private boolean invalidQBE = false;

  public boolean isValidQBE() {
    return !invalidQBE;
  }

  public enum DefaultMatch {
    EQUALS, STARTS_WITH, CONTAINS
  }

  public static Set<String> qbeOperatorsChars = CollectionUtilities.toSet("=", "!", "\"", "<", ">", "(", ")", "[", "]", "*", "#", "+", "//", ":");

  public DefaultMatch defaultMatch = DefaultMatch.CONTAINS;

  private String hql = "";
  private Map args = new HashTable();

  public QueryHelper(String hql) {
    this.hql = hql;
  }

  public class QueryHelperElement {

    public QueryHelperElement(String property, String alias, String type) {
      this.property = property;
      this.alias = alias;
      this.type = type;
    }

    public String property;
    public String alias;
    public String type;

  }

  public QueryHelperElement getOrElement(String property, String alias, String type) {
    return new QueryHelperElement(property, alias, type);
  }


  public OqlQuery toHql() throws PersistenceException {
    return toHql(hql);
  }

  public OqlQuery toHql(PersistenceContext pc) throws PersistenceException {
    return toHql(hql, pc);
  }

  public OqlQuery toHql(String query) throws PersistenceException {
    return toHql(query, getArgs());
  }

  public OqlQuery toHql(String queryString, Map args) throws PersistenceException {
    OqlQuery oql = new OqlQuery(queryString);
    return toHqlArgued(args, oql);
  }

  public OqlQuery toHql(String queryString, PersistenceContext pc) throws PersistenceException {
    OqlQuery oql = new OqlQuery(queryString, pc);
    return toHqlArgued(getArgs(), oql);
  }

  private OqlQuery toHqlArgued(Map args, OqlQuery oql) throws PersistenceException {
    if (args != null && args.size() > 0) {
      for (Iterator iterator = args.keySet().iterator(); iterator.hasNext();) {
        String paramName = (String) iterator.next();
        Object value = args.get(paramName);
        if (value instanceof Collection)
          try {
            oql.getQuery().setParameterList(paramName, (Collection) value);
          } catch (HibernateException e) {
            throw new PersistenceException(e);
          }
        else
          oql.setParameter(paramName, value);
      }
    }
    return oql;
  }

  public String getHqlString() {
    return hql;
  }


  /**
   * Only in very rare cases it should be used. If your name doesn't start with R||P avoid to use it.
   *
   * @param hql
   */
  public void setHqlString(String hql) {
    this.hql = hql;
  }

  public void wrapHql(String pre,String post){
    this.hql= JSP.w(pre)+" "+this.hql+" "+JSP.w(post);
  }

  public void addOQLClause(String condition) {
    addOQLClause(condition, null, null);
  }

  public void addOQLInClause(String property, String namedParameter, List arg) {
    addOQLClause(property + " in (:" + namedParameter + ")", namedParameter, arg);
  }

  /**
   * @param condition condition of the sort: t.name=:paramName
   * @param paramName this must be stated explicitly as the params are to be held in a separate map
   * @param arg       the filter e.g. "a*"
   */

  public void addOQLClause(String condition, String paramName, Object arg) {
    if (paramName != null && arg != null)
      addParameter(paramName, arg);
    privateAddQueryClause(condition);
  }

  /**
   * @param condition condition of the sort: t.name=:paramName
   * @param paramName this must be stated explicitly as the params are to be held in a separate map
   * @param arg       the filter e.g. "a*"
   */

  public StringBuffer getOQLClause(String condition, String paramName, Object arg) {
    StringBuffer ret = new StringBuffer();
    if (paramName != null && arg != null)
      addParameter(paramName, arg);
    ret.append(condition);
    return ret;
  }

  /**
   * @param property       of the object on which to filter by QBE e.g. "description"
   * @param namedParameter this must be stated explicitly as the params are to be held in a separate map
   * @param qbeString      the filter e.g. "a*"
   * @param type           is TYPE_CHAR or TYPE_DATE of the property in which we are searching
   */

  public void addQBEORClause(String property, String namedParameter, String qbeString, String type) {
    addQBEORClause(property, namedParameter, qbeString, type);
  }

  /**
   * This method should be used to test a joined abject field. E.g. assignment.resource.name  if search for () is translated to assignment is null instead of assignment.resource.name is null that generate an error
   */
  public void addQBEClauseForJoinedEntity(String property, String namedParameter, String qbeString, String type) {
    privateAddQueryClause(privateGetQbeClause(property, namedParameter, qbeString, type,true).toString());
  }

  public void addQBEClause(String property, String namedParameter, String qbeString, String type) {
    privateAddQueryClause(getQbeClause(property, namedParameter, qbeString, type).toString());
  }

  public void addQBEORClauses(String filter, QueryHelperElement... clauses) {

    String query = "(";
    boolean isFirst = true;
    for (QueryHelperElement qhe : clauses) {
      query += (!isFirst ? " or " : "") + getQbeClause(qhe.property, qhe.alias, filter, qhe.type).toString();
      isFirst = false;
    }
    query += " )";
    addQueryClause(query);
  }

  public void addParameter(String key, Object value) {
    if (!getArgs().containsKey(key))
      getArgs().put(key, value);
  }

  public void setParameter(String key, Object value) {
    getArgs().put(key, value);
  }

  public StringBuffer getQbeClause(String property, String alias, String testo, String type) {
    return privateGetQbeClause(property, alias, testo, type,false);
  }

  private StringBuffer privateGetQbeClause(String property, String alias, String testo, String type,boolean isExternalJoinedObjectField) {
    StringBuffer ret = new StringBuffer();
    testo = testo.trim().replaceAll("\\*", "%");

    if (convertToUpper && (TYPE_CHAR.equals(type) || TYPE_CLOB.equals(type)&& CLOB_SUPPORTS_UPPER)) {  // upper non supportato su molti db
      testo = testo.toUpperCase();
    }

    testo = testo.replaceAll("  ", " ");

    testo = testo.replaceAll(" \\+ ", "&");
    testo = testo.replaceAll("\\+ ", "&");
    testo = testo.replaceAll(" \\+", "&");

    testo = testo.replaceAll(" and ", "+");
    testo = testo.replaceAll(" AND ", "+");
    testo = testo.replaceAll(" + ", "+");
    testo = testo.replaceAll(" or ", "|");
    testo = testo.replaceAll(" OR ", "|");
    testo = testo.replaceAll(" , ", "|");
    testo = testo.replaceAll(", ", "|");
    testo = testo.replaceAll(" ,", "|");
    testo = testo.replaceAll(",", "|");
    testo = testo.replaceAll(";", "|");


    // to search the string # must be
    if (testo.startsWith("#") && testo.length() > 1) {
      String valore = testo.substring(1);// Mid(testo, 3) ;
      //testo = "%" + valore + "%|%" + valore + "|" + valore + "%|=" + valore;
      testo = "% " + valore + " %|% " + valore + "|" + valore + " %|=" + valore; // se si ammazzano gli spazi non trova più la parola isolata
    } else {
      testo = parseString(testo);
    }
    ret.append(controlString(new String[]{",", "|"}, testo.replaceAll("\\+", ","), property, alias, type, isExternalJoinedObjectField));
    return ret;
  }

  private String controlString(String[] op_cond, String testo, String campo, String aliasIn, String tipo,boolean isExternalJoinedObjectField) {

    improperUseOfQBEErrorCode = null;

    String testof = testo;

    String testoFinale = "";// testo;
    String testolav = "";
    String testoins;
    String condnext = "";
    String condnext2 = "";
    String testodas = "";

    int pos1 = -1;
    int pos2 = -1;
    int pos = -1;
    int i = 0;
    int testcount = testof.length();
    if (op_cond.length > 0) {
      while (testcount > 0) {
        i++;
        pos1 = testof.indexOf(op_cond[0]);
        pos2 = testof.indexOf(op_cond[1]);
        testoins = "";
        if (((pos1) >= 0) || (pos2 >= 0)) {

          if (((pos1 >= 0 && pos2 >= 0) && (pos1 < pos2)) || ((pos2 < 0) && (pos1 >= 0))) {
            pos = pos1;
            condnext = TYPE_logicAND;
          } else if (((pos1 >= 0 && pos2 >= 0) && (pos2 < pos1)) || ((pos1 < 0) && (pos2 >= 0))) {
            pos = pos2;
            condnext = TYPE_logicOR;
          }


          if (pos >= 0)
            testoins = testof.substring(0, pos);
          testof = testof.trim().substring(pos + 1);
          if (testolav.equals("")) {
            if (testoins != null && !testoins.equals("")) {
              testodas = QBEPartExtractor(campo, aliasIn + i, testoins, tipo,isExternalJoinedObjectField).toString();
              if (!testodas.equals("")) {
                if (!testolav.equals(""))
                  testolav = condnext2 + " ( " + testodas + " ) ";
                else
                  testolav = " ( " + testodas + " ) ";
              }
            }
          } else {
            if (!testodas.equals("")) {
              if (!testoins.equals(""))
                testodas = QBEPartExtractor(campo, aliasIn + i, testoins, tipo, isExternalJoinedObjectField).toString();
              if (!testodas.equals(""))
                testolav += condnext2 + " ( " + testodas + " ) ";
            }
          }
          condnext2 = condnext;
          pos1 = -1;
          pos2 = -1;
          pos = -1;
          testcount = testof.length();
        } else {
          if (!testof.equals("")) {
            testodas = QBEPartExtractor(campo, aliasIn + i, testof, tipo,isExternalJoinedObjectField).toString();
            if (!testodas.equals("")) {
              if (testolav.equals(""))
                testolav = " (" + testodas + " ) ";
              else
                testolav += condnext2 + " (" + testodas + " ) ";
            }
          }

          break;
        }
      }
    }
    if (!testolav.equals(""))
      testoFinale = testolav;
    else {
      testodas = QBEPartExtractor(campo, aliasIn, testoFinale, tipo,isExternalJoinedObjectField).toString();
      if (!testodas.equals("")) {
        testoFinale = "(" + testodas + ")";
      } else {
        testoFinale = "";
      }
    }
    return testoFinale;
  }

  private String parseString(String testof) {
    int testcount = testof.length();
    String testofinale = "";
    String value;
    while (testcount > 0) {
      int pos3 = testof.indexOf("\"");
      if (pos3 >= 0) {
        testofinale += testof.substring(0, pos3).replaceAll(" ", "|");
        value = testof.substring(pos3 + 1);
        pos3 = value.indexOf("\"");
        if (pos3 >= 0) {
          testofinale += "=" + value.substring(0, pos3);
          testof = value.substring(pos3 + 1);
        } else {
          testofinale += value;
          testof = "";
        }

      } else {
        testofinale += testof.replaceAll(" ", "|");
        testof = "";
      }
      testcount = testof.length();
    }
    return testofinale;
  }

  private StringBuffer QBEPartExtractor(String property, String alias, String text, String type,boolean isExternalJoinedObjectField) {
    String value;
    String left = "";
    StringBuffer finalstring = new StringBuffer();
    value = text;
    type = type.toUpperCase();
    boolean invalidValue = false;
    Date dateValue = null;
    Date dateValue2 = null;
    Date valore1 = null;
    Date valore2 = null;
    Number valoreInt = 0;
    Number valoreInt2 = 0;

    boolean thereisTime = false;

    String field=property;
    if (convertToUpper && (TYPE_CHAR.equals(type) || TYPE_CLOB.equals(type)&& CLOB_SUPPORTS_UPPER)) { // upper non supportato su molti db
      field = "upper(" + property + ")";
    }


    // hack to convert LM to from:to
    if (type.equals(TYPE_DATE)) {
      text = DateUtilities.qbeIntervalFromString(text);
    }

    // operatori di 2 carattere
    if (text.length() >= 2) {
      value = text.substring(2);// Mid(testo, 3) ;
      left = text.substring(0, 2); //  Left(testo, 2)
    }

    // "()"
    if ((left.equals("[]") || left.equals("()"))) {

      if (type.equals(TYPE_CHAR) && !isExternalJoinedObjectField ){
        finalstring.append(property).append(" is null or ").append(property).append("=:").append(alias).append("isempty");
        addParameter(alias + "isempty", "");
      } else if (isExternalJoinedObjectField){  // in case of joined entity don't use upper() and use the external object itself
        finalstring.append(property.substring(0,property.indexOf('.'))).append("=").append("null");
      } else {
        finalstring.append(property).append("=").append("null");
      }

      // "//"
    } else if (left.equals("//")) {

      if (type.equals(TYPE_CHAR)) {
        finalstring.append(property).append("=:").append(alias).append("isempty");
        addParameter(alias + "isempty", "");
      } else if (isExternalJoinedObjectField){  // in case of joined entity don't use upper() and use the external object itself assignment.resource.name =null -> assignment=null
        finalstring.append(property.substring(0,property.indexOf('.'))).append("=").append("null");
      } else {
        finalstring.append("(").append(field).append(" =").append("null)");
      }

      // "<>", "!="
    } else if (left.equals("<>") || left.equals("!=")) {

      if (type.equals(TYPE_CHAR)) {
        finalstring.append(field).append("!=:").append(alias);
        addParameter(alias, value);

      } else if (type.equals(TYPE_DATE)) {
        finalstring.append(" not(").append(QBEPartExtractor(property, alias, value, type,isExternalJoinedObjectField).toString()).append(")");

      } else if (type.equals(TYPE_CLOB)) {
        setImproperUseOfQBEErrorCode("Cannot use " + left + " on text/blob fields");

      } else {
        try {
          valoreInt = getNumberValue(text, valoreInt, type);
        } catch (ParseException e) {
          invalidValue = true;
        }
        if (!invalidValue) {
          finalstring.append(field).append("!=:").append(alias);
          addParameter(alias, valoreInt);
        }
      }

      // ">=" ,"<="
    } else if (left.equals(">=") || left.equals("<=")) {

      if (type.equals(TYPE_CHAR)) {
        finalstring.append(field).append(left).append(":").append(alias);
        addParameter(alias, value);

      } else if (type.equals(TYPE_CLOB)) {
        setImproperUseOfQBEErrorCode("Cannot use " + left + " on text/blob fields");

      } else if (type.equals(TYPE_DATE)) {
        value = value.replaceAll("-", "/");

        CompanyCalendar c = new CompanyCalendar();
        try {
          c.setTime(DateUtilities.dateFromString(value));
          dateValue = c.getTime();
          c.setAndGetTimeToDayStart();
          valore1 = c.getTime();
        } catch (ParseException e) {
          invalidValue = true;
        }

        if (left.equals("<=")) {
          if (!invalidValue) {
            if (valore1.compareTo(dateValue) == 0) { // da vedere il caso con ora in format
              c.setAndGetTimeToDayEnd();
              valore1 = c.getTime();
            } else {
              valore1 = dateValue;
            }
          }
        } else if (left.equals(">=")) {
          if (!invalidValue) {
            if (valore1.compareTo(dateValue) == 0) { // da vedere il caso con ora in format
              c.setAndGetTimeToDayStart();
              valore1 = c.getTime();
            } else {
              valore1 = dateValue;
            }
          }
        }
        if (!invalidValue) {
          finalstring.append(field).append(left).append(":").append(alias);
          addParameter(alias, valore1);
        }
      } else {
        try {
          valoreInt = getNumberValue(value, valoreInt, type);
        } catch (ParseException e) {
          invalidValue = true;
        }
        if (!invalidValue) {
          finalstring.append(field).append(left).append(":").append(alias);
          addParameter(alias, valoreInt);
        }
      }

      // one char operators
    } else {
      if (text.length() >= 1) {
        value = (text).substring(1);// Mid(testo, 3) ;
        left = (text).substring(0, 1); //  Left(testo, 2)
      }

      // "!"
      if (left.equals("!"))
        finalstring.append(" not(").append(QBEPartExtractor(property, alias, value, type,isExternalJoinedObjectField).toString()).append(")");

        // ">", "<", "="
      else if (left.equals("<") || left.equals(">") || left.equals("=")) {

        if (type.equals(TYPE_CHAR)) {
          finalstring.append(field).append(left).append(":").append(alias);
          addParameter(alias, value);

        } else if (type.equals(TYPE_CLOB)) {

          if (left.equals("=")) {
            finalstring.append(field).append(" like ").append(":").append(alias);
            addParameter(alias, value);
          } else
            setImproperUseOfQBEErrorCode("Cannot use " + left + " on text/blob fields");

        } else if (type.equals(TYPE_DATE)) {
          CompanyCalendar c = new CompanyCalendar();
          try {
            c.setTime(DateUtilities.dateFromString(value));
            dateValue = c.getTime();
            c.setAndGetTimeToDayStart();
            valore1 = c.getTime();
          } catch (ParseException e) {
            invalidValue = true;
          }

          if (left.equals("<")) {
            if (!invalidValue) {
              if (valore1.compareTo(dateValue) == 0 && !thereisTime) { // da vedere il caso con ora in format
                c.setAndGetTimeToDayStart();
                valore1 = c.getTime();
              } else {
                valore1 = dateValue;
              }
            }
          } else if (left.equals(">")) {

            if (!invalidValue) {
              if (valore1.compareTo(dateValue) == 0 && !thereisTime) { // da vedere il caso con ora in format
                c.setAndGetTimeToDayEnd();
                valore1 = c.getTime();
              } else {
                valore1 = dateValue;
              }
            }
          } else if (left.equals("=")) {
            if (!invalidValue) {
              if (valore1.compareTo(dateValue) == 0 && !thereisTime) { // da vedere il caso con ora in format
                c.setAndGetTimeToDayStart();
                valore1 = c.getTime();
                c.setAndGetTimeToDayEnd();
                valore2 = c.getTime();
              } else {
                valore1 = dateValue;
                valore2 = dateValue;
              }
            }
          }
          if (!invalidValue) {
            if (left.equals("=")) {
              finalstring.append(field).append(">=:").append(alias).append("A");
              finalstring.append(" and ").append(field).append("<=:").append(alias).append("B");
              addParameter(alias + "A", valore1);
              addParameter(alias + "B", valore2);
            } else {
              finalstring.append(field).append(left).append(":").append(alias);
              addParameter(alias, valore1);
            }
          }

        } else {
          try {
            valoreInt = getNumberValue(value, valoreInt, type);
          } catch (ParseException e) {
            invalidValue = true;
          }
          if (!invalidValue) {
            finalstring.append(field).append(left).append(":").append(alias);
            addParameter(alias, valoreInt);
          }
        }

        // "$"
      } else if (left.equals("$")) {

        if (type.equals(TYPE_CHAR) || type.equals(TYPE_CLOB)) {


          finalstring.append(field).append(" like :").append(alias);
          if (text.indexOf("%") >= 0)
            addParameter(alias, value);
          else
            addParameter(alias, value + "%");
        } else
          setImproperUseOfQBEErrorCode("Cannot use " + left + " on this field");

        //no initial operator
      } else {
        int pos = text.indexOf(":");
        if (pos > 0) {

          if (type.equals(TYPE_CHAR)) {

            finalstring.append(field).append(" between :").append(alias).append("A and :").append(alias).append("B");
            addParameter(alias + "A", text.substring(0, pos));
            addParameter(alias + "B", text.substring(pos + 1) + "zzzz");

          } else if (type.equals(TYPE_CLOB)) {

            setImproperUseOfQBEErrorCode("Cannot use " + left + " on text/blob fields");

          } else if (type.equals(TYPE_DATE)) {

            String valore1Str = text.substring(0, pos);
            String valore2Str = text.substring(pos + 1);

            CompanyCalendar c1 = new CompanyCalendar();
            CompanyCalendar c2 = new CompanyCalendar();
            try {
              c1.setTime(DateUtilities.dateFromString(valore1Str));
              dateValue = c1.getTime();
              c1.setAndGetTimeToDayStart();
              valore1 = c1.getTime();

              c2.setTime(DateUtilities.dateFromString(valore2Str));
              dateValue2 = c2.getTime();
              c2.setAndGetTimeToDayStart();
              valore2 = c2.getTime();
            } catch (ParseException e) {
              invalidValue = true;
            }
            if (!invalidValue) {

              if (valore1.compareTo(dateValue) == 0 && !thereisTime) { // da vedere il caso con ora in format
                c1.setAndGetTimeToDayStart();
                valore1 = c1.getTime();
              } else {
                valore1 = dateValue;
              }

              if (valore2.compareTo(dateValue2) == 0 && !thereisTime) { // da vedere il caso con ora in format
                c2.setAndGetTimeToDayEnd();
                valore2 = c2.getTime();
              } else {
                valore2 = dateValue2;
              }

              finalstring.append(field).append(" between :").append(alias).append("A and :").append(alias).append("B");
              addParameter(alias + "A", valore1);
              addParameter(alias + "B", valore2);
            }
          } else {
            try {
              valoreInt = getNumberValue(text.substring(0, pos), valoreInt, type);
              valoreInt2 = getNumberValue(text.substring(pos + 1), valoreInt2, type);
            } catch (ParseException e) {
              invalidValue = true;
            }
            if (!invalidValue) {
              finalstring.append(field).append(" between :").append(alias).append("A and :").append(alias).append("B");
              addParameter(alias + "A", valoreInt);
              addParameter(alias + "B", valoreInt2);
            }
          }

          //fallout case
        } else {

          if (type.equals(TYPE_CHAR) || type.equals(TYPE_CLOB)) {
            finalstring.append(field).append(" like :").append(alias);
            if (text.indexOf("%") >= 0)
              addParameter(alias, text);
            else {
              if (DefaultMatch.CONTAINS.equals(defaultMatch))
                addParameter(alias, "%" + text + "%");
              else if (DefaultMatch.STARTS_WITH.equals(defaultMatch))
                addParameter(alias, text + "%");
              else if (DefaultMatch.EQUALS.equals(defaultMatch))
                addParameter(alias, text);
            }
          } else if (type.equals(TYPE_DATE)) {
            CompanyCalendar c = new CompanyCalendar();
            try {
              c.setTime(DateUtilities.dateFromString(text));
              dateValue = c.getTime();
              c.setAndGetTimeToDayStart();
              valore1 = c.getTime();
            } catch (ParseException e) {
              invalidValue = true;
            }

            if (!invalidValue) {
              if (valore1.compareTo(dateValue) == 0 && !thereisTime) { // da vedere il caso con ora in format
                c.setAndGetTimeToDayStart();
                valore1 = c.getTime();
                c.setAndGetTimeToDayEnd();
                valore2 = c.getTime();
              } else {
                valore1 = dateValue;
                valore2 = dateValue;
              }
            }
            if (!invalidValue) {
              finalstring.append(field).append(">=:").append(alias).append("A");
              finalstring.append(" and ").append(field).append("<=:").append(alias).append("B");
              addParameter(alias + "A", valore1);
              addParameter(alias + "B", valore2);
            }
          } else {
            try {
              valoreInt = getNumberValue(text, valoreInt, type);
            } catch (ParseException e) {
              invalidValue = true;
            }
            if (!invalidValue) {
              finalstring.append(field).append("=:").append(alias);
              addParameter(alias, valoreInt);
            }
          }
        }
      }
    }
    return finalstring;
  }

  private Number getNumberValue(String testo, Number valoreInt, String type) throws ParseException {
    if (TYPE_INT.equals(type) || TYPE_INT.equals(type))
      valoreInt = NumberFormat.getInstance().parse(testo).intValue();
    else if (TYPE_MILLIS.equals(type))
      valoreInt = DateUtilities.millisFromString(testo,false);
    else if (TYPE_DOUBLE.equals(type))
      valoreInt = NumberFormat.getInstance().parse(testo).doubleValue();
    else if (TYPE_FLOAT.equals(type))
      valoreInt = NumberFormat.getInstance().parse(testo).floatValue();
    else if (TYPE_LONG.equals(type))
      valoreInt = NumberFormat.getInstance().parse(testo).longValue();
    return valoreInt;
  }

  private QueryHelper privateAddQueryClause(String condition) {
    return privateAddQueryClause(condition, "");
  }


  private QueryHelper privateAddQueryClause(String condition, String logic) {
    String sqlstr;
    String orderBySql = "";
    String groupBySql = "";
    String whereSql = "";
    if (logic.trim().equals(""))
      logic = TYPE_logicAND;
    int posOrderBy, posQroupBy, posWhere;
    String orderBy = " order by ";
    String groupBy = " group by ";
    String where = " where ";
    StringBuffer sqlQuery = new StringBuffer();
    sqlstr = hql.toLowerCase();
    if (!condition.trim().equals("")) {
      //check whether sqlstr has already "ORDER BY"
      posOrderBy = sqlstr.indexOf(orderBy);
      if (posOrderBy > 0) {
        orderBySql = hql.substring(posOrderBy);
        sqlstr = sqlstr.substring(0, posOrderBy);
        hql = hql.substring(0, posOrderBy);
      }
      //check whether sqlstr has already "GROUP BY"
      posQroupBy = sqlstr.indexOf(groupBy);
      if (posQroupBy > 0) {
        groupBySql = hql.substring(posQroupBy);
        hql = hql.substring(0, posQroupBy);
        sqlstr = sqlstr.substring(0, posQroupBy);
      }
      //check whether sqlstr has already "WHERE"
      posWhere = sqlstr.indexOf(where);
      if (posWhere > 0) {
        whereSql = hql.substring(posWhere) + logic;
        hql = hql.substring(0, posWhere);
        sqlstr = sqlstr.substring(0, posWhere);
      } else {
        whereSql = where.toLowerCase();
      }
      sqlQuery.append(hql);
      sqlQuery.append(whereSql);
      sqlQuery.append(" (").append(condition).append(") ");
      sqlQuery.append(groupBySql);
      sqlQuery.append(orderBySql);
    } else {
      sqlQuery.append(hql);
    }
    hql = sqlQuery.toString();
    //return sqlQuery;
    return this;
  }

  public void addQueryClause(String condition) {
    privateAddQueryClause(condition);
  }

  public void addOrQueryClause(String condition) {
    privateAddQueryClause(condition, TYPE_logicOR);
  }

  public void addToHqlString(String additionalHql) {
    hql = hql + additionalHql;
  }

  public void addJoinAlias(String joinAlias) {

    if (!joinAlias.startsWith(" "))
      joinAlias = " " + joinAlias;
    if (!joinAlias.endsWith(" "))
      joinAlias = joinAlias + " ";

    if (joinAlias.toLowerCase().indexOf("join") == -1)
      joinAlias = " join " + joinAlias;

    int afterFromIndex = hql.indexOf("from") + 4;
    String afterFrom = hql.substring(afterFromIndex);

    int spacesAfterFrom = StringUtilities.countConsecutiveOccurrences(" ", afterFrom);

    int afterClassIndex = hql.indexOf(" ", afterFromIndex + spacesAfterFrom);
    String afterClass = hql.substring(afterClassIndex);

    int spacesAfterClass = StringUtilities.countConsecutiveOccurrences(" ", afterClass);

    //is there "as" ?
    if (hql.substring(afterClassIndex + spacesAfterClass).trim().startsWith("as ")) {
      int spacesAfterAs = StringUtilities.countConsecutiveOccurrences(" ", hql.substring(afterClassIndex + spacesAfterClass + 2));
      //int afterClassAndAfterAs = sqlSelect.indexOf(" ",afterClassIndex+spacesAfterClass+2);
      afterClassIndex = afterClassIndex + 2 + spacesAfterAs;
    }


    int afterMainAlias = hql.indexOf(" ", afterClassIndex + spacesAfterClass);
    if (afterMainAlias == -1)
      afterMainAlias = hql.length();

    //int mainAlias = upToFrom.trim().indexOf(" ");

    hql = hql.substring(0, afterMainAlias) + joinAlias + hql.substring(afterMainAlias);
  }

  public Map getArgs() {
    return args;
  }

  public String doDebug(PersistenceContext pc) {
    StringBuffer str = new StringBuffer();
    // output the sqlSelect
    str.append("/*---------- sqlSelect ---------- */\n");
    str.append(getHqlString());
    str.append("\n\n");
    // get parameter values
    str.append("/*---------- parameters ---------- */\n");
    str.append(args.toString());
    str.append("\n\n");
    return str.toString();
  }

  public String doDebug() {
    return this.doDebug(null);
  }
  
  public void setDistinct() {
    String query = getHqlString();
    String distinct = "distinct";
    if (query.toLowerCase().indexOf(distinct) == -1) {
      int start = (query.startsWith("select") ? 6 : 0);
      query = "select distinct" + query.substring(start);
    }
    hql = query;
  }


  public static boolean containsQBEChars(String filter) {
    boolean containsQBEChars = false;
    for (int i = 0; i < filter.length(); i++) {
      String s = filter.substring(i, i + 1);
      if (qbeOperatorsChars.contains(s)) {
        containsQBEChars = true;
        break;
      }
    }
    return containsQBEChars;
  }

  public String getImproperUseOfQBEErrorCode() {
    return improperUseOfQBEErrorCode;
  }

  private void setImproperUseOfQBEErrorCode(String improperUseOfQBEErrorCode) {
    this.improperUseOfQBEErrorCode = improperUseOfQBEErrorCode;
    if (JSP.ex(improperUseOfQBEErrorCode))
      invalidQBE = true;
  }

}
