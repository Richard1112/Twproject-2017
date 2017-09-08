package org.jblooming.oql;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.NonUniqueResultException;
import org.hibernate.Query;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.waf.settings.ApplicationState;
import org.jblooming.persistence.exceptions.FindException;
import org.jblooming.persistence.exceptions.PersistenceException;
import org.jblooming.persistence.hibernate.HibernateFactory;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.tracer.Tracer;
import org.apache.log4j.Level;

import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 */
public class OqlQuery {

  public static Level originalHibLogLevel;
  private Query query;

  protected OqlQuery() {

  }

  public OqlQuery(String oql) {

    PersistenceContext pc;
    String aClass = guessTheClass(oql);
    if (aClass!=null)
      pc=PersistenceContext.get(aClass);
    else
      throw new PlatformRuntimeException("Missing class from:"+oql);

    this.setQuery(pc.session.createQuery(oql));
  }


  public OqlQuery(String oql, PersistenceContext pc) throws PersistenceException {
    try {
      this.setQuery(pc.session.createQuery(oql));

    } catch (HibernateException e) {
      throw new PersistenceException(doDebug(), e);
    }
  }
  
  
  public OqlQuery(String oql, PersistenceContext pc,Boolean isNative) throws PersistenceException {
	    try {
	      if(isNative){
	    	  this.setQuery(pc.session.createSQLQuery(oql));
	      }else{
	    	  this.setQuery(pc.session.createQuery(oql));
	      }
	      

	    } catch (HibernateException e) {
	      throw new PersistenceException(doDebug(), e);
	    }
	  }


  public List list(int maxResult) throws FindException {
    try {
      Query query = this.getQuery();
      query.setMaxResults(maxResult);
      return query.list();
    } catch (HibernateException e) {
      throw new FindException(doDebug(), e);
    }
  }

  public List list() throws FindException {
    try {
      return getQuery().list();
    } catch (HibernateException e) {
      throw new FindException(doDebug(), e);
    }
  }

  public Object uniqueResult() throws FindException {
    try {
      Object o = getQuery().uniqueResult();
      if (o == null)
        throw new FindException("object not found");
      return o;
    } catch (NonUniqueResultException e) {
      throw new FindException(doDebug(), e);
    } catch (HibernateException e) {
      throw new PlatformRuntimeException(doDebug(), e);
    }
  }

  public Object uniqueResultNullIfEmpty() {
    Object o = null;
    try {
      o = getQuery().uniqueResult();

    } catch (NonUniqueResultException e) {
    } catch (HibernateException e) {
      throw new PlatformRuntimeException(doDebug(), e);
    }
    return o;
  }


  public Query getQuery() {
    return query;
  }

  public void setQuery(Query query) {
    this.query = query;
  }

  public static List getListFromCriteria(Criteria criteria) throws FindException {
    try {
      return criteria.list();
    } catch (HibernateException e) {
      throw new FindException(e);
    }
  }

  public void setParameter(String paramName, Object value) throws PersistenceException {
    try {
      getQuery().setParameter(paramName, value);
    } catch (HibernateException e) {
      throw new PersistenceException(doDebug() + " setting: " + paramName + "=" + value, e);
    }
  }

  public String doDebug() {
    return doDebug(null);
  }

  public String doDebug(Object[] values) {

    if (getQuery() != null) {
      String s = getQuery().getQueryString() + "\n";
      if (values != null) {
        for (int i = 0; i < values.length; i++) {
          Object value = values[i];
          s = s + " " + value.toString();
        }
      }
      return s + (values != null ? values.toString() : "");
    } else
      return "doDebug: Query is null";
  }

  public static void debugHQLBegin() {
    originalHibLogLevel = Tracer.hibernateLogger.getLevel();
    Tracer.hibernateLogger.setLevel(Level.DEBUG);
  }

  public static void debugHQLEnd() {
    Tracer.hibernateLogger.setLevel(originalHibLogLevel);
  }

  private String guessTheClass(String oql) {

    String ret = null;
    Matcher matcher = Pattern.compile("((?i)(^from +)|( from +)|(^update +))([^ ]+)").matcher(oql);
    if (matcher.find()) {
      ret = matcher.group(5).trim();
    }
    return ret;
  }
 }
